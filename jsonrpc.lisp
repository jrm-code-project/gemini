;;; -*- Lisp -*-

(in-package "GEMINI")

(defun call-with-channel (thunk)
  "Call THUNK with a new channel."
  (funcall thunk (make-instance 'chanl:channel)))

(defmacro with-channel ((channel) &body body)
  "Execute BODY with a channel bound to CHANNEL."
  `(CALL-WITH-CHANNEL (LAMBDA (,channel) ,@body)))

(defun random-id ()
  (map 'string
       (lambda (n)
         (code-char (+ n (cond ((< n 10) 48)
                               ((< n 36) 55)
                               (t 61)))))
       (loop repeat 16
             collect (random 62))))

(defclass jsonrpc-client ()
  ((default-outgoing-channel :initform (make-instance 'chanl:channel)  :reader outgoing-channel)
   (incoming-channels        :initform (make-hash-table :test #'equal) :reader incoming-channels)
   (mutex                    :initform (bordeaux-threads:make-lock)    :reader mutex)
   (latest-server-output     :initform (get-universal-time)            :accessor latest-server-output)
   (ping-message             :initform (object :jsonrpc "2.0"
                                               :id (random-id)
                                               :method "ping")         :reader ping-message)
   (process-info             :initarg :process-info :reader process-info)
   (request-threads          :initform (make-hash-table :test #'equal) :reader request-threads)))

(defun call-with-jsonrpc-client-lock (jsonrpc-client thunk)
  "Call THUNK while holding the lock of the JSONRPC-CLIENT."
  (bordeaux-threads:with-lock-held ((mutex jsonrpc-client))
    (funcall thunk)))

(defmacro with-jsonrpc-client-lock ((jsonrpc-client) &body body)
  "Execute BODY while holding the lock of the JSONRPC-CLIENT."
  `(CALL-WITH-JSONRPC-CLIENT-LOCK ,jsonrpc-client (LAMBDA () ,@body)))

(defun call-with-incoming-channel (jsonrpc-client id thunk)
  "Call THUNK with an incoming channel for the given JSONRPC-CLIENT and ID."
  (with-channel (incoming-channel)
    (unwind-protect
         (progn
           (with-jsonrpc-client-lock (jsonrpc-client)
             (setf (gethash id (incoming-channels jsonrpc-client))
                   (cons incoming-channel (get-universal-time))))
           (funcall thunk incoming-channel))
      (with-jsonrpc-client-lock (jsonrpc-client)
        (remhash id (incoming-channels jsonrpc-client))))))

(defmacro with-jsonrpc-incoming-channel ((channel jsonrpc-client id) &body body)
  "Execute BODY with an incoming channel for the JSONRPC-CLIENT and ID."
  `(CALL-WITH-INCOMING-CHANNEL ,jsonrpc-client ,id (LAMBDA (,channel) ,@body)))

(defun start-request-thread (jsonrpc-client id thunk)
  (letrec ((new-thread (bordeaux-threads:make-thread
                        (lambda ()
                          (block nil
                            (unwind-protect
                                 (restart-case
                                     (progn (with-jsonrpc-client-lock (jsonrpc-client)
                                              (setf (gethash id (request-threads jsonrpc-client)) new-thread))
                                            (funcall thunk))
                                   (abort ()
                                     (format *trace-output* "~&Aborting request thread for ID ~a~%" id)
                                     (finish-output *trace-output*)
                                     (return nil)))
                              (with-jsonrpc-client-lock (jsonrpc-client)
                                (remhash id (request-threads jsonrpc-client))))))
                        :name (format nil "~a JSONRPC Request Thread" id))))
    new-thread))

(defun create-jsonrpc-client (name command args unsolicited-message-handler)
  (let* ((process-info
           (uiop:launch-program (append command args)
                                :error-output :stream
                                :input        :stream
                                :output       :stream))
         (client (make-instance 'jsonrpc-client :process-info process-info)))

    (labels ((json-send (json)
               ;; (format *trace-output* "Send: ~s~%" (cl-json:encode-json-to-string json))
               ;; (finish-output *trace-output*)
               (cl-json:encode-json json (uiop:process-info-input process-info))
               (terpri (uiop:process-info-input process-info))
               (finish-output (uiop:process-info-input process-info)))

             (line-receive (stream receiver)
               (let ((eof-value (cons nil nil)))
                 (lambda ()
                   (let iter ((line nil))
                     (unless (eq line eof-value)
                       (when line
                         (funcall receiver line))
                       (iter (read-line stream nil eof-value)))))))

             (error-receive (receiver)
               (line-receive (uiop:process-info-error-output process-info) receiver))

             (json-receive (receiver)
               (line-receive
                (uiop:process-info-output process-info)
                (lambda (line)
                  (handler-case
                      (let ((message (jsonx:with-decoder-jrm-semantics (cl-json:decode-json-from-string line))))
                        (setf (latest-server-output client) (get-universal-time))
                        (let ((id (get-id message))
                              (result (get-result message)))
                          ;; Filter out ping responses, send rest to receiver.
                          (unless (and (equal id (get-id (ping-message client)))
                                       (eql result jsonx:+json-empty-object+))
                            (funcall receiver message))))
                    (json:json-syntax-error (e)
                      (format *trace-output* "~&JSON Parse Error: ~a~%" e)
                      (finish-output *trace-output*)
                      (funcall receiver
                               (object :jsonrpc "2.0"
                                       :method "notification/message"
                                       :params (object :level "info" :data line)))))))))

        ;; Start a thread to send RPCs to the server.
        (bordeaux-threads:make-thread
         (lambda ()
           (loop
             (let ((json (chanl:recv (outgoing-channel client))))
               (when json
                 (json-send json)))))
         :name (format nil "~a JSONRPC Output" name))

        ;; Start a thread to sink the error output from the server.
        (bordeaux-threads:make-thread
           (error-receive
            (lambda (line)
              (format *trace-output* "~&[~a Error] ~a~%" name line)
              (finish-output *trace-output*)))
         :name (format nil "~a JSONRPC Error Output" name))

        ;; Start a thread to sink the standard output from the server.
        (bordeaux-threads:make-thread
         (json-receive
          (lambda (message)
            (cond ((not (equal (get-jsonrpc message) "2.0"))
                   (format *trace-output* "~&Unexpected jsonrpc version: ~s~%" message)
                   ;; discard message
                   nil)

                  ((equal (get-method message) "notifications/cancelled")
                   (let* ((params (get-params message))
                          ;; (reason (get-reason params))
                          (request-id (get-request-id params))
                          (thread (with-jsonrpc-client-lock (client)
                                    (gethash request-id (request-threads client)))))
                     (when (bordeaux-threads:thread-alive-p thread)
                       (bordeaux-threads:interrupt-thread thread #'abort))))

                  ((get-id message)
                   (let ((incoming-channel
                           (with-jsonrpc-client-lock (client)
                             (gethash (get-id message) (incoming-channels client)))))
                     (if incoming-channel
                         (chanl:send (car incoming-channel) message)
                         (start-request-thread client (get-id message)
                                               (lambda ()
                                                 (funcall unsolicited-message-handler message))))))

                  (t (start-request-thread client (get-id message)
                                           (lambda ()
                                             (funcall unsolicited-message-handler message)))))))
         :name (format nil "~a JSONRPC Output" name)))

    client))

(defun %jsonrpc-send (jsonrpc-client json)
  "Send a JSON message to the server."
  (chanl:send (outgoing-channel jsonrpc-client) json))

(defun jsonrpc-ping (jsonrpc-client)
  "Ping the other side of the JSONRPC connection."
  ;; (format *trace-output* "~&Pinging JSON-RPC server...~%")
  ;; (finish-output *trace-output*)
  (%jsonrpc-send jsonrpc-client (ping-message jsonrpc-client)))

(defun %call-with-jsonrpc (jsonrpc-client json id thunk)
  "Send a JSON message to the server with given ID."
  (with-jsonrpc-incoming-channel (incoming-channel jsonrpc-client id)
    (%jsonrpc-send jsonrpc-client json)
    (funcall thunk incoming-channel)))

(defmacro with-jsonrpc ((incoming-channel jsonrpc-client json id) &body body)
  "Execute BODY with a JSON message sent to the JSONRPC-CLIENT."
  `(%CALL-WITH-JSONRPC ,jsonrpc-client ,json ,id (LAMBDA (,incoming-channel) ,@body)))

(defun depaginate (first-page remaining-pages)
  (map 'list (lambda (key)
               (cons key
                     (let ((first-value (funcall (object-ref-function key) first-page))
                           (more-values (funcall (object-ref-function key) remaining-pages)))
                       (cond ((null more-values) first-value)
                             ((and (or (null first-value) (consp first-value))
                                   (or (null more-values) (consp more-values)))
                              (append first-value more-values))
                             (t first-value)))))
       (remove-duplicates
        (append (keys first-page)
                (keys remaining-pages))
        :test #'equal)))

(defun jsonrpc (jsonrpc-client method params)
  (let next-page ((id (random-id))
                  (cursor nil))
    (with-jsonrpc (incoming-channel
                   jsonrpc-client
                   (object :id id
                           :jsonrpc "2.0"
                           :method method
                           :params (if (null cursor)
                                       (if (null params)
                                           (object)
                                           params)
                                       (cond ((or (null params)
                                                  (eql params jsonx:+json-empty-object+))
                                              (object :cursor cursor))
                                             ((alist? params)
                                              (acons :cursor cursor params))
                                             ((hash-table-p params)
                                              (setf (gethash :cursor params) cursor)
                                              params))))
                   id)

      (let* ((response (chanl:recv incoming-channel))
             (result (get-result response))
             (cursor (get-next-cursor result)))
        (if (null cursor)
            result
            (depaginate result (next-page (random-id) cursor)))))))
