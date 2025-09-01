;;; -*- Lisp -*-

(in-package "GEMINI")

(defun call-with-channel (thunk)
  "Call THUNK with a new channel."
  (funcall thunk (make-instance 'chanl:channel)))

(defmacro with-channel ((channel) &body body)
  "Execute BODY with a channel bound to CHANNEL."
  `(CALL-WITH-CHANNEL (LAMBDA (,channel) ,@body)))

(defclass jsonrpc-client ()
  ((default-outgoing-channel :initform (make-instance 'chanl:channel)  :reader outgoing-channel)
   (incoming-channels        :initform (make-hash-table :test #'equal) :reader incoming-channels)
   (mutex                    :initform (bordeaux-threads:make-lock)    :reader mutex)
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
             (setf (gethash id (incoming-channels jsonrpc-client)) incoming-channel))
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
  (let ((process-info
          (uiop:launch-program (append command args)
                               :error-output :stream
                               :input        :stream
                               :output       :stream)))
    (letrec ((client (make-instance 'jsonrpc-client :process-info process-info)))

      ;; Start a thread to send RPCs to the server.
      (bordeaux-threads:make-thread
       (lambda ()
         (loop
           (let ((json (chanl:recv (outgoing-channel client))))
             (when json
               ;; (format *trace-output* "Send: ~s~%" (cl-json:encode-json-to-string json))
               ;; (finish-output *trace-output*)
               (format (uiop:process-info-input process-info) "~a~%" (cl-json:encode-json-to-string json))
               (finish-output (uiop:process-info-input process-info))))))
       :name (format nil "~a JSONRPC Output" name))

      ;; Start a thread to sink the error output from the server.
      (bordeaux-threads:make-thread
       (lambda ()
         (do ((line (read-line (uiop:process-info-error-output process-info) nil nil)
                    (read-line (uiop:process-info-error-output process-info) nil nil)))
             ((null line))
           (format *trace-output* "~&~a~%" line)
           (finish-output *trace-output*)))
       :name (format nil "~a JSONRPC Error Output" name))

      ;; Start a thread to sink the standard output from the server.
      (bordeaux-threads:make-thread
       (lambda ()
         (do ((line (read-line (uiop:process-info-output process-info) nil nil)
                    (read-line (uiop:process-info-output process-info) nil nil)))
             ((null line))
           (let ((message (jsonx:with-decoder-jrm-semantics  (cl-json:decode-json-from-string line))))
             (cond ((equal (get-jsonrpc message) "2.0")
                    (cond ((equal (get-method message) "notifications/cancelled")
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
                                 (chanl:send incoming-channel message)
                                 (start-request-thread client (get-id message)
                                                       (lambda ()
                                                         (funcall unsolicited-message-handler message))))))

                          (t (start-request-thread client (get-id message)
                                                   (lambda ()
                                                     (funcall unsolicited-message-handler message))))))
                   (t (format *trace-output* "~&Unexpected jsonrpc version: ~s~%" message))))))
       :name (format nil "~a JSONRPC Output" name))

      client)))

(defun %call-with-jsonrpc (jsonrpc-client json id thunk)
  "Send a JSON message to the server with given ID."
  (with-jsonrpc-incoming-channel (incoming-channel jsonrpc-client id)
    (chanl:send (outgoing-channel jsonrpc-client) json)
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

(defun random-id ()
  (map 'string
       (lambda (n)
         (code-char (+ n (cond ((< n 10) 48)
                               ((< n 36) 55)
                               (t 61)))))
       (loop repeat 16
             collect (random 62))))

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
