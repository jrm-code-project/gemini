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

(defparameter +jsonrpc-bookkeeper-interval+ 15
  "Interval in seconds for the JSONRPC bookkeeper to run.")

(defparameter +jsonrpc-ping-timeout+ 60
  "Time in seconds after which we ping the server if we haven't heard from it.")

(defparameter +jsonrpc-request-timeout+ 120
  "Time in seconds after which we consider a request to have timed out.")

(defparameter +jsonrpc-nonresponse-timeout+ 200
  "Time in seconds after which we assume the server is dead if we haven't heard from it.")

(defun create-jsonrpc-client (name command args unsolicited-message-handler)
  (let* ((process-info
           (uiop:launch-program (append command args)
                                :error-output :stream
                                :input        :stream
                                :output       :stream))
         (eof-value (cons nil nil))
         (counts (make-hash-table :test 'eql))
         (client (make-instance 'jsonrpc-client :process-info process-info)))

    (flet ((json-send (json)
             ;; (format *trace-output* "Send: ~s~%" (cl-json:encode-json-to-string json))
             ;; (finish-output *trace-output*)
             (cl-json:encode-json json (uiop:process-info-input process-info))
             (terpri (uiop:process-info-input process-info))
             (finish-output (uiop:process-info-input process-info)))

           (line-receiver (stream-name stream receiver)
             (lambda ()
               (unwind-protect
                    (let iter ((line nil))
                      ;; (format *trace-output* "~&[~a ~a ~d] ~a~%" stream-name name
                      ;;         (incf (gethash stream-name counts 0)) line)
                      ;; (finish-output *trace-output*)
                      (when line
                        (funcall receiver line))
                      (unless (eq line eof-value)
                        (iter (read-line stream nil eof-value))))
                 (format *trace-output* "~&Line receiver for ~a ~a exiting...~%" stream-name name)
                 (finish-output *trace-output*)))))

      (flet ((error-receiver (receiver)
               (line-receiver 'error (uiop:process-info-error-output process-info) receiver))

             (json-receiver (receiver)
               (line-receiver
                'output
                (uiop:process-info-output process-info)
                (lambda (line)
                  (handler-case
                      (if (eq line eof-value)
                          (funcall receiver line)
                          (let ((message (jsonx:with-decoder-jrm-semantics (cl-json:decode-json-from-string line))))
                            (setf (latest-server-output client) (get-universal-time))
                            (cond ((not (equal (get-jsonrpc message) "2.0"))
                                   (format *trace-output* "~&Unexpected jsonrpc version: ~s~%" message)
                                   (finish-output *trace-output*)
                                   ;; discard messages with wrong jsonrpc version
                                   nil)

                                  ;; Discard responses to our pings.
                                  ((and (equal (get-id message) (get-id (ping-message client)))
                                        (eql (get-result message) jsonx:+json-empty-object+))
                                   nil)

                                  ;; Respond to ping request.
                                  ((equal (get-method message) "ping")
                                   (json-send (object :jsonrpc "2.0"
                                                      :id (get-id message)
                                                      :result (object))))

                                  ;; Pass other messages to receiver. 
                                  (t (funcall receiver message)))))
                    ;; Handle JSON parse errors by turning them into notifications.
                    (json:json-syntax-error (e)
                      (format *trace-output* "~&JSON Parse Error: ~a~%" e)
                      (finish-output *trace-output*)
                      (funcall receiver
                               (object :jsonrpc "2.0"
                                       :method "notification/message"
                                       :params (object :level "info" :data line)))))))))

             (let ((input-thread
                     ;; Start a thread to send RPCs to the server.
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (let iter ((json nil))
                          (if (eq json :exit)
                              (progn
                                (format *trace-output* "~&Exiting JSONRPC send thread for ~a...~%" name)
                                (finish-output *trace-output*))
                              (progn
                                (when json (json-send json))
                                (iter (chanl:recv (outgoing-channel client)))))))
                      :name (format nil "~a JSONRPC Output" name)))

                   (output-thread
                     ;; Start a thread to sink the standard output from the server.
                     (bordeaux-threads:make-thread
                      (json-receiver
                       (lambda (message)
                         (cond ((eql message eof-value)
                                (maphash (lambda (id channel-info)
                                           (let ((channel (car channel-info)))
                                             (chanl:send channel
                                                         (object :jsonrpc "2.0"
                                                                 :id id
                                                                 :error (object :code -32800
                                                                                :message "Connection closed")))))
                                         (incoming-channels client)))

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

                   (error-thread
                     ;; Start a thread to sink the error output from the server.
                     (bordeaux-threads:make-thread
                      (error-receiver
                       (lambda (line)
                         (unless (eql line eof-value)
                           (format *trace-output* "~&[~a Error] ~a~%" name line)
                           (finish-output *trace-output*))))
                      :name (format nil "~a JSONRPC Error Output" name))))

               (flet ((shutdown-process ()
                        ;; Terminate the process and all associated threads.
                        (chanl:send (outgoing-channel client) :exit)
                        (bordeaux-threads:join-thread input-thread)
                        (uiop:close-streams process-info)
                        (bordeaux-threads:interrupt-thread error-thread #'abort)
                        (bordeaux-threads:interrupt-thread output-thread #'abort)
                        (uiop:terminate-process (process-info client) :urgent t)
                        (uiop:wait-process (process-info client))
                        (format *trace-output* "~&Process for ~a has been terminated.~%" name)
                        (finish-output *trace-output*)
                        (maphash (lambda (id channel-info)
                                   (let ((channel (car channel-info)))
                                     (chanl:send channel
                                                 (object :jsonrpc "2.0"
                                                         :id id
                                                         :error (object :code -32801
                                                                        :message "Connection closed")))))
                                 (incoming-channels client))))

                 (let ((bookkeeper-thread
                         (bordeaux-threads:make-thread
                          (lambda ()
                            (let iter ()
                              (sleep +jsonrpc-bookkeeper-interval+)
                              ;; (format *trace-output* "~&JSONRPC Bookkeeper running for ~a...~%" name)
                              ;; (finish-output *trace-output*)
                              (cond ((not (uiop:process-alive-p (process-info client)))
                                     (format *trace-output* "~&Process for ~a has exited.~%" name)
                                     (finish-output *trace-output*)
                                     (chanl:send (outgoing-channel client) :exit)

                                     ;; Simply exit the bookkeeper thread.
                                     ;; The stream sinks should exit because of eof.
                                     )

                                    ;; If we haven't heard from the server in a long while, assume it died.
                                    ((> (- (get-universal-time) (latest-server-output client))
                                        +jsonrpc-nonresponse-timeout+)
                                     ;; Assume the server is dead.
                                     (format *trace-output* "~&No response from ~a for ~a seconds, assuming it hanged.~%"
                                             name
                                             +jsonrpc-nonresponse-timeout+)
                                     (finish-output *trace-output*)
                                     (shutdown-process))

                                    (t
                                     ;; If we haven't heard from the server in a while, ping it.
                                     (when (> (- (get-universal-time) (latest-server-output client))
                                              +jsonrpc-ping-timeout+)
                                       (jsonrpc-ping client))

                                     ;; Cancel requests that have been pending for too long.
                                     (with-jsonrpc-client-lock (client)
                                       (maphash (lambda (id channel-info)
                                                  (let ((channel (car channel-info))
                                                        (start-time (cdr channel-info)))
                                                    (when (> (- (get-universal-time) start-time)
                                                             +jsonrpc-request-timeout+)
                                                      (format *trace-output* "~&Request ~a to ~a has timed out, cancelling...~%"
                                                              id name)
                                                      (finish-output *trace-output*)
                                                      (chanl:send (outgoing-channel client)
                                                                  (object :jsonrpc "2.0"
                                                                          :method "notifications/cancelled"
                                                                          :params (object :request-id id
                                                                                          :reason "timeout")))
                                                      (chanl:send channel
                                                                  (object :jsonrpc "2.0"
                                                                          :id id
                                                                          :error (object :code -32800
                                                                                         :message "Request cancelled due to timeout"))))))
                                                (incoming-channels client)))

                                     (iter)))))
                          :name (format nil "~a Bookkeeper" name))))
                   client)))))))

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
             (err (get-error response))
             (result (get-result response))
             (cursor (get-next-cursor result)))
        (cond (err
               (error "JSON-RPC Error: ~a" (get-message err)))
              ((null cursor) result)
              (t (depaginate result (next-page (random-id) cursor))))))))
