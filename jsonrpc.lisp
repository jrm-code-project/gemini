;;; -*- Lisp -*-

(in-package "GEMINI")

(defun call-with-channel (thunk)
  "Call THUNK with a new channel."
  (check-type thunk function)
  (funcall thunk (make-instance 'chanl:channel)))

(defmacro with-channel ((channel) &body body)
  "Execute BODY with a channel bound to CHANNEL."
  `(CALL-WITH-CHANNEL (LAMBDA (,channel) ,@body)))

(defun random-id (&optional (length 16))
  (let ((chars "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))
    (map 'string
         (lambda (n) (schar chars n))
         (loop repeat length
               collect (random (length chars))))))

(defclass jsonrpc-client ()
  ((default-outgoing-channel :initform (make-instance 'chanl:channel)  :reader outgoing-channel)
   (incoming-channels        :initform (make-hash-table :test #'equal) :reader incoming-channels)
   (mutex                    :initform (bordeaux-threads:make-lock)    :reader mutex)
   (latest-server-output     :initform (get-universal-time)            :accessor latest-server-output)
   (process-info             :initarg :process-info :reader process-info)
   (request-threads          :initform (make-hash-table :test #'equal) :reader request-threads))
  (:documentation "Represents a JSON-RPC client, managing outgoing requests, incoming responses, and thread synchronization. It maintains channels for communication, a mutex for thread safety, and tracks server activity for connection management."))

(defun call-with-jsonrpc-client-lock (jsonrpc-client thunk)
  "Call THUNK while holding the lock of the JSONRPC-CLIENT."
  (check-type jsonrpc-client jsonrpc-client)
  (check-type thunk function)
  (bordeaux-threads:with-lock-held ((mutex jsonrpc-client))
    (funcall thunk)))

(defmacro with-jsonrpc-client-lock ((jsonrpc-client) &body body)
  "Execute BODY while holding the lock of the JSONRPC-CLIENT."
  `(CALL-WITH-JSONRPC-CLIENT-LOCK ,jsonrpc-client (LAMBDA () ,@body)))

(defun call-with-incoming-channel (jsonrpc-client id progress-token thunk)
  "Call THUNK with an incoming channel for the given JSONRPC-CLIENT and ID."
  (check-type jsonrpc-client jsonrpc-client)
  (check-type id (or string integer))
  (check-type thunk function)
  (with-channel (incoming-channel)
    (unwind-protect
         (progn
           (with-jsonrpc-client-lock (jsonrpc-client)
             (setf (gethash id (incoming-channels jsonrpc-client))
                   (list incoming-channel progress-token (get-universal-time))))
           (funcall thunk incoming-channel))
      (with-jsonrpc-client-lock (jsonrpc-client)
        (remhash id (incoming-channels jsonrpc-client))))))

(defmacro with-jsonrpc-incoming-channel ((channel jsonrpc-client id progress-token) &body body)
  "Execute BODY with an incoming channel for the JSONRPC-CLIENT and ID."
  `(CALL-WITH-INCOMING-CHANNEL ,jsonrpc-client ,id ,progress-token (LAMBDA (,channel) ,@body)))

(defun start-request-thread (jsonrpc-client id thunk)
  "Starts a new thread to execute `thunk` for the given `jsonrpc-client` and `id`. The thread is registered in the client's `request-threads` hash table and automatically deregistered upon completion or abortion. Provides an `abort` restart to gracefully terminate the thread."
  (check-type jsonrpc-client jsonrpc-client)
  (check-type thunk function)
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

(defparameter +jsonrpc-nonresponse-timeout+ 300
  "Time in seconds after which we assume the server is dead if we haven't heard from it.")

(defun jsonrpc-client-alive? (jsonrpc-client)
  (check-type jsonrpc-client jsonrpc-client)
  (< (get-universal-time)
     (+ (latest-server-output jsonrpc-client)
        +jsonrpc-nonresponse-timeout+)))

(defun disable-jsonrpc-timeouts ()
  "Disable ping and nonresponse timeouts."
  (setf +jsonrpc-ping-timeout+ most-positive-fixnum
        +jsonrpc-nonresponse-timeout+ most-positive-fixnum))

(defun make-ping-request ()
  (object :jsonrpc "2.0"
          :id (concatenate 'string "ping-" (random-id 8))
          :method "ping"))

(defun ping-id? (id)
  "Check if ID is a ping request ID."
  (and (stringp id)
       (str:starts-with? "ping-" id)))

(defun create-jsonrpc-client (name command args unsolicited-message-handler)
  "Creates and initializes a multi-threaded JSON-RPC client connected to an external server process. It launches the `command` with `args`, sets up dedicated threads for sending requests, receiving responses/notifications, and sinking error output. A bookkeeper thread monitors server liveness, sends pings, and manages request timeouts. `unsolicited-message-handler` is called for messages not matching active requests. Returns the initialized `jsonrpc-client` object."
  (check-type name string)
  (check-type command list)
  (check-type args list)
  (check-type unsolicited-message-handler function)
  (let* ((process-info
           (uiop:launch-program (append command args)
                                :error-output :stream
                                :input        :stream
                                :output       :stream))
         (eof-value (cons nil nil))
         ;; (counts (make-hash-table :test 'eql))
         (client (make-instance 'jsonrpc-client :process-info process-info)))

    (flet ((json-send (json)
             ;; (format *trace-output* "~&[INPUT ~a] ~a~%" name (cl-json:encode-json-to-string json))
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
                                  ((and (ping-id? (get-id message))
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
                   (unwind-protect
                        (let iter ((json nil))
                          (unless (eq json eof-value)
                            (when json (json-send json))
                            (iter (chanl:recv (outgoing-channel client)))))
                     (format *trace-output* "~&Exiting JSONRPC send thread for ~a...~%" name)
                     (finish-output *trace-output*)))
                 :name (format nil "~a JSONRPC Output" name)))

              (output-thread
                ;; Start a thread to sink the standard output from the server.
                (bordeaux-threads:make-thread
                 (json-receiver
                  (lambda (message)
                    (cond ((eql message eof-value)
                           (map nil (lambda (entry)
                                      (let* ((id (car entry))
                                             (channel-info (cdr entry))
                                             (channel (first channel-info)))
                                        (chanl:send channel
                                                    (object :jsonrpc "2.0"
                                                            :id id
                                                            :error (object :code -32800
                                                                           :message "Connection closed")))))
                                (alexandria:hash-table-alist (incoming-channels client))))

                          ((equal (get-method message) "notifications/cancelled")
                           (let* ((params (get-params message))
                                  ;; (reason (get-reason params))
                                  (request-id (get-request-id params))
                                  (thread (with-jsonrpc-client-lock (client)
                                            (gethash request-id (request-threads client)))))
                             (when (and thread (bordeaux-threads:thread-alive-p thread))
                               (bordeaux-threads:interrupt-thread thread #'abort))))

                          ((equal (get-method message) "notifications/progress")
                           (let* ((params (get-params message))
                                  (token  (get-progress-token params)))
                             (when token
                               (with-jsonrpc-client-lock (client)
                                 (map nil (lambda (entry)
                                            (let* ((channel-info (cdr entry))
                                                   (progress-token (second channel-info)))
                                              (when (equal progress-token token)
                                                (setf (third channel-info) (get-universal-time)))))
                                      (hash-table-alist (incoming-channels client))))))
                           (start-request-thread client (get-id message)
                                                 (lambda ()
                                                   (funcall unsolicited-message-handler message))))

                          ((and (get-id message)
                                (or (get-result message)
                                    (get-error message)))
                           (let ((incoming-channel
                                   (with-jsonrpc-client-lock (client)
                                     (gethash (get-id message) (incoming-channels client)))))
                             (if incoming-channel
                                 (chanl:send (first incoming-channel) message)
                                 ;; If incoming channel is missing, log message and discard it.
                                 (progn
                                   (format *trace-output* "~&No incoming channel for message: ~s~%" message)
                                   (finish-output *trace-output*)))))

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
                      (format *trace-output* "~&[~a] ~a~%" name line)
                      (finish-output *trace-output*))))
                 :name (format nil "~a JSONRPC Error Output" name))))

          (bordeaux-threads:make-thread
           (lambda ()
             (let iter ()
               (sleep +jsonrpc-bookkeeper-interval+)
               ;; (format *trace-output* "~&JSONRPC Bookkeeper running for ~a...~%" name)
               ;; (finish-output *trace-output*)
               (cond ((not (uiop:process-alive-p (process-info client)))
                      (format *trace-output* "~&Process for ~a has exited.~%" name)
                      (finish-output *trace-output*)
                      (chanl:send (outgoing-channel client) eof-value)

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
                      ;; Terminate the process and all associated threads.
                      (chanl:send (outgoing-channel client) eof-value)
                      (bordeaux-threads:join-thread input-thread)
                      (uiop:close-streams process-info)
                      (bordeaux-threads:interrupt-thread error-thread #'abort)
                      (bordeaux-threads:interrupt-thread output-thread #'abort)
                      (uiop:terminate-process (process-info client) :urgent t)
                      (uiop:wait-process (process-info client))
                      (format *trace-output* "~&Process for ~a has been terminated.~%" name)
                      (finish-output *trace-output*)
                      (map nil (lambda (entry)
                                 (let* ((id (car entry))
                                        (channel-info (cdr entry))
                                        (channel (first channel-info)))
                                   (chanl:send channel
                                               (object :jsonrpc "2.0"
                                                       :id id
                                                       :error (object :code -32801
                                                                      :message "Connection closed")))))
                           (hash-table-alist (incoming-channels client))))

                     (t
                      ;; If we haven't heard from the server in a while, ping it.
                      (when (> (- (get-universal-time) (latest-server-output client))
                               +jsonrpc-ping-timeout+)
                        (jsonrpc-ping client))

                      ;; Cancel requests that have been pending for too long.
                      (with-jsonrpc-client-lock (client)
                        (map nil (lambda (entry)
                                   (let* ((id (car entry))
                                          (channel-info (cdr entry))
                                          (channel (first channel-info))
                                          (start-time (third channel-info)))
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
                             (hash-table-alist (incoming-channels client))))

                      (iter)))))
           :name (format nil "~a Bookkeeper" name)))
        client))))

(defun %jsonrpc-send (jsonrpc-client json)
  "Send a JSON message to the server."
  (check-type jsonrpc-client jsonrpc-client)
  (chanl:send (outgoing-channel jsonrpc-client) json))

(defun jsonrpc-ping (jsonrpc-client)
  "Ping the other side of the JSONRPC connection."
  (check-type jsonrpc-client jsonrpc-client)
  (%jsonrpc-send jsonrpc-client (make-ping-request)))

(defun %call-with-jsonrpc (jsonrpc-client json id progress-token thunk)
  "Send a JSON message to the server with given ID."
  (check-type jsonrpc-client jsonrpc-client)
  (check-type id (or string integer))
  (check-type thunk function)
  (with-jsonrpc-incoming-channel (incoming-channel jsonrpc-client id progress-token)
    (%jsonrpc-send jsonrpc-client json)
    (funcall thunk incoming-channel)))

(defmacro with-jsonrpc ((incoming-channel jsonrpc-client json id progress-token) &body body)
  "Sends a JSON message (`json`) with the given `id` to the `jsonrpc-client`. Executes `BODY` with `incoming-channel` bound to a dedicated channel for receiving the server's response to this request. Ensures the incoming channel is properly managed."
  `(%CALL-WITH-JSONRPC ,jsonrpc-client ,json ,id ,progress-token (LAMBDA (,incoming-channel) ,@body)))

(defun depaginate (first-page remaining-pages)
  "Combines `first-page` and `remaining-pages` into a single object. For each unique key, values are merged as follows: if both values are lists, they are appended. If only one page has a value, that value is used. If both pages have non-list values, or one is a list and the other is not, the value from `first-page` is currently preferred. This behavior may lead to data loss from `remaining-pages` for non-list values."
  (map 'list (lambda (key)
               (cons key
                     (let ((first-value (funcall (object-ref-function key) first-page))
                           (more-values (funcall (object-ref-function key) remaining-pages)))
                       (cond ((null more-values) first-value)
                             ((null first-value) more-values)
                             ((and (consp first-value)
                                   (consp more-values))
                              (append first-value more-values))
                             ((consp first-value) (append first-value (list more-values)))
                             ((consp more-values) (cons first-value more-values))
                             (t first-value)))))
       (remove-duplicates
        (append (keys first-page)
                (keys remaining-pages))
        :test #'equal)))

(defun jsonrpc (jsonrpc-client method params progress-token)
  "Sends a JSON-RPC request to the server with the given `method` and `params`. Handles automatic pagination by recursively fetching subsequent pages if a `next-cursor` is present in the response. Combines paginated results using `depaginate`. Signals a Lisp error if the server returns a JSON-RPC error."
  (check-type jsonrpc-client jsonrpc-client)
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
                                              (let ((new-params (alexandria:copy-hash-table params)))
                                                (setf (gethash :cursor new-params) cursor)
                                                new-params)))))
                   id
                   progress-token)

      (let* ((response (chanl:recv incoming-channel))
             (err (get-error response))
             (result (get-result response))
             (cursor (get-next-cursor result)))
        (cond (err
               (error "JSON-RPC Error: ~a" (get-message err)))
              ((null cursor) result)
              (t (depaginate result (next-page (random-id) cursor))))))))
