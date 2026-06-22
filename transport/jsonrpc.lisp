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
   (mutex                    :initform (sb-thread:make-mutex :name "jsonrpc-client-lock") :reader mutex)
   (latest-server-output     :initform (get-universal-time)            :accessor latest-server-output)
   (process-info             :initarg :process-info :accessor process-info)
   (request-threads          :initform (make-hash-table :test #'equal) :reader request-threads)
   ;; New slots for self-healing and lifecycle management
   (client-name              :initarg :name :accessor client-name)
   (command                  :initarg :command :accessor client-command)
   (args                     :initarg :args :accessor client-args)
   (unsolicited-handler      :initarg :unsolicited-handler :accessor unsolicited-handler)
   (input-thread             :initform nil :accessor input-thread)
   (output-thread            :initform nil :accessor output-thread)
   (error-thread             :initform nil :accessor error-thread)
   (bookkeeper-thread        :initform nil :accessor bookkeeper-thread)
   (is-reconnecting          :initform nil :accessor is-reconnecting-p))
  (:documentation "Represents a JSON-RPC client, managing outgoing requests, incoming responses, and thread synchronization. It maintains channels for communication, a mutex for thread safety, and tracks server activity for connection management."))

(defun call-with-jsonrpc-client-lock (jsonrpc-client thunk)
  "Call THUNK while holding the lock of the JSONRPC-CLIENT."
  (check-type jsonrpc-client jsonrpc-client)
  (check-type thunk function)
  (sb-thread:with-mutex ((mutex jsonrpc-client))
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
  (letrec ((new-thread (sb-thread:make-thread
                        (lambda ()
                          (block nil
                            (unwind-protect
                                 (restart-case
                                     (progn (with-jsonrpc-client-lock (jsonrpc-client)
                                              (setf (gethash id (request-threads jsonrpc-client)) new-thread))
                                            (funcall thunk))
                                   (abort ()
                                     (log-info "Aborting request thread for ID ~a" id)
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

(defun stop-jsonrpc-client-threads (client &optional eof-value)
  "Stops all background threads and terminates the child process for the JSON-RPC client."
  (let ((proc (process-info client))
        (eof (or eof-value (cons nil nil))))
    (ignore-errors (chanl:send (outgoing-channel client) eof))
    (when proc
      (ignore-errors
       (when (uiop:process-alive-p proc)
         (uiop:terminate-process proc :urgent t)
         (uiop:wait-process proc))
       (uiop:close-streams proc)))
    (flet ((kill-thread (slot-name)
             (let ((thread (slot-value client slot-name)))
               (when (and thread (sb-thread:thread-alive-p thread))
                 (ignore-errors
                  (sb-thread:interrupt-thread thread #'abort)
                  (sb-thread:join-thread thread :timeout 2))
                 (setf (slot-value client slot-name) nil)))))
      (kill-thread 'input-thread)
      (kill-thread 'output-thread)
      (kill-thread 'error-thread)
      (kill-thread 'bookkeeper-thread))))

(defun start-jsonrpc-client-threads (client eof-value)
  "Spins up the input, output, error, and bookkeeper background threads for the client."
  (let* ((process-info (process-info client))
         (name (client-name client))
         (unsolicited-message-handler (unsolicited-handler client)))
    (flet ((json-send (json)
             (handler-case
                 (progn
                   (cl-json:encode-json json (uiop:process-info-input process-info))
                   (terpri (uiop:process-info-input process-info))
                   (finish-output (uiop:process-info-input process-info)))
               (condition () nil)))

           (line-receiver (stream-name stream receiver)
             (lambda ()
               (unwind-protect
                    (let iter ((line nil))
                      (when (and line (not (eq line eof-value)))
                        (handler-case (funcall receiver line)
                          (condition (c) (log-warn "Receiver error: ~a" c))))
                      (unless (eq line eof-value)
                        (iter (handler-case (read-line stream nil eof-value)
                                (condition () eof-value)))))
                 (log-info "Line receiver for ~a ~a exiting..." stream-name name)))))

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
                                   (log-warn "Unexpected jsonrpc version: ~s" message)
                                   nil)

                                  ((and (ping-id? (get-id message))
                                        (eql (get-result message) jsonx:+json-empty-object+))
                                   nil)

                                  ((equal (get-method message) "ping")
                                   (json-send (object :jsonrpc "2.0"
                                                      :id (get-id message)
                                                      :result (object))))

                                  (t (funcall receiver message)))))
                    (json:json-syntax-error (e)
                      (log-warn "JSON parse error: ~a" e)
                      (funcall receiver
                               (object :jsonrpc "2.0"
                                       :method "notification/message"
                                       :params (object :level "info" :data line)))))))))

        (let ((input-thread
                (sb-thread:make-thread
                 (lambda ()
                   (unwind-protect
                        (let iter ((json nil))
                          (unless (eq json eof-value)
                            (when json (json-send json))
                            (iter (chanl:recv (outgoing-channel client)))))
                     (log-info "Exiting JSONRPC send thread for ~a..." name)))
                 :name (format nil "~a JSONRPC Output" name)))

              (output-thread
                (sb-thread:make-thread
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
                                  (request-id (get-request-id params))
                                  (thread (with-jsonrpc-client-lock (client)
                                            (gethash request-id (request-threads client)))))
                             (when (and thread (sb-thread:thread-alive-p thread))
                               (sb-thread:interrupt-thread thread #'abort))))

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
                                 (progn
                                   (log-warn "No incoming channel for message: ~s" message)))))

                          (t (start-request-thread client (get-id message)
                                                   (lambda ()
                                                     (funcall unsolicited-message-handler message)))))))
                 :name (format nil "~a JSONRPC Output" name)))

              (error-thread
                (sb-thread:make-thread
                 (error-receiver
                  (lambda (line)
                    (unless (eql line eof-value)
                      (log-warn "[~a] ~a" name line))))
                 :name (format nil "~a JSONRPC Error Output" name))))

          (let ((bk-thread
                  (sb-thread:make-thread
                   (lambda ()
                     (let iter ()
                       (sleep +jsonrpc-bookkeeper-interval+)
                       (cond ((not (uiop:process-alive-p (process-info client)))
                              (log-warn "Process for ~a has exited." name)
                              (chanl:send (outgoing-channel client) eof-value))

                             ((> (- (get-universal-time) (latest-server-output client))
                                 +jsonrpc-nonresponse-timeout+)
                              (log-error "No response from ~a for ~a seconds, assuming it hanged."
                                         name
                                         +jsonrpc-nonresponse-timeout+)
                              (chanl:send (outgoing-channel client) eof-value)
                              (sb-thread:join-thread input-thread)
                              (uiop:close-streams process-info)
                              (sb-thread:interrupt-thread error-thread #'abort)
                              (sb-thread:interrupt-thread output-thread #'abort)
                              (uiop:terminate-process (process-info client) :urgent t)
                              (uiop:wait-process (process-info client))
                              (log-warn "Process for ~a has been terminated." name)
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
                              (when (> (- (get-universal-time) (latest-server-output client))
                                       +jsonrpc-ping-timeout+)
                                (jsonrpc-ping client))

                              (with-jsonrpc-client-lock (client)
                                (map nil (lambda (entry)
                                           (let* ((id (car entry))
                                                  (channel-info (cdr entry))
                                                  (channel (first channel-info))
                                                  (start-time (third channel-info)))
                                             (when (> (- (get-universal-time) start-time)
                                                      +jsonrpc-request-timeout+)
                                               (log-warn "Request ~a to ~a has timed out, cancelling..."
                                                         id name)
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
                   :name (format nil "~a Bookkeeper" name))))
            (setf (input-thread client) input-thread
                  (output-thread client) output-thread
                  (error-thread client) error-thread
                  (bookkeeper-thread client) bk-thread)))))))

(defun reconnect-jsonrpc-client (client)
  "Closes the old connection and launches a new server process, restarting all transport threads."
  (let ((eof-value (cons nil nil)))
    (log-warn "Attempting to reconnect and heal JSON-RPC client for ~a..." (client-name client))
    (setf (is-reconnecting-p client) t)
    (unwind-protect
         (progn
           (stop-jsonrpc-client-threads client eof-value)
           (let ((new-proc (uiop:launch-program (append (client-command client) (client-args client))
                                                :error-output :stream
                                                :input        :stream
                                                :output       :stream)))
             (setf (process-info client) new-proc)
             (setf (latest-server-output client) (get-universal-time))
             (start-jsonrpc-client-threads client eof-value)
             (log-info "Successfully reconnected and healed JSON-RPC client for ~a." (client-name client))))
      (setf (is-reconnecting-p client) nil))))

(defun ensure-jsonrpc-client-alive (client)
  "Ensures that the JSON-RPC client's child process is alive. If it has exited, automatically restarts/reconnects it."
  (unless (is-reconnecting-p client)
    (let ((proc (process-info client)))
      (unless (and proc (uiop:process-alive-p proc))
        (sb-thread:with-mutex ((mutex client))
          (let ((proc-locked (process-info client)))
            (unless (and proc-locked (uiop:process-alive-p proc-locked))
              (reconnect-jsonrpc-client client))))))))

(defun create-jsonrpc-client (name command args unsolicited-message-handler)
  "Creates and initializes a multi-threaded JSON-RPC client connected to an external server process."
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
         (client (make-instance 'jsonrpc-client
                                :process-info process-info
                                :name name
                                :command command
                                :args args
                                :unsolicited-handler unsolicited-message-handler)))
    (start-jsonrpc-client-threads client eof-value)
    client))

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
  (ensure-jsonrpc-client-alive jsonrpc-client)
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
