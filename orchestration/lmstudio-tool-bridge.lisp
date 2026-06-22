;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter *lmstudio-tool-bridge-session-header* "X-Gemini-Bridge-Session")
(defparameter *lmstudio-tool-bridge-registration-ttl-seconds* 600)
(defparameter *lmstudio-tool-bridge* nil)
(defparameter *lmstudio-tool-bridge-registrations* (make-hash-table :test #'equal))
(defparameter *lmstudio-tool-bridge-protocol-version* "2025-06-18")
(defparameter *lmstudio-tool-bridge-lock*
  (sb-thread:make-mutex :name "lmstudio-tool-bridge-lock"))

(defun safely-cleanup-lmstudio-tool-bridge-resource (description thunk)
  "Runs THUNK during LM Studio tool bridge cleanup and logs teardown failures."
  (handler-case
      (funcall thunk)
    (error (e)
      (log-warn "LM Studio tool bridge cleanup failed while ~A: ~A" description e))))

(defstruct lmstudio-tool-bridge-registration
  declarations
  functions
  created-at)

(defstruct lmstudio-tool-bridge-server
  host
  port
  path
  url
  listener
  thread
  stop-requested-p)

(defun lmstudio-tool-bridge-header-object (session-token)
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash *lmstudio-tool-bridge-session-header* headers) session-token)
    headers))

(defun active-lmstudio-tool-bridge-url ()
  (and *lmstudio-tool-bridge*
       (lmstudio-tool-bridge-server-url *lmstudio-tool-bridge*)))

(defun lmstudio-tool-bridge-require-sockets ()
  (require :sb-bsd-sockets))

(defun lmstudio-tool-bridge-socket-symbol (name)
  (lmstudio-tool-bridge-require-sockets)
  (or (find-symbol name "SB-BSD-SOCKETS")
      (error "SB-BSD-SOCKETS symbol ~a is unavailable." name)))

(defun lmstudio-tool-bridge-socket-call (name &rest args)
  (apply (symbol-function (lmstudio-tool-bridge-socket-symbol name)) args))

(defun lmstudio-tool-bridge-make-listener (host port)
  (let* ((socket-class (lmstudio-tool-bridge-socket-symbol "INET-SOCKET"))
         (listener (make-instance socket-class :type :stream :protocol :tcp))
         (address (lmstudio-tool-bridge-socket-call "MAKE-INET-ADDRESS" host)))
    (lmstudio-tool-bridge-socket-call "SOCKET-BIND" listener address port)
    (lmstudio-tool-bridge-socket-call "SOCKET-LISTEN" listener 16)
    listener))

(defun lmstudio-tool-bridge-listener-port (listener)
  (nth-value 1 (lmstudio-tool-bridge-socket-call "SOCKET-NAME" listener)))

(defun lmstudio-tool-bridge-accept (listener)
  (lmstudio-tool-bridge-socket-call "SOCKET-ACCEPT" listener))

(defun lmstudio-tool-bridge-stream (socket)
  (lmstudio-tool-bridge-socket-call "SOCKET-MAKE-STREAM"
                                    socket
                                    :input t
                                    :output t
                                    :element-type 'character
                                    :external-format :utf-8
                                    :buffering :full))

(defun lmstudio-tool-bridge-close-socket (socket)
  (safely-cleanup-lmstudio-tool-bridge-resource
   "closing a bridge socket"
   (lambda ()
     (lmstudio-tool-bridge-socket-call "SOCKET-CLOSE" socket))))

(defun lmstudio-tool-bridge-close-listener (server)
  (let ((listener (lmstudio-tool-bridge-server-listener server)))
    (when listener
      (setf (lmstudio-tool-bridge-server-listener server) nil)
      (lmstudio-tool-bridge-close-socket listener))))

(defun lmstudio-tool-bridge-touch-listener (server)
  (safely-cleanup-lmstudio-tool-bridge-resource
   "touching the bridge listener"
   (lambda ()
     (let* ((socket-class (lmstudio-tool-bridge-socket-symbol "INET-SOCKET"))
            (client (make-instance socket-class :type :stream :protocol :tcp))
            (address (lmstudio-tool-bridge-socket-call "MAKE-INET-ADDRESS"
                                                       (lmstudio-tool-bridge-server-host server))))
       (unwind-protect
            (lmstudio-tool-bridge-socket-call "SOCKET-CONNECT"
                                              client
                                              address
                                              (lmstudio-tool-bridge-server-port server))
         (lmstudio-tool-bridge-close-socket client))))))

(defun lmstudio-tool-bridge-read-body (stream content-length)
  (let ((body (make-string content-length)))
    (read-sequence body stream)
    body))

(defun lmstudio-tool-bridge-read-request (stream)
  (let ((request-line (read-line stream nil nil)))
    (when request-line
      (let ((headers (make-hash-table :test #'equal)))
        (loop for line = (read-line stream nil nil)
              while (and line (> (length line) 0))
              do (let ((colon (position #\: line)))
                   (when colon
                     (let ((name (string-downcase (string-trim '(#\Space #\Tab)
                                                              (subseq line 0 colon))))
                           (value (string-trim '(#\Space #\Tab)
                                               (subseq line (1+ colon)))))
                       (setf (gethash name headers) value)))))
        (let* ((parts (str:split " " request-line))
               (method (first parts))
               (path (second parts))
               (content-length-header (gethash "content-length" headers))
               (content-length (if content-length-header
                                   (parse-integer content-length-header)
                                   0))
               (body (if (plusp content-length)
                         (lmstudio-tool-bridge-read-body stream content-length)
                         "")))
          (values method path headers body))))))

(defun lmstudio-tool-bridge-write-response (stream status content-type body)
  (let ((payload (or body "")))
    (format stream "HTTP/1.1 ~a~c~c" status #\Return #\Linefeed)
    (format stream "Content-Type: ~a~c~c" content-type #\Return #\Linefeed)
    (format stream "Content-Length: ~d~c~c" (length payload) #\Return #\Linefeed)
    (format stream "Connection: close~c~c" #\Return #\Linefeed)
    (format stream "~c~c" #\Return #\Linefeed)
    (when (> (length payload) 0)
      (write-string payload stream))
    (finish-output stream)))

(defun lmstudio-tool-bridge-json-response (stream status payload)
  (lmstudio-tool-bridge-write-response stream
                                       status
                                       "application/json"
                                       (cl-json:encode-json-to-string payload)))

(defun lmstudio-tool-bridge-empty-response (stream status)
  (lmstudio-tool-bridge-write-response stream status "text/plain; charset=utf-8" ""))

(defun lmstudio-tool-bridge-purge-registrations ()
  (let ((cutoff (- (get-universal-time) *lmstudio-tool-bridge-registration-ttl-seconds*)))
    (maphash (lambda (token registration)
               (when (< (lmstudio-tool-bridge-registration-created-at registration) cutoff)
                 (remhash token *lmstudio-tool-bridge-registrations*)))
             *lmstudio-tool-bridge-registrations*)))

(defun lmstudio-tool-declaration-schema-json (schema)
  (cond ((null schema) nil)
        ((hash-table-p schema)
         (let ((out (make-hash-table :test #'equal)))
           (maphash (lambda (key value)
                      (let ((json-key
                              (cond ((or (eq key :type) (equal key "type")) "type")
                                    ((or (eq key :required) (equal key "required")) "required")
                                    ((stringp key) key)
                                    ((keywordp key) (cl-json:lisp-to-camel-case (symbol-name key)))
                                    ((symbolp key) (cl-json:lisp-to-camel-case (symbol-name key)))
                                    (t (princ-to-string key)))))
                        (setf (gethash json-key out)
                              (cond ((equal json-key "type")
                                     (map-type-to-string value))
                                    ((equal json-key "required")
                                     (coerce (mapcar (lambda (item)
                                                       (if (symbolp item)
                                                           (cl-json:lisp-to-camel-case (symbol-name item))
                                                           (princ-to-string item)))
                                                     (adapter-as-list value))
                                             'vector))
                                    (t
                                     (lmstudio-tool-declaration-schema-json value))))))
                    schema)
           out))
        ((listp schema)
         (mapcar #'lmstudio-tool-declaration-schema-json schema))
        ((vectorp schema)
         (map 'vector #'lmstudio-tool-declaration-schema-json schema))
        (t schema)))

(defun lmstudio-tool-declaration->mcp-tool (declaration)
  (let* ((tool (make-hash-table :test #'equal))
         (name (get-name declaration))
         (description (get-description declaration))
         (parameters (or (get-parameters-json-schema declaration)
                         (get-parameters declaration)))
         (response (or (get-response-json-schema declaration)
                       (get-response declaration))))
    (setf (gethash "name" tool) name)
    (setf (gethash "title" tool) name)
    (when description
      (setf (gethash "description" tool) description))
    (setf (gethash "inputSchema" tool)
          (or (lmstudio-tool-declaration-schema-json parameters)
              (let ((empty (make-hash-table :test #'equal)))
                (setf (gethash "type" empty) "object"
                      (gethash "properties" empty) (make-hash-table :test #'equal))
                empty)))
    (when response
      (setf (gethash "outputSchema" tool)
            (lmstudio-tool-declaration-schema-json response)))
    tool))

(defun find-lmstudio-tool-bridge-functions (declarations content-generator)
  (let ((functions (standard-functions-and-handlers content-generator)))
    (mapcar (lambda (declaration)
              (let* ((name (get-name declaration))
                     (entry (assoc name functions :key #'get-name :test #'equal))
                     (handler (and entry (cdr entry))))
                (unless entry
                  (error "LM Studio tool bridge could not find a local handler for tool ~S." name))
                (unless handler
                  (error "LM Studio tool bridge found tool ~S but it has no handler." name))
                (unless (functionp handler)
                  (error "LM Studio tool bridge found tool ~S but its handler is not callable." name))
                entry))
            declarations)))

(defun lmstudio-tool-bridge-function-entry-parameter-properties (entry)
  (let* ((declaration (and entry (car entry)))
         (parameters (and declaration
                          (or (get-parameters-json-schema declaration)
                              (get-parameters declaration)))))
    (and parameters
         (get-properties parameters))))

(defun lmstudio-tool-bridge-invoke-local-function-by-name (name arguments functions)
  (let* ((resolved-name (resolve-function-call-alias name))
         (entry (assoc resolved-name functions :key #'get-name :test #'equal))
         (schema (lmstudio-tool-bridge-function-entry-parameter-properties entry))
         (handler (and entry (cdr entry)))
         (arglist (default-process-args (or arguments (object)) schema)))
    (labels ((success-response (answers output-string error-string)
               (if (consp answers)
                   (if (consp (cdr answers))
                       (object :result (car answers)
                               :additional-results (coerce (cdr answers) 'vector)
                               :standard-output output-string
                               :error-output error-string)
                       (object :result (car answers)
                               :standard-output output-string
                               :error-output error-string))
                   (object :result jsonx:+json-null+
                           :standard-output output-string
                           :error-output error-string))))
      (values resolved-name
              (cond
                ((null entry)
                 (object :error (format nil "Function `~s` does not exist." resolved-name)))
                ((null handler)
                 (object :error (format nil "Function `~s` has no handler." resolved-name)))
                ((not (functionp handler))
                 (object :error (format nil "Handler for `~s` is not a function." resolved-name)))
                (t
                 (let ((answers nil)
                       (output-string nil)
                       (error-string nil))
                   (handler-case
                       (progn
                         (setf output-string
                               (with-output-to-string (out)
                                 (let ((*standard-output* (make-broadcast-stream *standard-output* out)))
                                   (setf error-string
                                         (with-output-to-string (err)
                                           (let ((*error-output* (make-broadcast-stream *error-output* err)))
                                             (setf answers
                                                   (multiple-value-list (apply handler arglist)))))))))
                         (success-response answers output-string error-string))
                     (error (e)
                       (object :error (format nil "~a" e)
                               :standard-output output-string
                               :error-output error-string))))))))))

(defun register-lmstudio-tool-bridge-session (declarations content-generator)
  (let ((token (random-id 24))
        (functions (find-lmstudio-tool-bridge-functions declarations content-generator)))
    (sb-thread:with-mutex (*lmstudio-tool-bridge-lock*)
      (lmstudio-tool-bridge-purge-registrations)
      (setf (gethash token *lmstudio-tool-bridge-registrations*)
            (make-lmstudio-tool-bridge-registration
             :declarations declarations
             :functions functions
             :created-at (get-universal-time))))
    token))

(defun find-lmstudio-tool-bridge-registration (headers)
  (let ((token (gethash (string-downcase *lmstudio-tool-bridge-session-header*) headers)))
    (when token
      (sb-thread:with-mutex (*lmstudio-tool-bridge-lock*)
        (lmstudio-tool-bridge-purge-registrations)
        (gethash token *lmstudio-tool-bridge-registrations*)))))

(defun lmstudio-tool-bridge-session-headers (declarations &key content-generator)
  (when content-generator
    (lmstudio-tool-bridge-header-object
     (register-lmstudio-tool-bridge-session declarations content-generator))))

(defun lmstudio-tool-result-text (value)
  (cond ((null value) "null")
        ((stringp value) value)
        ((or (hash-table-p value) (consp value) (vectorp value) (numberp value) (typep value 'jsonx:json-literal))
         (cl-json:encode-json-to-string value))
        (t
         (princ-to-string value))))

(defun lmstudio-tool-bridge-call-result (registration name arguments)
  (multiple-value-bind (resolved-name response)
      (lmstudio-tool-bridge-invoke-local-function-by-name
       name arguments
       (lmstudio-tool-bridge-registration-functions registration))
    (declare (ignore resolved-name))
    (let* ((result (adapter-field response "result" :result))
           (error-text (adapter-field response "error" :error))
           (content (vector (object :type "text"
                                    :text (if error-text
                                              error-text
                                              (lmstudio-tool-result-text result)))))
           (payload (object :content content)))
      (when (and result (hash-table-p result))
        (setf (gethash "structuredContent" payload) result))
      (when error-text
        (setf (gethash "isError" payload) jsonx:+json-true+))
      payload)))

(defun lmstudio-tool-bridge-success (id result)
  (object :jsonrpc "2.0" :id id :result result))

(defun lmstudio-tool-bridge-error (id code message)
  (object :jsonrpc "2.0"
          :id id
          :error (object :code code :message message)))

(defun lmstudio-tool-bridge-handle-jsonrpc (headers payload)
  (let ((id (adapter-field payload "id" :id))
        (method (adapter-field payload "method" :method))
        (registration (find-lmstudio-tool-bridge-registration headers)))
    (cond
      ((null method)
       (values 202 nil))
      ((equal method "notifications/initialized")
       (values 202 nil))
      ((equal method "ping")
       (values 200 (lmstudio-tool-bridge-success id (object))))
      ((equal method "initialize")
       (values 200
               (lmstudio-tool-bridge-success
                id
                (object :protocol-version *lmstudio-tool-bridge-protocol-version*
                        :capabilities (object :tools (object :list-changed nil))
                        :server-info (object :name "gemini-lmstudio-tool-bridge"
                                             :title "Gemini LM Studio Tool Bridge"
                                             :version "0.1.0")
                        :instructions "Exposes Gemini local tool handlers to LM Studio over MCP Streamable HTTP."))))
      ((null id)
       (values 202 nil))
      ((null registration)
       (values 200
               (lmstudio-tool-bridge-error
                id -32000
                (format nil "Missing or expired ~a header."
                        *lmstudio-tool-bridge-session-header*))))
      ((equal method "tools/list")
       (values 200
               (lmstudio-tool-bridge-success
                id
                (object :tools
                        (coerce (mapcar #'lmstudio-tool-declaration->mcp-tool
                                        (lmstudio-tool-bridge-registration-declarations registration))
                                'vector)))))
      ((equal method "tools/call")
       (let* ((params (adapter-field payload "params" :params))
              (name (adapter-field params "name" :name))
              (arguments (or (adapter-field params "arguments" :arguments)
                             (object))))
         (if (null (find name (lmstudio-tool-bridge-registration-functions registration)
                         :key (lambda (entry) (get-name (car entry)))
                         :test #'equal))
             (values 200
                     (lmstudio-tool-bridge-error id -32602
                                                 (format nil "Unknown tool: ~a" name)))
             (values 200
                     (lmstudio-tool-bridge-success
                      id
                      (lmstudio-tool-bridge-call-result registration name arguments))))))
      (t
       (values 200
               (lmstudio-tool-bridge-error id -32601
                                           (format nil "Method not found: ~a" method)))))))

(defun lmstudio-tool-bridge-handle-client (server client-socket)
  (let ((stream nil))
    (unwind-protect
         (progn
           (setf stream (lmstudio-tool-bridge-stream client-socket))
           (multiple-value-bind (method path headers body)
              (lmstudio-tool-bridge-read-request stream)
             (cond
              ((null method)
               (lmstudio-tool-bridge-empty-response stream "400 Bad Request"))
              ((not (equal path (lmstudio-tool-bridge-server-path server)))
               (lmstudio-tool-bridge-empty-response stream "404 Not Found"))
              ((equal method "GET")
               (lmstudio-tool-bridge-empty-response stream "405 Method Not Allowed"))
              ((not (equal method "POST"))
               (lmstudio-tool-bridge-empty-response stream "405 Method Not Allowed"))
              (t
               (handler-case
                   (let ((payload (if (> (length body) 0)
                                      (jsonx:with-decoder-jrm-semantics
                                        (cl-json:decode-json-from-string body))
                                      (object))))
                     (multiple-value-bind (status response-payload)
                         (lmstudio-tool-bridge-handle-jsonrpc headers payload)
                       (if response-payload
                           (lmstudio-tool-bridge-json-response stream "200 OK" response-payload)
                           (lmstudio-tool-bridge-empty-response
                            stream
                            (if (= status 202) "202 Accepted" "200 OK")))))
                 (error (e)
                   (lmstudio-tool-bridge-json-response
                    stream
                    "500 Internal Server Error"
                    (lmstudio-tool-bridge-error
                     jsonx:+json-null+
                     -32603
                     (format nil "~a" e)))))))))
      (when stream
        (safely-cleanup-lmstudio-tool-bridge-resource
         "closing a bridge client stream"
         (lambda () (close stream))))
      (lmstudio-tool-bridge-close-socket client-socket))))

(defun lmstudio-tool-bridge-serve (server)
  (loop until (lmstudio-tool-bridge-server-stop-requested-p server)
        do (handler-case
               (multiple-value-bind (client-socket)
                   (lmstudio-tool-bridge-accept (lmstudio-tool-bridge-server-listener server))
                 (when client-socket
                   (lmstudio-tool-bridge-handle-client server client-socket)))
             (error ()
               (unless (lmstudio-tool-bridge-server-stop-requested-p server)
                 (log-warn "LM Studio tool bridge accept loop hit an unexpected error.")
                 (sleep 0.05))))))

(defun start-lmstudio-tool-bridge (&key (host "127.0.0.1") (port 0) (path "/mcp"))
  (when *lmstudio-tool-bridge*
    (stop-lmstudio-tool-bridge))
  (let* ((listener (lmstudio-tool-bridge-make-listener host port))
         (actual-port (lmstudio-tool-bridge-listener-port listener))
         (server (make-lmstudio-tool-bridge-server
                  :host host
                  :port actual-port
                  :path path
                  :url (format nil "http://~a:~d~a" host actual-port path)
                  :listener listener
                  :stop-requested-p nil)))
    (setf (lmstudio-tool-bridge-server-thread server)
          (sb-thread:make-thread (lambda ()
                                   (lmstudio-tool-bridge-serve server))
                                 :name "LM Studio Tool Bridge"))
    (setf *lmstudio-tool-bridge* server)
    ;; Prime the listener so early clients do not race bridge thread startup.
    (lmstudio-tool-bridge-touch-listener server)
    server))

(defun stop-lmstudio-tool-bridge ()
  (when *lmstudio-tool-bridge*
    (let ((server *lmstudio-tool-bridge*))
      (setf *lmstudio-tool-bridge* nil
            (lmstudio-tool-bridge-server-stop-requested-p server) t)
      (lmstudio-tool-bridge-touch-listener server)
      (lmstudio-tool-bridge-close-listener server)
      (when (lmstudio-tool-bridge-server-thread server)
        (safely-cleanup-lmstudio-tool-bridge-resource
         "joining the bridge server thread"
         (lambda ()
           (sb-thread:join-thread (lmstudio-tool-bridge-server-thread server)))))
      server)))
