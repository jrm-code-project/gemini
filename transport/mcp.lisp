;;; -*- Lisp -*-

(in-package "GEMINI")

;;;;;;;;
;;; Model-Context-Protocol (MCP) Server Support
;;;;;;;;

(defun mcp-config-pathname ()
  (uiop/configuration:xdg-config-pathname "mcp/mcp.lisp"))

(defun read-mcp-config ()
  (let ((config-pathname (mcp-config-pathname)))
    (when (and config-pathname
               (probe-file config-pathname))
      (with-open-file (stream config-pathname :direction :input :if-does-not-exist nil)
        (read stream nil nil)))))

(defclass mcp-server ()
  ((capability/completions            :initarg nil :accessor completions-capability)
   (capability/logging                :initarg nil :accessor logging-capability)
   (capability/prompts                :initarg nil :accessor prompts-capability)
   (capability/prompts/list-changed   :initarg nil :accessor prompts-list-changed-capability)
   (capability/resources              :initarg nil :accessor resources-capability)
   (capability/resources/list-changed :initarg nil :accessor resources-list-changed-capability)
   (capability/resources/subscribe    :initarg nil :accessor resources-subscribe-capability)
   (capability/tools                  :initarg nil :accessor tools-capability)
   (capability/tools/list-changed     :initarg nil :accessor tools-list-changed-capability)

   (autonym             :accessor get-autonym)
   (instructions        :accessor get-instructions)
   (server-instructions :accessor get-server-instructions)
   (protocol-version    :accessor get-protocol-version)
   (title               :accessor get-title)
   (version             :accessor get-version)

   (config :initarg :config :reader config)
   (context :initarg nil :accessor context)
   (delayed-prompts   :initarg :delayed-prompts   :reader delayed-prompts)
   (delayed-resources :initarg :delayed-resources :reader delayed-resources)
   (delayed-resource-templates :initarg :delayed-resource-templates :reader delayed-resource-templates)
   (delayed-tools     :initarg :delayed-tools     :reader delayed-tools)
   (jsonrpc-client    :accessor jsonrpc-client)
   (mutex :initform (sb-thread:make-mutex :name "mcp-server-lock") :reader mutex)
   (name  :initarg :name :reader get-name)
   (notification-filter :initform (constantly t) :accessor notification-filter)
   (notification-stream :initform *trace-output* :accessor notification-stream)
   (resource-subscriptions :initform (make-hash-table :test #'equal) :reader resource-subscriptions)
   ))

(defun call-with-mcp-server-mutex (mcp-server thunk)
  (sb-thread:with-mutex ((mutex mcp-server))
    (funcall thunk)))

(defmacro with-mcp-server-mutex ((mcp-server) &body body)
  `(CALL-WITH-MCP-SERVER-MUTEX ,mcp-server (LAMBDA () ,@body)))

(defmethod get-prompts ((object mcp-server))
  (force (delayed-prompts object)))

(defmethod get-prompt ((object mcp-server) name)
  (jsonrpc (jsonrpc-client object) "prompts/get" (object :name name) nil))

(defmethod read-resource ((object mcp-server) uri)
  (let ((contents (get-contents (jsonrpc (jsonrpc-client object) "resources/read" (object :uri uri) nil))))
    (cond ((and (consp contents)
                (null (cdr contents)))
           (let* ((resource-spec (car contents))
                  (mime-type (get-mime-type resource-spec)))
             (cond ((equal mime-type "text/plain") (get-text resource-spec))
                   ((equal mime-type "application/octet-stream")
                    (cl-base64:base64-string-to-usb8-array (get-blob resource-spec)))
                   (t (error "Unrecognized mime-type ~s" mime-type)))))
          ((consp contents) (error "Multiple resources returned? ~s" contents))
          ((null contents) nil)
          (t (error "Unrecognized contents: ~s" contents)))))

(defmethod get-resources ((object mcp-server))
  (force (delayed-resources object)))

(defmethod get-resource-templates ((object mcp-server))
  (force (delayed-resource-templates object)))

(defun find-resource-template (mcp-server name)
  (find name (get-resource-templates mcp-server) :test #'equal :key #'get-name))

(defun expand-resource-template (resource-template substitution-plist)
  (let ((expanded-1 (expand-resource-template-1 resource-template substitution-plist)))
    (if (equal expanded-1 resource-template)
        expanded-1
        (expand-resource-template expanded-1 substitution-plist))))

(defun expand-resource-template-1 (resource-template substitution-plist)
  (let ((open-curly (position #\{ resource-template)))
    (if (null open-curly)
        resource-template
        (let ((close-curly (position #\} resource-template :start open-curly)))
          (if (null close-curly)
              (error "Unbalanced curly braces in ~s." resource-template)
              (let ((before  (subseq resource-template 0  open-curly))
                    (between (->keyword (subseq resource-template (1+ open-curly) close-curly)))
                    (after   (subseq resource-template (1+ close-curly) (length resource-template))))
                (format nil "~a~a~a"
                        before
                        (or (getf substitution-plist between)
                            (error "No substitution for {~a}" between))
                        after)))))))

(defun read-resource-by-template (mcp-server template-name &rest substitution-plist)
  "Read a resource by its template name, substituting args into the template."
  (let ((template (get-uri-template (find-resource-template mcp-server template-name))))
    (if (null template)
        (error "No resource template named ~a" template-name)
        (let ((expanded-template (expand-resource-template template substitution-plist)))
          (read-resource mcp-server expanded-template)))))

(defun subscribe-to-resource (mcp-server resource-uri receiver)
  "Subscribe to a resource URI in the MCP server."
  (setf (gethash resource-uri (resource-subscriptions mcp-server)) receiver)
  (jsonrpc (jsonrpc-client mcp-server) "resources/subscribe" (object :uri resource-uri) nil))

(defmethod get-system-instruction ((object mcp-server))
  (get-system-instruction (config object)))

(defmethod get-tools ((object mcp-server))
  (force (delayed-tools object)))

(defmethod print-object ((server mcp-server) stream)
  (print-unreadable-object (server stream :type t)
    (format stream "~a" (get-name server))))

(defun mcp-server-alive? (mcp-server)
  (jsonrpc-client-alive? (jsonrpc-client mcp-server)))

(defun find-mcp-server (name &key (start-if-needed t))
  (when (and start-if-needed
             (null *mcp-servers*))
    (start-mcp-servers))
  (find name *mcp-servers* :key #'get-name :test #'equal))

(defun create-mcp-server (name config)
  (letrec ((server
            (make-instance
             'mcp-server
             :config config
             :delayed-prompts   (delay
                                 (when (prompts-capability server)
                                   (get-prompts (jsonrpc jsonrpc-clnt "prompts/list" '() nil))))
             :delayed-resources (delay
                                 (when (resources-capability server)
                                   (get-resources (jsonrpc jsonrpc-clnt "resources/list" '() nil))))
             :delayed-resource-templates (delay
                                          (when (resources-capability server)
                                            (get-resource-templates
                                             (jsonrpc jsonrpc-clnt "resources/templates/list" '() nil))))
             :delayed-tools     (delay
                                 (when (tools-capability server)
                                   (get-tools (jsonrpc jsonrpc-clnt "tools/list" '() nil))))
             :name name))

           (jsonrpc-clnt
            (progn
              (map nil (lambda (entry)
                         (setf (uiop:getenv (car entry)) (cdr entry)))
                   (get-env config))

              (create-jsonrpc-client
               name
               (get-command config)
               (get-args config)
               (lambda (message)
                 (cond ((equal (get-method message) "elicitation/create")
                        (handle-elicitation server message))

                       ((and (equal (get-method message) "notifications/message")
                             (notification-stream server))
                        (handle-notification-message server message))

                       ((and (equal (get-method message) "notifications/progress")
                             (notification-stream server))
                        (handle-notification-progress server message))

                       ((equal (get-method message) "notifications/resources/updated")
                        (handle-notification-resources-updated server message))

                       ((equal (get-method message) "roots/list")
                        (handle-roots-list server message))

                       ((equal (get-method message) "sampling/createMessage")
                        (handle-sampling-create-message server message))

                       (t (log-warn "Unexpected MCP message: ~s" (dehashify message)))))))))

    (setf (jsonrpc-client server) jsonrpc-clnt)
    (handler-case
        (trivial-timeout:with-timeout (10)
          (initialize-mcp-server! server))
      (error (e)
        (log-warn "Failed to initialize MCP server ~a: ~a" name e)))
    server))

(defparameter *mcp-protocol-version* "2025-06-18"
  "The MCP protocol version this server supports.")

(defun initialize-mcp-server! (mcp-server)
  (let* ((client-initialization-info
           (object :capabilities (object :elicitation (object)
                                         :roots (object :list-changed jsonx:+json-true+)
                                         :sampling (object))
                   :client-info (object :display-name "SBCL Gemini Client"
                                        :name "SBCLGeminiClient"
                                        :version "1.1.0")
                   :protocol-version *mcp-protocol-version*))

         (server-initialization-info
           (prog1 (jsonrpc (jsonrpc-client mcp-server) "initialize" client-initialization-info nil)
             (chanl:send (outgoing-channel (jsonrpc-client mcp-server))
                         (object :jsonrpc "2.0"
                                 :method "notifications/initialized"))))

         (capabilities (get-capabilities server-initialization-info))

         (instructions (get-instructions server-initialization-info))

         (protocol-version (get-protocol-version server-initialization-info))

         (server-info (get-server-info server-initialization-info))
         (server-name    (get-name server-info))
         (server-title   (get-title server-info))
         (server-version (get-version server-info))

         (server-instructions (get-server-instructions server-initialization-info)))

    (log-info "MCP Server ~a, ~a" server-name server-version)
    (when server-title
      (log-info "  Title: ~a" server-title))
    (log-info "  Protocol Version: ~a" protocol-version)

    (setf (completions-capability mcp-server) (get-completions capabilities)
          (logging-capability     mcp-server) (get-logging     capabilities)
          (prompts-capability     mcp-server) (get-prompts     capabilities)
          (resources-capability   mcp-server) (get-resources   capabilities)
          (tools-capability       mcp-server) (get-tools       capabilities)
          (get-autonym mcp-server) server-name
          (get-instructions mcp-server) instructions
          (get-protocol-version mcp-server) protocol-version
          (get-title mcp-server)   server-title
          (get-version mcp-server) server-version
          (get-server-instructions mcp-server) server-instructions)
    (log-info "  Capabilities:")
    (when (completions-capability mcp-server)
      (log-info "    Completions"))
    (when (logging-capability mcp-server)
      (log-info "    Logging"))
    (when (prompts-capability mcp-server)
      (log-info "    Prompts")
      (setf (prompts-list-changed-capability mcp-server) (get-list-changed (get-prompts capabilities)))
      (when (prompts-list-changed-capability mcp-server)
        (log-info "      List Changed")))
    (when (resources-capability mcp-server)
      (log-info "    Resources")
      (setf (resources-list-changed-capability mcp-server) (get-list-changed (get-resources capabilities)))
      (when (resources-list-changed-capability mcp-server)
        (log-info "      List Changed"))
      (setf (resources-subscribe-capability mcp-server) (get-subscribe (get-resources capabilities)))
      (when (resources-subscribe-capability mcp-server)
        (log-info "      Subscribe")))
    (when (tools-capability mcp-server)
      (log-info "    Tools")
      (setf (tools-list-changed-capability mcp-server) (get-list-changed (get-tools capabilities))))

    (log-info "MCP Server ~a initialized." server-name)))

(defun handle-elicitation (server message)
  (log-debug "MCP elicitation request: ~s" (dehashify message))
  (let* ((params (get-params message))
         (msg (get-message params))
         (requested-schema (get-requested-schema params))
         ;;(type (get-type requested-schema))
         )
    (format *query-io* "~%MCP Server ~a elicits a response: ~a~%" (get-autonym server) msg)
    (finish-output *query-io*)
    (let ((response
            (let iter ((thing (cons :requested-schema requested-schema)))
              (let ((name (car thing))
                    (components (cdr thing)))
                (cons name
                      (cond ((equal (get-type components) "integer")
                             (let ((description (get-description components))
                                   (maximum (get-maximum components))
                                   (minimum (get-minimum components)))
                               (format *query-io* "~&~a (~d-~d): " description minimum maximum)
                               (finish-output *query-io*)
                               (parse-integer (read-line *query-io*))))
                            ((equal (get-type components) "string")
                             (let retry ((description (get-description components))
                                         (enum (get-enum components)))
                               (format *query-io* "~&~a~@[ (~(object~a~^ ~))~]: " description enum)
                               (finish-output *query-io*)
                               (let ((string (read-line *query-io*)))
                                 (if (and enum
                                          (not (member string enum :test #'equal)))
                                     (retry description enum)
                                     string))))
                            ((equal (get-type components) "object")
                             (map 'list #'iter (get-properties components)))
                            (t (error "Unrecognized schema"))))))))
      (chanl:send (outgoing-channel (jsonrpc-client server))
                  (object :jsonrpc "2.0"
                          :id (get-id message)
                          :result (object :action "accept"
                                          :content (cdr response)))))))

(defun handle-notification-message (server message)
  (let* ((params (get-params message))
         (level  (->keyword (get-level  params)))
         (data   (get-data   params)))
    (when (funcall (notification-filter server) level)
      (format (notification-stream server)
              "~&~a MCP ~:(~a~) Notification: ~a~%" (get-autonym server) level data)
      (finish-output (notification-stream server)))))

(defun handle-notification-progress (server message)
  (let* ((params (get-params message))
         (progress (get-progress params))
         (token    (get-progress-token params))
         (total    (get-total params)))
    (format (notification-stream server)
            "~&~s (~d/~d)~%" token progress total)
    (finish-output (notification-stream server))))

(defun handle-notification-resources-updated (server message)
  (let* ((params (get-params message))
         (uri    (get-uri    params))
         (handler (gethash uri (resource-subscriptions server))))
    (when handler
      (funcall handler uri))))

(defun handle-roots-list (server message)
  (chanl:send
   (outgoing-channel (jsonrpc-client server))
   (object :jsonrpc "2.0"
           :id (get-id message)
           :result (object :roots
                           (vector (object :uri "file:///"
                                           :name "Root")
                                   (object :uri "file:///home/jrm/"
                                           :name "Home"))))))

(defun handle-sampling-create-message (server message)
  (let* ((params (get-params message))
         (include-context (->keyword (get-include-context params)))
         (max-tokens (get-max-tokens params))
         (messages (get-messages params))
         ;; (_ignore (format t "~&~s~%" messages))
         (system-prompt (get-system-prompt params))
         (temperature (and (stringp (get-temperature params))
                           (parse-float-safely (get-temperature params))))
         (sample (let* ((session (make-runtime-session
                      :context (context server)
                      :prior-context (if include-context (context server) nil)
                      :conversation-topic (current-topic)))
                (*system-instruction*
                  (content
                   :parts (list (part system-prompt))))
                (*max-output-tokens* max-tokens)
                (*return-text-string* nil)
                (*temperature* temperature)
                (prompt (remove nil (map 'list (lambda (message)
                                 (let* ((role (get-role message))
                                    (content (get-content message))
                                    (type (get-type content)))
                                   (if (equal type "text")
                                     (content
                                    :role role
                                    :parts (list (part (get-text content)))))))
                             messages))))
               (let ((result (continue-gemini-with-session session prompt)))
               (if include-context
                 (setf (context server)
                     (append result (runtime-session-prior-context session)))
                 (setf (context server) result))
               result))))
    (log-debug "MCP sampling response: ~s" (dehashify sample))
    (chanl:send (outgoing-channel (jsonrpc-client server))
                (object :jsonrpc "2.0"
                        :id (get-id message)
                        :result 
                        (object
                         :role "assistant"
                         :content (object
                                   :type "text"
                                   :text (get-text (car (get-parts (get-content (car (get-candidates sample)))))))
                         :model (get-model-version sample)
                         :stop-reason "endTurn"
                         )))))

(defun start-mcp-servers ()
  (unless *mcp-servers*
    (let ((config (read-mcp-config)))
      (when config
        (dolist (server-config (get-mcp-servers config))
          (let ((name (car server-config)))
            (push (create-mcp-server name (cdr server-config)) *mcp-servers*))))))
  *mcp-servers*)

(defun stop-mcp-server (mcp-server)
  "Stops one MCP server process and associated JSONRPC transport.
Safe to call repeatedly."
  (when (and mcp-server (slot-boundp mcp-server 'jsonrpc-client))
    (let* ((client (jsonrpc-client mcp-server))
           (proc (and client (process-info client))))
      (when proc
        (handler-case
            (progn
              (when (uiop:process-alive-p proc)
                (uiop:terminate-process proc :urgent t)
                (uiop:wait-process proc))
              (uiop:close-streams proc))
          (error (e)
            (log-warn "Failed to stop MCP server ~a: ~a"
                      (get-name mcp-server)
                      e))))))
  nil)

(defun stop-mcp-servers ()
  "Stops all MCP servers and clears the global registry. Safe to call repeatedly."
  (when *mcp-servers*
    (dolist (server *mcp-servers*)
      (stop-mcp-server server))
    (setf *mcp-servers* nil))
  nil)

(defun restart-mcp-servers ()
  (stop-mcp-servers)
  (start-mcp-servers))

(defun memory-config (memory-file)
  (let ((config (read-mcp-config)))
    (when config
      (let ((memory-config (cdr (assoc "memory" (cdr (assoc :mcp-servers config)) :test #'equal))))
        (when memory-config
          `(,(assoc :command memory-config)
            (:ARGS ,@(append (butlast (cdr (assoc :args memory-config)))
                             (list (namestring memory-file))))
            (:ENV ,@(map 'list (lambda (env-var)
                                (cond ((equal (car env-var) "MEMORY_FILE_PATH")
                                       (cons "MEMORY_FILE_PATH" (namestring memory-file)))
                                      (t env-var)))
                        (cdr (assoc :env memory-config))))
            ,(assoc :system-instruction memory-config)))))))

(defun memory-mcp-server (memory-file)
  "Create an MCP server instance for the memory server."
  (let ((config (memory-config memory-file)))
    (when config
      (create-mcp-server "Memory" config))))

(defun find-tool (mcp-server tool-name)
  "Find a tool by name in the MCP server."
  (find tool-name (get-tools mcp-server) :test #'equal :key #'get-name))

(defun call-tool (mcp-server tool params)
  (let ((progress-token (format nil "~aProgress-~a" (get-name tool) (random-id 8))))
    (jsonrpc (jsonrpc-client mcp-server)
             "tools/call"
             (object :_meta (object :progress-token progress-token)
                     :name (get-name tool)
                     :arguments (or params
                                    jsonx:+json-empty-object+))
             progress-token)))

(defun test-tools ()
  (let ((server (find-mcp-server "Everything")))
    (print (call-tool server (find-tool server "echo") (object :message "Hello, MCP!")))
    (finish-output)
    (print (call-tool server (find-tool server "add") (object :a 42 :b 69 )))
    (finish-output)
    ;; (print (call-tool server (find-tool server "longRunningOperation") (object :duration 15 :steps 10)))
    ;; (finish-output)
    ;; (print (call-tool server (find-tool server "printEnv")))
    ;; (finish-output)
    (print (call-tool server (find-tool server "sampleLLM") (object :prompt "Magic Eight Ball phrases")))
    (finish-output)
    ;; (print (call-tool server (find-tool server "startElicitation") +json-empty-object+))
    ;; (finish-output)
    ))


(defun convert-property (prop)
  (cons (if (keywordp (car prop))
            (car prop)
            (keystring->keyword (car prop)))
        (encode-schema-type (cdr prop))))

(defun convert-input-schema (input-schema)
  (object :type (get-type input-schema)
          :properties (alist-hash-table (map 'list #'convert-property (hash-table-alist (get-properties input-schema))))
          :required (or (get-required input-schema)
                        #())))
                              
(defun transform-description (input-string)
  "Transforms the description string by looking for _ characters, uppercasing the next character, and removing the _."
  (with-output-to-string (output-string)
    (with-input-from-string (str input-string)
      (loop for char = (read-char str nil)
            while char
            do (if (char= char #\_)
                   (let ((next-char (read-char str nil)))
                     (when next-char
                       (write-char (char-upcase next-char) output-string)))
                   (write-char char output-string))))))

(defun convert-tool (mcp-server tool)
  "Converts an MCP tool to a function specification."
  (let ((input-schema (convert-input-schema (get-input-schema tool)))
        (output-schema (get-output-schema tool)))

    (let ((fd (function-declaration
               :name (format nil "~a" (get-name tool))
               :description (transform-description (get-description tool))
               :behavior :blocking
               :parameters (encode-schema-type input-schema)
               :response (encode-schema-type output-schema))))
      ;; (format *trace-output* "~&;; Converted MCP tool: ~a~%" (dehashify fd))
      ;; (finish-output *trace-output*)
      (cons fd
            (lambda (&rest args &key &allow-other-keys)
              ;; (format *trace-output* "~&;; Calling MCP tool: ~a with args: ~a~%" (get-name tool) args)
              ;; (format *trace-output* "~&;; required: ~a~%" (get-required input-schema))
              ;; (finish-output *trace-output*)
              (call-tool mcp-server tool (plist-hash-table args)))))))

(defun get-mcp-functions-and-handlers (mcp-server)
  "Returns the functions and handlers for the given MCP server."
  (when (and mcp-server
             (mcp-server-alive? mcp-server)
             (tools-capability mcp-server))
    (map 'list (lambda (tool) (convert-tool mcp-server tool)) (get-tools mcp-server))))

(defun content-generator-mcp-servers (content-generator)
  "Returns the lazily-started MCP servers applicable to CONTENT-GENERATOR."
  (when (get-enable-mcp-tools (get-config content-generator))
    (let* ((shared-servers (or (start-mcp-servers) *mcp-servers*))
           (memory-server (get-memory-mcp-server content-generator))
           (global-memory-server (find "memory" shared-servers :key #'get-name :test #'equal)))
      (remove nil
             (cons memory-server
                   (remove global-memory-server shared-servers))))))

(defun mcp-functions-and-handlers (content-generator)
  "Extracts the list of functions supplied by the MCP servers."
  (fold-left (binary-compose-right #'append #'get-mcp-functions-and-handlers) nil
             (content-generator-mcp-servers content-generator)))
