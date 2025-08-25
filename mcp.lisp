;;; -*- Lisp -*-

(in-package "GEMINI")

(defun mcp-config-pathname ()
  (uiop/configuration:xdg-config-pathname "mcp/mcp.lisp"))

(defun read-mcp-config ()
  (with-open-file (stream (mcp-config-pathname) :direction :input :if-does-not-exist nil)
    (read stream)))

(defclass mcp-client ()
  ((capability/completions :initarg nil :accessor has-completions-capability?)
   (capability/elicitation :initarg nil :accessor has-elicitation-capability?)
   (capability/logging     :initarg nil :accessor has-logging-capability?)
   (capability/prompts     :initarg nil :accessor has-prompts-capability?)
   (capability/resources   :initarg nil :accessor has-resources-capability?)
   (capability/resources/subscribe
    :initarg nil :accessor has-resources-subscribe-capability?)
   (capability/tools       :initarg nil :accessor has-tools-capability?)

   (autonym          :accessor get-autonym)
   (instructions     :accessor get-instructions)
   (protocol-version :accessor get-protocol-version)
   (title            :accessor get-title)
   (version          :accessor get-version)

   (config :initarg :config :reader config)
   (context :initarg nil :accessor context)
   (delayed-prompts   :initarg :delayed-prompts   :reader delayed-prompts)
   (delayed-resources :initarg :delayed-resources :reader delayed-resources)
   (delayed-resource-templates :initarg :delayed-resource-templates :reader delayed-resource-templates)
   (delayed-tools     :initarg :delayed-tools     :reader delayed-tools)
   (jsonrpc-client    :accessor jsonrpc-client)
   (mutex :initform (bordeaux-threads:make-lock) :reader mutex)
   (name  :initarg :name :reader get-name)
   (notification-filter :initform (constantly t) :accessor notification-filter)
   (notification-stream :initform *trace-output* :accessor notification-stream)
   (resource-subscriptions :initform (make-hash-table :test #'equal) :reader resource-subscriptions)
   ))

(defun call-with-mcp-client-mutex (mcp-client thunk)
  (bordeaux-threads:with-lock-held ((mutex mcp-client))
    (funcall thunk)))

(defmacro with-mcp-client-mutex ((mcp-client) &body body)
  `(CALL-WITH-MCP-CLIENT-MUTEX ,mcp-client (LAMBDA () ,@body)))

(defmethod get-prompts ((object mcp-client))
  (force (delayed-prompts object)))

(defmethod get-prompt ((object mcp-client) name)
  (jsonrpc (jsonrpc-client object) "prompts/get" (object :name name)))

(defmethod read-resource ((object mcp-client) uri)
  (let ((contents (get-contents (jsonrpc (jsonrpc-client object) "resources/read" (object :uri uri)))))
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

(defmethod get-resources ((object mcp-client))
  (force (delayed-resources object)))

(defmethod get-resource-templates ((object mcp-client))
  (force (delayed-resource-templates object)))

(defun find-resource-template (mcp-client name)
  (find name (get-resource-templates mcp-client) :test #'equal :key #'get-name))

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
              (error "Unbalance curly braces in ~s." resource-template)
              (let ((before  (subseq resource-template 0  open-curly))
                    (between (->keyword (subseq resource-template (1+ open-curly) close-curly)))
                    (after   (subseq resource-template (1+ close-curly) (length resource-template))))
                (format nil "~a~a~a"
                        before
                        (or (getf substitution-plist between)
                            (error "No substitution for {~a}" between))
                        after)))))))

(defun read-resource-by-template (mcp-client template-name &rest substitution-plist)
  "Read a resource by its template name, substituting args into the template."
  (let ((template (get-uri-template (find-resource-template mcp-client template-name))))
    (if (null template)
        (error "No resource template named ~a" template-name)
        (let ((expanded-template (expand-resource-template template substitution-plist)))
          (read-resource mcp-client expanded-template)))))

(defun subscribe-to-resource (mcp-client resource-uri receiver)
  "Subscribe to a resource URI in the MCP client."
  (setf (gethash resource-uri (resource-subscriptions mcp-client)) receiver)
  (jsonrpc (jsonrpc-client mcp-client) "resources/subscribe" (object :uri resource-uri)))

(defmethod get-system-instruction ((object mcp-client))
  (get-system-instruction (config object)))

(defmethod get-tools ((object mcp-client))
  (force (delayed-tools object)))

(defmethod print-object ((client mcp-client) stream)
  (print-unreadable-object (client stream :type t)
    (format stream "~a" (get-name client))))

(defun find-mcp-client (name)
  (find name *mcp-clients* :key #'get-name :test #'equal))

(defun create-mcp-client (name config)
  (letrec ((client
            (make-instance
             'mcp-client
             :config config
             :delayed-prompts   (delay
                                 (when (has-prompts-capability? client)
                                   (get-prompts (jsonrpc jsonrpc-clnt "prompts/list" '()))))
             :delayed-resources (delay
                                 (when (has-resources-capability? client)
                                   (get-resources (jsonrpc jsonrpc-clnt "resources/list" '()))))
             :delayed-resource-templates (delay
                                          (when (has-resources-capability? client)
                                            (get-resource-templates
                                             (jsonrpc jsonrpc-clnt "resources/templates/list" '()))))
             :delayed-tools     (delay
                                 (when (has-tools-capability? client)
                                   (get-tools (jsonrpc jsonrpc-clnt "tools/list" '()))))
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
                        (handle-elicitation client message))

                       ((and (equal (get-method message) "notifications/message")
                             (notification-stream client))
                        (handle-notification-message client message))

                       ((and (equal (get-method message) "notifications/progress")
                             (notification-stream client))
                        (handle-notification-progress client message))

                       ((equal (get-method message) "notifications/resources/updated")
                        (handle-notification-resources-updated client message))

                       ((equal (get-method message) "roots/list")
                        (handle-roots-list client message))

                       ((equal (get-method message) "sampling/createMessage")
                        (handle-sampling-create-message client message))

                       (t (format *trace-output* "~&Unexpected MCP message: ~s~%" (dehashify message))
                          (finish-output *trace-output*))))))))

    (setf (jsonrpc-client client) jsonrpc-clnt)
    (initialize-mcp-client! client)
    client))

(defun initialize-mcp-client! (mcp-client)
  (let* ((client-initialization-info
           (object :capabilities (object :elicitation (object)
                                         :roots (object :list-changed +json-true+)
                                         :sampling (object))
                   :client-info (object :display-name "SBCL Gemini Client"
                                        :name "SBCLGeminiClient"
                                        :version "1.0.0")
                   :protocol-version "2024-11-05"))

         (server-initialization-info
           (prog1 (jsonrpc (jsonrpc-client mcp-client) "initialize" client-initialization-info)
             (chanl:send (outgoing-channel (jsonrpc-client mcp-client))
                         (object :jsonrpc "2.0"
                                 :method "notifications/initialized"))))

         (capabilities (get-capabilities server-initialization-info))
         (capabilities-keys (keys capabilities))

         (instructions (get-instructions server-initialization-info))

         (protocol-version (get-protocol-version server-initialization-info))

         (server-info (get-server-info server-initialization-info))
         (server-name    (get-name server-info))
         (server-title   (get-title server-info))
         (server-version (get-version server-info)))

    (setf (has-completions-capability? mcp-client) (->boolean (member :completions capabilities-keys))
          (has-elicitation-capability? mcp-client) (->boolean (member :elicitation capabilities-keys))
          (has-logging-capability?     mcp-client) (->boolean (member :logging     capabilities-keys))
          (has-prompts-capability?     mcp-client) (->boolean (member :prompts     capabilities-keys))
          (has-resources-capability?   mcp-client) (->boolean (member :resources   capabilities-keys))
          (has-tools-capability?       mcp-client) (->boolean (member :tools       capabilities-keys))
          (get-autonym mcp-client) server-name
          (get-instructions mcp-client) instructions
          (get-protocol-version mcp-client) protocol-version
          (get-title mcp-client)   server-title
          (get-version mcp-client) server-version)
    (when (has-resources-capability? mcp-client)
      (setf (has-resources-subscribe-capability? mcp-client) (get-subscribe (get-resources capabilities))))
         
    (format t "~&MCP Client ~a initialized.~%" server-name)))

(defun handle-elicitation (client message)
  (format t "~&~s~%" message)
  (finish-output)
  (let* ((params (get-params message))
         (msg (get-message params))
         (requested-schema (get-requested-schema params))
         ;;(type (get-type requested-schema))
         )
    (format *query-io* "~%MCP Server ~a elicits a response: ~a~%" (get-autonym client) msg)
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
      (chanl:send (outgoing-channel (jsonrpc-client client))
                  (object :jsonrpc "2.0"
                          :id (get-id message)
                          :result (object :action "accept"
                                          :content (cdr response)))))))

(defun handle-notification-message (client message)
  (let* ((params (get-params message))
         (level  (->keyword (get-level  params)))
         (data   (get-data   params)))
    (when (funcall (notification-filter client) level)
      (format (notification-stream client)
              "~&~a MCP ~:(~a~) Notification: ~a~%" (get-autonym client) level data)
      (finish-output (notification-stream client)))))

(defun handle-notification-progress (client message)
  (let* ((params (get-params message))
         (progress (get-progress params))
         (token    (get-progress-token params))
         (total    (get-total params)))
    (format (notification-stream client)
            "~&~s (~d/~d)~%" token progress total)
    (finish-output (notification-stream client))))

(defun handle-notification-resources-updated (client message)
  (let* ((params (get-params message))
         (uri    (get-uri    params))
         (handler (gethash uri (resource-subscriptions client))))
    (when handler
      (funcall handler uri))))

(defun handle-roots-list (client message)
  (chanl:send
   (outgoing-channel (jsonrpc-client client))
   (object :jsonrpc "2.0"
           :id (get-id message)
           :result (object :roots
                           (vector (object :uri "file:///home/jrm/gemini"
                                           :name "Gemini"))))))

(defun handle-sampling-create-message (client message)
  (let* ((params (get-params message))
         (include-context (->keyword (get-include-context params)))
         (max-tokens (get-max-tokens params))
         (messages (get-messages params))
         ;; (_ignore (format t "~&~s~%" messages))
         (system-prompt (get-system-prompt params))
         (temperature (and (stringp (get-temperature params))
                           (read-from-string (get-temperature params))))
         (sample (let ((*system-instruction*
                         (content
                          :parts (list (part system-prompt))))
                       (*max-output-tokens* max-tokens)
                       (*return-text-string* nil)
                       (*history* (context client))
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
                   (if include-context
                       (setf *prior-history* (context client))
                       (setf *prior-history* nil))
                   (let ((result (continue-gemini prompt)))
                     (if include-context
                         (setf (context client) (append result *prior-history*))
                         (setf (context client) result))
                     result))))
    (format t "~&sample: ~s~%" (dehashify sample))
    (chanl:send (outgoing-channel (jsonrpc-client client))
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


(defun start-mcp-clients ()
  (unless *mcp-clients*
    (let ((config (read-mcp-config)))
      (when config
        (dolist (client-config (get-mcp-servers config))
          (let ((name (car client-config)))
            (push (create-mcp-client name (cdr client-config)) *mcp-clients*)))))))

(eval-when (:load-toplevel :execute)
  (start-mcp-clients))

(defun find-tool (mcp-client tool-name)
  "Find a tool by name in the MCP client."
  (find tool-name (get-tools mcp-client) :test #'equal :key #'get-name))

(defun call-tool (mcp-client tool params)
  (jsonrpc (jsonrpc-client mcp-client)
           "tools/call" (object :_meta (object :progress-token (format nil "~aProgress" (get-name tool)) )
                                :name (get-name tool)
                                :arguments (or params
                                               +json-empty-object+)
                                )))

(defun test-tools ()
  (let ((client (find-mcp-client "Everything")))
    (print (call-tool client (find-tool client "echo") (object :message "Hello, MCP!")))
    (finish-output)
    (print (call-tool client (find-tool client "add") (object :a 42 :b 69 )))
    (finish-output)
    ;; (print (call-tool client (find-tool client "longRunningOperation") (object :duration 15 :steps 10)))
    ;; (finish-output)
    ;; (print (call-tool client (find-tool client "printEnv")))
    ;; (finish-output)
    (print (call-tool client (find-tool client "sampleLLM") (object :prompt "Magic Eight Ball phrases")))
    (finish-output)
    ;; (print (call-tool client (find-tool client "startElicitation") +json-empty-object+))
    ;; (finish-output)
    ))
