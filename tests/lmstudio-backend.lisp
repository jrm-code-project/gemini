;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite lmstudio-backend-tests)

(test test-lmstudio-backend-request-and-reconstruction
  "Verify the LM Studio backend builds /api/v1/chat payloads and reconstructs a non-streaming result when no callback is supplied."
  (let ((session (gemini:make-runtime-session))
       (orig-post #'gemini::lmstudio-post))
    (gemini:with-runtime-session (session)
     (setf (gemini:runtime-session-interaction-id session) "resp_prev")
     (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-post)
                  (lambda (uri payload &key verbose read-timeout connect-timeout)
                    (declare (ignore verbose read-timeout connect-timeout))
                    (is (equal "http://localhost:1234/api/v1/chat" uri))
                    (is (equal "model-a" (gemini::adapter-field payload "model" :model)))
                    (is (eq jsonx:+json-false+ (gemini::adapter-field payload "stream" :stream)))
                    (is (eq jsonx:+json-true+ (gemini::adapter-field payload "store" :store)))
                    (is (equal "resp_prev" (gemini::adapter-field payload "previous_response_id" :previous_response_id)))
                    (is (equal "be terse" (gemini::adapter-field payload "system_prompt" :system_prompt)))
                    (is (= 0.2 (gemini::adapter-field payload "temperature" :temperature)))
                    (is (= 42 (gemini::adapter-field payload "max_output_tokens" :max_output_tokens)))
                    (is (equal "Hello LM Studio"
                               (gemini::adapter-field payload "input" :input)))
                    (gemini::object
                     :model_instance_id "openai/gpt-oss-20b"
                     :response_id "resp_next"
                     :output (vector (gemini::object :type "message" :content "STREAM_OK"))
                     :stats (gemini::object :input_tokens 10
                                            :total_output_tokens 4
                                            :reasoning_output_tokens 0))))
            (let* ((backend (make-instance 'gemini::lmstudio-backend :url "http://localhost:1234/api/v1/chat"))
                   (payload (gemini::object
                             :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                             :system-instruction (gemini::content :role "system" :parts (list (part "be terse")))
                             :generation-config (gemini::object :temperature 0.2 :max-output-tokens 42))))
              (multiple-value-bind (response usage)
                  (gemini:invoke-backend backend "model-a" payload)
                (is (equal "STREAM_OK"
                           (gemini:get-text
                            (first (gemini:get-parts
                                    (gemini:get-content (first (gemini:get-candidates response))))))))
                (is (equal "resp_next" (gemini::get-response-id response)))
                (is (equal "resp_next" (gemini:runtime-session-interaction-id session)))
                (is (= 10 (gemini:get-prompt-token-count usage)))
                (is (= 4 (gemini:get-candidates-token-count usage))))))
       (setf (fdefinition 'gemini::lmstudio-post) orig-post))))

(test test-lmstudio-backend-errors-on-empty-nonstream-output
  "Verify the LM Studio backend signals when a non-streaming result contains no usable output parts."
  (let ((orig-post #'gemini::lmstudio-post))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::lmstudio-post)
                (lambda (uri payload &key verbose read-timeout connect-timeout)
                  (declare (ignore uri payload verbose read-timeout connect-timeout))
                  (gemini::object
                   :model_instance_id "qwen/qwen3.6-27b"
                   :response_id "resp_empty"
                   :output (vector))))
          (let* ((backend (make-instance 'gemini::lmstudio-backend :url "http://localhost:1234/api/v1/chat"))
                 (payload (gemini::object
                           :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio")))))))
            (handler-case
                (progn
                  (gemini:invoke-backend backend "model-a" payload)
                  (fail "Expected empty non-stream output to signal an error."))
              (error (e)
                (is (search "contained no result parts" (princ-to-string e)))))))
      (setf (fdefinition 'gemini::lmstudio-post) orig-post))))

(test test-lmstudio-backend-aborts-stream-after-chat-end
  "Verify LM Studio backend requests stream teardown as soon as chat.end arrives."
  (let ((orig-post-stream #'gemini::lmstudio-post-streaming))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::lmstudio-post-streaming)
                (lambda (uri payload receiver &key verbose read-timeout connect-timeout)
                  (declare (ignore uri payload verbose read-timeout connect-timeout))
                  (let ((socket (make-instance 'gemini::stateful-sse-socket
                                               :read-timeout 60
                                               :receiver receiver)))
                    (let ((gemini::*current-sse-socket* socket))
                      (funcall receiver
                               (gemini::object
                                :type "chat.end"
                                :result (gemini::object
                                         :model_instance_id "qwen/qwen3.6-27b"
                                         :response_id "resp_abort_after_end"
                                         :output (vector (gemini::object :type "message"
                                                                         :content "OK"))
                                         :stats (gemini::object :input_tokens 1
                                                                :total_output_tokens 1
                                                                :reasoning_output_tokens 0))))
                      (is (gemini::sse-socket-abort-requested-p socket))))))
          (let ((backend (make-instance 'gemini::lmstudio-backend :url "http://localhost:1234/api/v1/chat"))
                (payload (gemini::object
                          :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio")))))))
            (multiple-value-bind (response usage)
                (gemini:invoke-backend backend "model-a" payload
                                       :receiver (lambda (event-type parsed-data raw-event)
                                                   (declare (ignore event-type parsed-data raw-event))))
              (is (null response))
              (is (null usage)))))
      (setf (fdefinition 'gemini::lmstudio-post-streaming) orig-post-stream))))

(test test-lmstudio-backend-surfaces-error-events-when-chat-end-is-empty
  "Verify LM Studio error events are surfaced directly when chat.end carries no usable output."
  (let ((orig-post-stream #'gemini::lmstudio-post-streaming))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::lmstudio-post-streaming)
              (lambda (uri payload receiver &key verbose read-timeout connect-timeout)
                (declare (ignore uri payload verbose read-timeout connect-timeout))
                (funcall receiver
                         (gemini::object :type "error"
                                         :error (gemini::object
                                                 :message "Unable to connect to remote MCP server at a non-public address."
                                                 :type "mcp_connection_error")))
                (funcall receiver
                         (gemini::object :type "chat.end"
                                         :result (gemini::object
                                                  :model_instance_id "qwen/qwen3.6-27b"
                                                  :output (vector)
                                                  :stats (gemini::object :input_tokens 0
                                                                         :total_output_tokens 0
                                                                         :reasoning_output_tokens 0)
                                                  :response_id "resp_error")))))
          (handler-case
              (progn
              (gemini:invoke-backend
               (make-instance 'gemini::lmstudio-backend :url "http://localhost:1234/api/v1/chat")
               "qwen/qwen3.6-27b"
               (gemini::object :input "Hello LM Studio"))
              (fail "Expected LM Studio backend to surface the stream error."))
            (error (e)
              (is (search "Unable to connect to remote MCP server" (princ-to-string e))))))
      (setf (fdefinition 'gemini::lmstudio-post-streaming) orig-post-stream))))

(test test-lmstudio-backend-forwards-tool-call-stream-events
  "Verify the LM Studio backend forwards tool_call.* events to a receiver and still updates the session response id."
  (let ((session (gemini:make-runtime-session))
        (events '())
        (orig-post-stream #'gemini::lmstudio-post-streaming))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::lmstudio-post-streaming)
                   (lambda (uri payload receiver &key verbose read-timeout connect-timeout)
                     (declare (ignore uri payload verbose read-timeout connect-timeout))
                     (funcall receiver
                              (gemini::object :type "tool_call.start"
                                              :tool "machineType"
                                              :provider_info (gemini::object :type "ephemeral_mcp"
                                                                             :server_label "gemini-tools")))
                     (funcall receiver
                              (gemini::object :type "tool_call.arguments"
                                              :tool "machineType"
                                              :arguments (gemini::object :detail "full")
                                              :provider_info (gemini::object :type "ephemeral_mcp"
                                                                             :server_label "gemini-tools")))
                     (funcall receiver
                              (gemini::object :type "tool_call.success"
                                              :tool "machineType"
                                              :arguments (gemini::object :detail "full")
                                              :output "[{\"type\":\"text\",\"text\":\"x86_64\"}]"
                                              :provider_info (gemini::object :type "ephemeral_mcp"
                                                                             :server_label "gemini-tools")))
                     (funcall receiver
                              (gemini::object :type "message.delta" :content "STREAM_OK"))
                     (funcall receiver
                              (gemini::object :type "chat.end"
                                              :result (gemini::object
                                                       :model_instance_id "qwen/qwen3.6-27b"
                                                       :response_id "resp_tool_stream"
                                                       :output (vector
                                                                (gemini::object :type "tool_call"
                                                                                :tool "machineType"
                                                                                :arguments (gemini::object :detail "full")
                                                                                :output "[{\"type\":\"text\",\"text\":\"x86_64\"}]")
                                                                (gemini::object :type "message"
                                                                                :content "STREAM_OK"))
                                                       :stats (gemini::object :input_tokens 7
                                                                              :total_output_tokens 5
                                                                              :reasoning_output_tokens 0))))
                     nil))
             (let* ((backend (make-instance 'gemini::lmstudio-backend :url "http://localhost:1234/api/v1/chat"))
                    (payload (gemini::object
                              :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio")))))))
               (gemini:invoke-backend
                backend "model-a" payload
                :receiver (lambda (event-type parsed-data raw-event)
                            (declare (ignore raw-event))
                            (push (list event-type parsed-data) events)))
               (let ((rev-events (nreverse events)))
                 (is (= 5 (length rev-events)))
                 (is (eq :tool-call-start (first (first rev-events))))
                 (is (eq :tool-call-arguments (first (second rev-events))))
                 (is (eq :tool-call-success (first (third rev-events))))
                 (is (eq :message-delta (first (fourth rev-events))))
                 (is (equal "STREAM_OK" (second (fourth rev-events))))
                 (is (eq :chat-end (first (fifth rev-events))))
                 (is (equal "machineType"
                            (gemini::adapter-field (second (first rev-events)) "tool" :tool)))
                 (is (equal "resp_tool_stream"
                            (gemini:runtime-session-interaction-id session)))))))
        (setf (fdefinition 'gemini::lmstudio-post-streaming) orig-post-stream)))))

(test test-lmstudio-backend-translates-gemini-tools-to-ephemeral-mcp-integration
  "Verify supported Gemini tool declarations become LM Studio request tools plus an ephemeral_mcp integration."
  (let ((session (gemini:make-runtime-session))
       (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () "http://bridge.test/mcp"))
            (let* ((payload (gemini::object
                             :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                             :tools (vector
                                     (gemini::object
                                      :function-declarations
                                      (vector (gemini::function-declaration
                                               :name "machineType"
                                               :description "returns machine type"
                                               :parameters (gemini::schema
                                                            :type :object
                                                            :properties (gemini::object :detail (gemini::schema :type :string))
                                                            :required (vector :detail))))))))
                   (lmstudio-payload (gemini::build-lmstudio-payload "model-a" payload session))
                   (tools (coerce (gemini::adapter-field lmstudio-payload "tools" :tools) 'list))
                   (tool (first tools))
                   (integrations (coerce (gemini::adapter-field lmstudio-payload "integrations" :integrations) 'list))
                   (integration (first integrations))
                   (allowed-tools (coerce (gemini::adapter-field integration "allowed_tools" :allowed_tools) 'list)))
              (is (= 1 (length tools)))
              (is (equal "function" (gemini::adapter-field tool "type" :type)))
              (is (equal "machineType" (gemini::adapter-field tool "name" :name)))
              (is (equal "auto" (gemini::adapter-field lmstudio-payload "tool_choice" :tool_choice)))
              (is (= 1 (length integrations)))
              (is (equal "ephemeral_mcp" (gemini::adapter-field integration "type" :type)))
              (is (equal "gemini-tools" (gemini::adapter-field integration "server_label" :server_label)))
              (is (equal "http://bridge.test/mcp" (gemini::adapter-field integration "server_url" :server_url)))
              (is (equal '("machineType") allowed-tools))))
       (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)))))

(test test-lmstudio-backend-registers-bridge-session-headers-for-content-generator
  "Verify LM Studio tool translation auto-starts and registers a local bridge session when a content-generator is available."
  (let ((session (gemini:make-runtime-session))
       (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
         (progn
           (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                 (lambda () nil))
           (gemini:stop-lmstudio-tool-bridge)
           (let* ((config (make-instance 'gemini::persona-config
                                         :name "lmstudio-bridge-test"
                                         :googleapi :lmstudio-api
                                         :enable-misc-tools t
                                         :model "qwen/qwen3.6-27b"
                                         :url "http://127.0.0.1:1234/api/v1/chat"))
                  (generator (make-instance 'gemini::content-generator :config config))
                  (payload (gemini::object
                            :contents (list (gemini::content :role "user"
                                                              :parts (list (part "Hello LM Studio"))))
                            :tools (vector
                                    (gemini::object
                                     :function-declarations
                                     (vector
                                      (gemini::function-declaration
                                       :name "machineType"
                                       :description "returns machine type"
                                       :parameters (gemini::schema :type :object)))))))
                  (lmstudio-payload (gemini::build-lmstudio-payload
                                     "model-a" payload session
                                     :content-generator generator))
                  (integration (first (coerce (gemini::adapter-field lmstudio-payload
                                                                      "integrations"
                                                                      :integrations)
                                              'list)))
                  (headers (gemini::adapter-field integration "headers" :headers))
                  (token (and headers
                              (gemini::adapter-field headers
                                                     "X-Gemini-Bridge-Session"
                                                     "x-gemini-bridge-session"))))
             (is (not (null headers)))
             (is (stringp token))
             (is (> (length token) 0))
             (is (stringp (gemini:active-lmstudio-tool-bridge-url))))))
       (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)
       (gemini:stop-lmstudio-tool-bridge))))

(test test-lmstudio-tool-bridge-serves-registered-tools-via-jsonrpc
  "Verify the LM Studio MCP bridge serves initialize, tools/list, and tools/call for registered tools."
  (let ((session (gemini:make-runtime-session))
        (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () nil))
            (gemini:stop-lmstudio-tool-bridge)
            (let* ((config (make-instance 'gemini::persona-config
                                          :name "lmstudio-bridge-http-test"
                                          :googleapi :lmstudio-api
                                          :enable-misc-tools t
                                          :model "qwen/qwen3.6-27b"
                                          :url "http://127.0.0.1:1234/api/v1/chat"))
                   (generator (make-instance 'gemini::content-generator :config config))
                   (payload (gemini::object
                             :contents (list (gemini::content :role "user"
                                                              :parts (list (part "Hello LM Studio"))))
                             :tools (vector
                                     (gemini::object
                                      :function-declarations
                                      (vector (gemini::function-declaration
                                               :name "machineType"
                                               :description "returns machine type"
                                               :parameters (gemini::schema :type :object)))))))
                   (lmstudio-payload (gemini::build-lmstudio-payload
                                      "model-a" payload session :content-generator generator))
                   (integration (first (coerce (gemini::adapter-field lmstudio-payload
                                                                       "integrations"
                                                                       :integrations)
                                               'list)))
                   (bridge-headers (gemini::adapter-field integration "headers" :headers))
                   (request-headers (make-hash-table :test #'equal)))
              (maphash (lambda (key value)
                         (setf (gethash (string-downcase key) request-headers) value))
                       bridge-headers)
              (flet ((bridge-call (body)
                       (multiple-value-bind (status response)
                           (gemini::lmstudio-tool-bridge-handle-jsonrpc request-headers body)
                         (is (= 200 status))
                         response)))
                (let* ((initialize-response
                         (bridge-call
                          (gemini::object :jsonrpc "2.0"
                                          :id 1
                                          :method "initialize"
                                          :params (gemini::object
                                                   :protocol-version "2025-06-18"
                                                   :capabilities (gemini::object :tools (gemini::object))
                                                   :client-info (gemini::object
                                                                 :name "lmstudio-test"
                                                                 :version "0.1.0")))))
                       (initialize-result (gemini::adapter-field initialize-response "result" :result))
                       (tools-response
                         (bridge-call
                          (gemini::object :jsonrpc "2.0"
                                          :id 2
                                          :method "tools/list"
                                          :params (gemini::object))))
                       (tools-result (gemini::adapter-field tools-response "result" :result))
                       (tools (coerce (gemini::adapter-field tools-result "tools" :tools) 'list))
                       (tool (first tools))
                       (call-response
                         (bridge-call
                          (gemini::object :jsonrpc "2.0"
                                          :id 3
                                          :method "tools/call"
                                          :params (gemini::object
                                                   :name "machineType"
                                                   :arguments (gemini::object)))))
                       (call-result (gemini::adapter-field call-response "result" :result))
                       (content (coerce (gemini::adapter-field call-result "content" :content) 'list))
                       (text-item (first content)))
                  (is (equal "2025-06-18"
                             (gemini::adapter-field initialize-result "protocolVersion" :protocol-version)))
                  (is (= 1 (length tools)))
                  (is (equal "machineType"
                             (gemini::adapter-field tool "name" :name)))
                  (is (equal (machine-type)
                             (gemini::adapter-field text-item "text" :text)))))))
        (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)
        (gemini:stop-lmstudio-tool-bridge)))))

(test test-lmstudio-tool-bridge-errors-when-local-handler-missing
  "Verify LM Studio bridge registration fails clearly when the declared tool has no local handler."
  (let ((session (gemini:make-runtime-session))
       (bridge nil)
       (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf bridge (gemini:start-lmstudio-tool-bridge))
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () (gemini:active-lmstudio-tool-bridge-url)))
            (let* ((config (make-instance 'gemini::persona-config
                                          :name "lmstudio-bridge-missing-handler"
                                          :googleapi :lmstudio-api
                                          :enable-misc-tools t
                                          :model "qwen/qwen3.6-27b"
                                          :url "http://127.0.0.1:1234/api/v1/chat"))
                   (generator (make-instance 'gemini::content-generator :config config))
                   (payload (gemini::object
                             :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                             :tools (vector
                                     (gemini::object
                                      :function-declarations
                                      (vector (gemini::function-declaration
                                               :name "noHandler"
                                               :description "missing handler"
                                               :parameters (gemini::schema :type :object))))))))
              (handler-case
                  (progn
                    (gemini::build-lmstudio-payload "model-a" payload session :content-generator generator)
                    (fail "Expected LM Studio bridge registration to reject tools without handlers."))
                (error (e)
                  (is (search "has no handler" (princ-to-string e)))))))
       (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)
       (when bridge
         (gemini:stop-lmstudio-tool-bridge))))))

(test test-lmstudio-backend-errors-when-tool-bridge-url-missing
  "Verify payload-only LM Studio tool translation still fails clearly when no MCP bridge URL is configured."
  (let ((session (gemini:make-runtime-session))
        (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () nil))
            (gemini:stop-lmstudio-tool-bridge)
            (let ((payload (gemini::object
                            :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                            :tools (vector
                                    (gemini::object
                                     :function-declarations
                                     (vector (gemini::function-declaration
                                              :name "machineType"
                                              :description "returns machine type"
                                              :parameters (gemini::schema :type :object))))))))
              (handler-case
                  (progn
                    (gemini::build-lmstudio-payload "model-a" payload session)
                    (fail "Expected LM Studio tool translation to require a bridge URL."))
                (error (e)
                  (is (search "GEMINI_LMSTUDIO_TOOL_BRIDGE_URL" (princ-to-string e)))
                  (is (search "active local LM Studio tool bridge" (princ-to-string e)))))))
        (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)
        (gemini:stop-lmstudio-tool-bridge)))))

(test test-lmstudio-backend-rejects-unsupported-tool-schema
  "Verify LM Studio tool translation rejects schema features outside the supported subset."
  (let ((session (gemini:make-runtime-session))
       (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () "http://bridge.test/mcp"))
            (let ((payload (gemini::object
                            :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                            :tools (vector
                                    (gemini::object
                                     :function-declarations
                                     (vector (gemini::function-declaration
                                              :name "pickColor"
                                              :description "Pick a color"
                                              :parametersJsonSchema
                                              (gemini::object :type "object"
                                                              :properties (gemini::object
                                                                           :color (gemini::object :type "string"
                                                                                                  :enum (vector "red" "blue")))))))))))
              (handler-case
                  (progn
                    (gemini::build-lmstudio-payload "model-a" payload session)
                    (fail "Expected LM Studio tool translation to reject unsupported schema keys."))
                (error (e)
                  (is (search "does not yet support schema key" (princ-to-string e)))))))
       (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)))))

(test test-lmstudio-backend-allows-minimum-and-maximum-schema-constraints
  "Verify LM Studio tool translation accepts numeric minimum/maximum constraints used by real tools."
  (let ((session (gemini:make-runtime-session))
        (orig-bridge-url #'gemini::lmstudio-tool-bridge-url))
    (gemini:with-runtime-session (session)
      (unwind-protect
          (progn
            (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                  (lambda () "http://bridge.test/mcp"))
            (let* ((parameter-schema
                    (gemini::object
                     :type "object"
                     :properties (gemini::object
                                  :thoughtNumber (gemini::object :type "integer"
                                                                 :minimum 1
                                                                 :maximum 99))
                     :required (vector "thoughtNumber")))
                   (payload
                    (gemini::object
                     :contents (list (gemini::content :role "user" :parts (list (part "Hello LM Studio"))))
                     :tools (vector
                             (gemini::object
                              :function-declarations
                              (vector
                               (gemini::function-declaration
                                :name "boundedCounter"
                                :description "Uses bounded integer inputs"
                                :parametersJsonSchema parameter-schema))))))
                   (lmstudio-payload
                    (gemini::build-lmstudio-payload "model-a" payload session))
                   (integration
                    (first (coerce (gemini::adapter-field lmstudio-payload "integrations" :integrations)
                                   'list))))
              (is (equal "ephemeral_mcp" (gemini::adapter-field integration "type" :type)))
              (is (equal '("boundedCounter")
                         (coerce (gemini::adapter-field integration "allowed_tools" :allowed_tools) 'list)))))
       (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)))))

(test test-lmstudio-backend-contents-payload-translation-uses-latest-turn-with-previous-response
  "Verify LM Studio stateful follow-ups send only the latest turn text when previous_response_id is present."
  (let ((session (gemini:make-runtime-session)))
    (gemini:with-runtime-session (session)
      (setf (gemini:runtime-session-interaction-id session) "resp_prev")
      (let* ((payload (gemini::object
                      :contents (list (gemini::object
                                       :role "user"
                                       :parts (vector (gemini::object :text "Old turn")))
                                      (gemini::object
                                       :role "model"
                                       :parts (vector (gemini::object :text "Old reply")))
                                      (gemini::object
                                       :role "user"
                                       :parts (vector (gemini::object :text "Translate me!"))))))
             (lmstudio-payload (gemini::build-lmstudio-payload "model-a" payload session)))
        (is (equal "resp_prev"
                   (gemini::adapter-field lmstudio-payload "previous_response_id" :previous_response_id)))
        (is (equal "Translate me!"
                   (gemini::adapter-field lmstudio-payload "input" :input)))))))
