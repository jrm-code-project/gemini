;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite interaction-backend-tests)

(test test-interactions-backend-retries-malformed-tool-call
  "Verify that interactions-backend retries malformed_tool_call responses and succeeds on a later attempt."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post)
        (calls 0)
        (gemini::+interactions-malformed-tool-call-max-retries+ 5))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (incf calls)
                     (if (= calls 1)
                         (error "HTTP 400 bad request. {\"error\":{\"message\":\"Model generated invalid JSON syntax and the output could not be parsed.\",\"code\":\"malformed_tool_call\"}}")
                         (gemini::object
                          :id "retry_ok"
                          :steps (vector (gemini::object
                                          :type "model_output"
                                          :index 0
                                          :content (gemini::object
                                                    :parts (vector (gemini::object :text "Recovered")))))))))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (declare (ignore usage))
                 (is (= 2 calls))
                 (is (equal "Recovered"
                            (gemini:get-text
                             (first (gemini:get-parts
                                     (gemini:get-content (first (gemini:get-candidates response)))))))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-does-not-retry-other-bad-request
  "Verify that interactions-backend does not retry unrelated 400 bad request responses."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post)
        (calls 0)
        (gemini::+interactions-malformed-tool-call-max-retries+ 5))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (incf calls)
                     (error "HTTP 400 bad request. {\"error\":{\"message\":\"schema at top-level must be a boolean or an object\",\"code\":\"invalid_request\"}}")))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (signals error
                 (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))
               (is (= 1 calls))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-errors-on-empty-results
  "Verify that interactions-backend signals when the API returns no result parts instead of silently returning NIL candidates."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (gemini::object
                      :id "empty_result"
                      :steps #())))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (signals error
                 (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-invoke-interaction-payload
  "Verify that invoke-interaction constructs payload correctly."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Hello robot" (gethash "input" payload)))
                     (mock-successful-interaction-response)))
             (multiple-value-bind (steps resp)
                 (gemini:invoke-interaction "Hello robot" :model :gemini-3.5-flash)
               (declare (ignore steps resp))
               (is (equal "mock_id" (gemini:runtime-session-interaction-id session)))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-invoke-interaction-with-session-binds-explicit-session
  "Verify that explicit-session interaction calls update the supplied session rather than the ambient one."
  (let ((ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (unwind-protect
         (progn
           (setf (fdefinition 'google:google-post)
                 (lambda (url key payload &key api-revision read-timeout connect-timeout)
                   (declare (ignore url key api-revision read-timeout connect-timeout))
                   (is (equal "Hello explicit" (gethash "input" payload)))
                   (mock-successful-interaction-response :id "explicit_id")))
           (let ((gemini::*current-session* ambient-session))
             (multiple-value-bind (steps resp)
                 (gemini:invoke-interaction-with-session explicit-session "Hello explicit" :model :gemini-3.5-flash)
               (declare (ignore steps resp))
               (is (null (gemini:runtime-session-interaction-id ambient-session)))
               (is (equal "explicit_id" (gemini:runtime-session-interaction-id explicit-session)))
               (is (eq ambient-session gemini::*current-session*)))))
      (setf (fdefinition 'google:google-post) orig-google-post))))

(test test-backend-selection-via-config
  "Verify that setting googleapi slot in config to symbol selects the correct backend class."
  (let* ((config-interactions (make-instance 'gemini::persona-config :name "int" :googleapi :google-interactions-api))
         (config-gemini-symbol (make-instance 'gemini::persona-config :name "gem-sym" :googleapi :google-api))
         (config-lmstudio-symbol (make-instance 'gemini::persona-config :name "lm" :googleapi :lmstudio-api :url "http://localhost:1234/api/v1/chat"))
         (config-openai-symbol (make-instance 'gemini::persona-config :name "op-sym" :googleapi :openai-api :url "http://test"))
         (config-gemini-bool (make-instance 'gemini::persona-config :name "gem-bool" :googleapi t))
         (config-openai-bool (make-instance 'gemini::persona-config :name "op-bool" :googleapi nil :url "http://test"))
         (config-lmstudio-no-symbol (make-instance 'gemini::persona-config :name "lm-no-symbol" :googleapi nil :url "http://localhost:1234/api/v1/chat")))

    (let ((gen-int (make-instance 'gemini::content-generator :config config-interactions))
          (gen-gem-sym (make-instance 'gemini::content-generator :config config-gemini-symbol))
          (gen-lm-sym (make-instance 'gemini::content-generator :config config-lmstudio-symbol))
          (gen-op-sym (make-instance 'gemini::content-generator :config config-openai-symbol))
          (gen-gem-bool (make-instance 'gemini::content-generator :config config-gemini-bool))
          (gen-op-bool (make-instance 'gemini::content-generator :config config-openai-bool))
          (gen-lm-no-symbol (make-instance 'gemini::content-generator :config config-lmstudio-no-symbol)))

      (is (typep (gemini::get-backend gen-int) 'gemini:interactions-backend))
      (is (typep (gemini::get-backend gen-gem-sym) 'gemini:gemini-backend))
      (is (typep (gemini::get-backend gen-lm-sym) 'gemini:lmstudio-backend))
      (is (typep (gemini::get-backend gen-op-sym) 'gemini:openai-backend))
      (is (typep (gemini::get-backend gen-gem-bool) 'gemini:gemini-backend))
      (is (typep (gemini::get-backend gen-op-bool) 'gemini:openai-backend))
      (is (typep (gemini::get-backend gen-lm-no-symbol) 'gemini:openai-backend)))))

(test test-interactions-backend-contents-payload-translation
  "Verify that passing a legacy contents-based payload to interactions-backend automatically translates it into a stateful interactions payload."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Translate me!" (gethash "input" payload)))
                     (mock-successful-interaction-response)))
             (let* ((legacy-payload (gemini::object
                                     :contents (list (gemini::object
                                                      :role "user"
                                                      :parts (vector (gemini::object :text "Old turn")))
                                                     (gemini::object
                                                      :role "model"
                                                      :parts (vector (gemini::object :text "Response")))
                                                     (gemini::object
                                                      :role "user"
                                                      :parts (vector (gemini::object :text "Translate me!"))))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)
                 (declare (ignore steps resp))
                 (is (equal "mock_id" (gemini:runtime-session-interaction-id session))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-strips-unsupported-gemini-fields
  "Verify that Gemini-only legacy fields are removed before posting to the Interactions API."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (null (gethash "systemInstruction" payload)))
                     (is (null (gethash "generationConfig" payload)))
                     (is (null (gethash "safetySettings" payload)))
                     (is (null (gethash "cachedContent" payload)))
                     (is (null (gethash "toolConfig" payload)))
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Translate me!" (gethash "input" payload)))
                     (mock-successful-interaction-response)))
             (let* ((legacy-payload
                     (gemini::object
                      :contents (list (gemini::object
                                       :role "user"
                                       :parts (vector (gemini::object :text "Translate me!"))))
                      :system-instruction (list "Keep the noir tone.")
                      :generation-config (gemini::object :temperature 0.2)
                      :safety-settings (list (gemini::object :category "HARM_CATEGORY_HATE_SPEECH"
                                                             :threshold "BLOCK_ONLY_HIGH"))
                      :cached-content "cached/123"
                      :tool-config (gemini::object :function-calling-config
                                                   (gemini::object :mode "AUTO"))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)
                 (declare (ignore steps resp))
                 (is (equal "mock_id" (gemini:runtime-session-interaction-id session))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-zero-arg-tool-translation
  "Verify that zero-argument function tools omit an empty required array for Interactions."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters)))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "read_graph" (gemini::adapter-field tool "name" :name)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (null (gemini::adapter-field params "required" :required))))
                     (mock-successful-interaction-response)))
             (let* ((declaration
                     (gemini::object
                      :name "read_graph"
                      :description "Read the entire knowledge graph"
                      :parameters (gemini::object
                                   :type 6
                                   :properties (gemini::object)
                                   :required '())))
                    (legacy-payload
                     (gemini::object
                      :model "gemini-3.5-flash"
                      :input "hello"
                      :tools (list
                              (gemini::object
                               :function-declarations (list declaration)))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-zero-arg-tool-default-parameters
  "Verify that zero-argument tools without declared parameters still emit an empty object schema."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters))
                            (properties (and params (gemini::adapter-field params "properties" :properties))))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "currentDirectory" (gemini::adapter-field tool "name" :name)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (hash-table-p properties))
                       (is (= 0 (hash-table-count properties)))
                       (is (null (gemini::adapter-field params "required" :required))))
                     (mock-successful-interaction-response)))
             (let* ((declaration
                     (gemini::object
                      :name "currentDirectory"
                      :description "Returns the current directory pathname."))
                    (legacy-payload
                     (gemini::object
                      :model "gemini-3.5-flash"
                      :input "hello"
                      :tools (list
                              (gemini::object
                               :function-declarations (list declaration)))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-tools-translation
  "Verify that passing legacy tools schemas to interactions-backend translates them flatly to 'type: function' format."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters))
                            (properties (gemini::adapter-field params "properties" :properties))
                            (inner-type-param (gemini::adapter-field properties "type" :type)))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "create_entities" (gemini::adapter-field tool "name" :name)))
                       (is (equal "Create entities" (gemini::adapter-field tool "description" :description)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (equal "string" (gemini::adapter-field inner-type-param "type" :type))))
                     (mock-successful-interaction-response)))
             (let* ((legacy-payload (gemini::object
                                     :model "gemini-3.5-flash"
                                     :input (list (make-instance 'gemini::user-input-step
                                                                 :content (list (make-instance 'gemini::text-content :text "hi"))))
                                     :tools (list (gemini::object
                                                   :function-declarations (list (gemini::object
                                                                                 :name "create_entities"
                                                                                 :description "Create entities"
                                                                                 :parameters (gemini::object
                                                                                              :type 6
                                                                                              :properties (gemini::object
                                                                                                           :type (gemini::object :type 1 :description "A string parameter name type")))))))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))
