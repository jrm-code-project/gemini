;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite interaction-live-tests)

(defun live-google-interactions-stream-test-enabled-p ()
  (let ((flag (uiop:getenv "GEMINI_RUN_LIVE_INTERACTIONS_STREAM_TEST")))
    (and flag (> (length flag) 0))))

(defun live-lmstudio-stream-test-enabled-p ()
  (let ((flag (uiop:getenv "GEMINI_RUN_LIVE_LMSTUDIO_STREAM_TEST")))
    (and flag (> (length flag) 0))))

(defun find-live-lmstudio-model-id ()
  (flet ((decode-json-body (body)
           (jsonx:with-decoder-jrm-semantics
             (cl-json:decode-json-from-string body)))
         (first-string-field (items &rest keys)
           (loop for item in items
                 for value = (apply #'gemini::adapter-field item keys)
                 when (stringp value)
                   do (return value))))
    (handler-case
        (let* ((v1-body (dex:get "http://127.0.0.1:1234/v1/models"))
               (v1-json (decode-json-body v1-body))
               (models (coerce (or (gemini::adapter-field v1-json "models" :models) #()) 'list))
               (loaded-id
                 (loop for model in models
                       for loaded = (gemini::adapter-as-list
                                     (or (gemini::adapter-field model "loaded_instances"
                                                                 :loaded_instances
                                                                 :loaded-instances)
                                         '()))
                       when (plusp (length loaded))
                         do (return (or (first-string-field loaded "id" :id)
                                        (gemini::adapter-field model "key" :key)
                                        (gemini::adapter-field model "id" :id))))))
          (or loaded-id
              (let* ((api-body (dex:get "http://127.0.0.1:1234/api/v1/models"))
                     (api-json (decode-json-body api-body))
                     (models (coerce (or (gemini::adapter-field api-json "data" :data) #()) 'list)))
                (first-string-field models "id" :id))))
      (error () nil))))

(test test-google-interactions-streaming-live-e2e
  "Opt-in live end-to-end test against the Google Interactions SSE endpoint."
  (if (and (live-google-interactions-stream-test-enabled-p)
           (handler-case (google:gemini-api-key)
             (error () nil)))
      (let ((session (gemini:make-runtime-session))
            (events '())
            (payload (make-hash-table :test 'equal)))
        (setf (gethash "model" payload) "models/gemini-3.5-flash")
        (setf (gethash "input" payload)
              "Reply with exactly the token STREAM_OK and no other text.")
        (setf (gethash "stream" payload) t)
        (gemini:with-runtime-session (session)
          (gemini::google-interactions-post-streaming
           "https://generativelanguage.googleapis.com/v1beta/interactions?alt=sse"
           (google:gemini-api-key)
           payload
           (gemini::make-interaction-stream-processor
            session
            (lambda (event-type parsed-data raw-event)
              (push (list event-type parsed-data raw-event) events))))
          (let* ((rev-events (nreverse events))
                 (delta-texts (remove nil
                                      (mapcar (lambda (entry)
                                                (let ((event-type (first entry))
                                                      (parsed-data (second entry)))
                                                  (and (eq event-type :step-delta)
                                                       (stringp parsed-data)
                                                       parsed-data)))
                                              rev-events))))
            (is (> (length rev-events) 0))
            (is (find :interaction-created rev-events :key #'first))
            (is (find :interaction-completed rev-events :key #'first))
            (is (find "STREAM_OK" delta-texts :test #'search))
            (is (stringp (gemini:runtime-session-interaction-id session))))))
      (finishes (values))))

(test test-google-interactions-live-backend-response-e2e
  "Opt-in live end-to-end test for the full Interactions backend reconstruction path."
  (if (and (live-google-interactions-stream-test-enabled-p)
           (handler-case (google:gemini-api-key)
             (error () nil)))
      (let ((session (gemini:make-runtime-session))
            (payload (make-hash-table :test 'equal))
            (backend (make-instance 'gemini:interactions-backend)))
        (setf (gethash "model" payload) "models/gemini-3.5-flash")
        (setf (gethash "input" payload)
              "Reply with exactly the token STREAM_OK and no other text.")
        (gemini:with-runtime-session (session)
          (multiple-value-bind (response usage)
              (gemini:invoke-backend backend "models/gemini-3.5-flash" payload)
            (declare (ignore usage))
            (is (equal "STREAM_OK"
                       (gemini:get-text
                        (first (gemini:get-parts
                                (gemini:get-content (first (gemini:get-candidates response))))))))
            (is (stringp (gemini::get-response-id response)))
            (is (stringp (gemini:runtime-session-interaction-id session))))))
      (finishes (values))))

(test test-lmstudio-live-streaming-e2e
  "Opt-in live end-to-end test for the raw LM Studio named-event SSE stream."
  (let ((model-id (and (live-lmstudio-stream-test-enabled-p)
                       (find-live-lmstudio-model-id))))
    (if model-id
        (let ((session (gemini:make-runtime-session))
              (events '())
              (payload (gemini::object
                        :model model-id
                        :input "Reply with exactly the token STREAM_OK and no other text."
                        :stream t
                        :store nil)))
          (gemini:with-runtime-session (session)
            (gemini::lmstudio-post-streaming
             "http://127.0.0.1:1234/api/v1/chat"
             payload
             (gemini::make-lmstudio-stream-processor
              session
              (lambda (event-type parsed-data raw-event)
                (push (list event-type parsed-data raw-event) events))))
            (let* ((rev-events (nreverse events))
                   (chat-end-entry (find :chat-end rev-events :key #'first))
                   (chat-end-result (and chat-end-entry
                                         (gemini::adapter-field (third chat-end-entry) "result" :result)))
                   (output-items (coerce (or (and chat-end-result
                                                  (gemini::adapter-field chat-end-result "output" :output))
                                             #())
                                        'list))
                   (message-item (find "message" output-items
                                       :test #'equal
                                       :key (lambda (item)
                                              (gemini::adapter-field item "type" :type))))
                   (delta-texts (remove nil
                                        (mapcar (lambda (entry)
                                                  (let ((event-type (first entry))
                                                        (parsed-data (second entry)))
                                                    (and (eq event-type :message-delta)
                                                         (stringp parsed-data)
                                                         parsed-data)))
                                                rev-events))))
              (is (> (length rev-events) 0))
              (is (find :chat-start rev-events :key #'first))
              (is (not (null chat-end-entry)))
              (is (find "STREAM_OK" delta-texts :test #'search))
              (is (equal "STREAM_OK"
                         (gemini::adapter-field message-item "content" :content)))
              (is (stringp (gemini:runtime-session-interaction-id session))))))
        (finishes (values)))))

(test test-lmstudio-live-backend-response-e2e
  "Opt-in live end-to-end test against a real local LM Studio /api/v1/chat endpoint."
  (let ((model-id (and (live-lmstudio-stream-test-enabled-p)
                       (find-live-lmstudio-model-id))))
    (if model-id
        (let ((session (gemini:make-runtime-session))
              (backend (make-instance 'gemini::lmstudio-backend :url "http://127.0.0.1:1234/api/v1/chat"))
              (payload (gemini::object
                        :contents (list (gemini::content
                                         :role "user"
                                         :parts (list (part "Reply with exactly the token STREAM_OK and no other text.")))))))
          (gemini:with-runtime-session (session)
            (multiple-value-bind (response usage)
                (gemini:invoke-backend backend model-id payload)
              (is (equal "STREAM_OK"
                         (gemini:get-text
                          (first (gemini:get-parts
                                  (gemini:get-content (first (gemini:get-candidates response))))))))
              (is (stringp (gemini::get-response-id response)))
              (is (equal (gemini::get-response-id response)
                         (gemini:runtime-session-interaction-id session)))
              (is (plusp (gemini:get-prompt-token-count usage)))
              (is (>= (gemini:get-candidates-token-count usage) 1)))))
        (finishes (values)))))

(test test-lmstudio-live-tool-bridge-e2e
  "Opt-in live end-to-end LM Studio tool call test through the local MCP bridge."
  (let ((model-id (and (live-lmstudio-stream-test-enabled-p)
                       (find-live-lmstudio-model-id))))
    (if model-id
        (let ((session (gemini:make-runtime-session))
              (bridge nil)
              (events '())
              (orig-bridge-url #'gemini::lmstudio-tool-bridge-url)
              (backend (make-instance 'gemini::lmstudio-backend
                                      :url "http://127.0.0.1:1234/api/v1/chat")))
          (gemini:with-runtime-session (session)
            (unwind-protect
                (progn
                  (setf bridge (gemini:start-lmstudio-tool-bridge))
                  (sleep 0.1)
                  (setf (fdefinition 'gemini::lmstudio-tool-bridge-url)
                        (lambda () (gemini:active-lmstudio-tool-bridge-url)))
                  (let* ((config (make-instance 'gemini::persona-config
                                                :name "lmstudio-live-tool-bridge-test"
                                                :googleapi :lmstudio-api
                                                :enable-misc-tools t
                                                :model model-id
                                                :url "http://127.0.0.1:1234/api/v1/chat"))
                         (generator (make-instance 'gemini::content-generator :config config))
                         (payload (gemini::object
                                   :system_instruction
                                   (gemini::content
                                    :role "system"
                                    :parts (list (part (concatenate 'string
                                                                    "You must call the machineType tool exactly once. "
                                                                    "Do not guess. After the tool returns, respond "
                                                                    "with exactly the tool result and no extra text."))))
                                   :contents
                                   (list (gemini::content
                                          :role "user"
                                          :parts (list (part "What is the machine type?"))))
                                   :tools (vector
                                           (gemini::object
                                            :function-declarations
                                            (vector (gemini::function-declaration
                                                     :name "machineType"
                                                     :description "Returns the type of the machine."
                                                     :parameters (gemini::schema :type :object))))))))
                    (gemini:invoke-backend
                     backend
                     model-id
                     payload
                     :content-generator generator
                     :receiver (lambda (event-type parsed-data raw-event)
                                 (push (list event-type parsed-data raw-event) events))))
                  (let* ((rev-events (nreverse events))
                         (chat-end-entry (find :chat-end rev-events :key #'first))
                         (chat-end-result (and chat-end-entry
                                               (gemini::adapter-field (third chat-end-entry) "result" :result)))
                         (tool-start-entry (find :tool-call-start rev-events :key #'first))
                         (tool-arguments-entry (find :tool-call-arguments rev-events :key #'first))
                         (tool-success-entry (find :tool-call-success rev-events :key #'first)))
                    (is (> (length rev-events) 0))
                    (is (not (null tool-start-entry)))
                    (is (not (null tool-arguments-entry)))
                    (is (not (null tool-success-entry)))
                    (is (not (null chat-end-result)))
                    (is (equal "machineType"
                               (gemini::adapter-field (third tool-start-entry) "tool" :tool)))
                    (multiple-value-bind (response usage)
                        (gemini::lmstudio-result->gemini-response chat-end-result)
                      (let* ((final-text (string-trim '(#\Space #\Tab #\Return #\Newline)
                                                      (gemini:get-text
                                                       (first (gemini:get-parts
                                                               (gemini:get-content
                                                                (first (gemini:get-candidates response))))))))
                             (expected (string-trim '(#\Space #\Tab #\Return #\Newline)
                                                    (machine-type))))
                        (is (string-equal expected final-text))
                        (is (plusp (gemini:get-prompt-token-count usage)))
                        (is (equal (gemini::get-response-id response)
                                   (gemini:runtime-session-interaction-id session)))))))
              (setf (fdefinition 'gemini::lmstudio-tool-bridge-url) orig-bridge-url)
              (when bridge
                (gemini:stop-lmstudio-tool-bridge))))
        (finishes (values))))))
