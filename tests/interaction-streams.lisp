;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite interaction-stream-tests)

(test test-interaction-step-parsing
  "Test parsing of timeline-oriented steps from the Interactions API responses."
  (let* ((model-output-json (gemini::object
                             :type "model_output"
                             :index 1
                             :content (gemini::object
                                       :parts (vector (gemini::object :text "Hello world")))))
         (thought-json (gemini::object
                        :type "thought"
                        :index 0
                        :signature "opaque_thought_sig"
                        :summary "Thinking about the meaning of life"))
         (func-call-json (gemini::object
                          :type "function_call"
                          :index 2
                          :id "call_123"
                          :function-call (gemini::object
                                          :name "get_weather"
                                          :arguments (gemini::object :location "Boston"))))
         (step-mo (gemini:parse-interaction-step model-output-json))
         (step-th (gemini:parse-interaction-step thought-json))
         (step-fc (gemini:parse-interaction-step func-call-json)))

    (is (typep step-mo 'gemini:model-output-step))
    (is (= 1 (gemini:get-step-index step-mo)))
    (is (equal '("Hello world") (gemini:get-content step-mo)))

    (is (typep step-th 'gemini:thought-step))
    (is (= 0 (gemini:get-step-index step-th)))
    (is (equal "opaque_thought_sig" (gemini:get-signature step-th)))
    (is (equal "Thinking about the meaning of life" (gemini:get-summary step-th)))

    (is (typep step-fc 'gemini:function-call-step))
    (is (= 2 (gemini:get-step-index step-fc)))
    (is (equal "call_123" (gemini:get-call-id step-fc)))
    (is (equal "get_weather" (gemini:get-function-name step-fc)))
    (is (equal "Boston" (gethash :location (gemini:get-arguments step-fc))))))

(test test-interactions-backend-statefulness
  "Verify that interactions-backend injects previous_interaction_id, and captures the new interaction and environment IDs."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (setf (gemini:runtime-session-interaction-id session) "initial_id")
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (equal "initial_id" (gethash "previous_interaction_id" payload)))
                     (mock-successful-interaction-response
                      :id "new_interaction_id"
                      :environment-id "env_abc_123")))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (declare (ignore steps resp))
                 (is (equal "new_interaction_id" (gemini:runtime-session-interaction-id session)))
                 (is (equal "env_abc_123" (gemini:runtime-session-environment-id session))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-normalizes-steps-to-candidates
  "Verify that interactions-backend returns Gemini-style candidates and usage metadata from Interactions steps."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (gemini::object
                      :id "new_interaction_id"
                      :steps (vector
                              (gemini::object
                               :type "thought"
                               :index 0
                               :signature "sig_1"
                               :summary "Thinking")
                              (gemini::object
                               :type "function_call"
                               :index 1
                               :id "call_123"
                               :function-call (gemini::object
                                               :name "get_weather"
                                               :arguments (gemini::object :location "Boston")))
                              (gemini::object
                               :type "model_output"
                               :index 2
                               :content (gemini::object
                                         :parts (vector (gemini::object :text "Hello world")))))
                      :usage-metadata (gemini::object
                                       :prompt-token-count 11
                                       :thoughts-token-count 7
                                       :candidates-token-count 3))))

             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (let* ((candidates (gemini:get-candidates response))
                        (candidate (car candidates))
                        (content (gemini:get-content candidate))
                        (parts (gemini:get-parts content))
                        (function-calls (gemini::extract-function-calls-from-results response)))
                   (is (= 1 (length candidates)))
                   (is (equal 11 (gemini:get-prompt-token-count usage)))
                   (is (equal 7 (gemini:get-thoughts-token-count usage)))
                   (is (equal 3 (gemini:get-candidates-token-count usage)))
                   (is (= 3 (length parts)))
                   (is (gemini:thought-part? (first parts)))
                   (is (gemini::function-call-part? (second parts)))
                   (is (gemini:text-part? (third parts)))
                   (is (equal "Hello world" (gemini:get-text (third parts))))
                   (is (= 1 (length function-calls)))
                   (is (equal "get_weather"
                              (gemini:get-name (gemini:get-function-call (first function-calls)))))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-streaming
  "Verify that interactions-backend streaming handles SSE events, updates session IDs, and invokes the receiver."
  (let ((session (gemini:make-runtime-session))
        (orig-post-stream #'gemini::google-interactions-post-streaming))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::google-interactions-post-streaming)
                   (lambda (uri api-key payload receiver &key verbose read-timeout connect-timeout)
                     (declare (ignore uri api-key verbose read-timeout connect-timeout))
                     (is (gethash "stream" payload))
                     (funcall receiver (gemini::object :event-type "interaction.created" :interaction-id "id_streaming_123"))
                     (funcall receiver (gemini::object :event-type "step.start"
                                                       :step (gemini::object :type "thought" :index 0 :signature "sig" :summary "Starting thoughts")))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :delta (gemini::object :text "Hello ")))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :delta (gemini::object :text "world")))
                     (funcall receiver (gemini::object :event-type "step.stop"
                                                       :step (gemini::object :type "thought" :index 0 :signature "sig" :summary "Thoughts complete")))
                     (funcall receiver (gemini::object :event-type "interaction.completed"
                                                       :interaction (gemini::object :id "id_streaming_123" :environment-id "env_streaming_xyz" :steps #())))
                     nil))

             (let ((events '())
                   (backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload
                                      :receiver (lambda (event-type parsed-data raw)
                                                  (declare (ignore raw))
                                                  (push (list event-type parsed-data) events)))
               (let ((rev-events (nreverse events)))
                 (is (= 6 (length rev-events)))
                 (is (eq :interaction-created (first (first rev-events))))
                 (is (eq :step-start (first (second rev-events))))
                 (is (typep (second (second rev-events)) 'gemini:thought-step))
                 (is (eq :step-delta (first (third rev-events))))
                 (is (equal "Hello " (second (third rev-events))))
                 (is (eq :step-delta (first (fourth rev-events))))
                 (is (equal "world" (second (fourth rev-events))))
                 (is (eq :step-stop (first (fifth rev-events))))
                 (is (typep (second (fifth rev-events)) 'gemini:thought-step))
                 (is (eq :interaction-completed (first (sixth rev-events)))))
               (is (equal "id_streaming_123" (gemini:runtime-session-interaction-id session)))
               (is (equal "env_streaming_xyz" (gemini:runtime-session-environment-id session)))))
        (setf (fdefinition 'gemini::google-interactions-post-streaming) orig-post-stream)))))

(test test-interaction-stream-processor-accepts-decoder-double-hyphen-keys
  "Verify the stream processor accepts the keyword shape produced by the live SSE JSON decoder."
  (let ((session (gemini:make-runtime-session))
        (events '()))
    (let ((receiver (gemini::make-interaction-stream-processor
                     session
                     (lambda (event-type parsed-data raw)
                       (declare (ignore raw))
                       (push (list event-type parsed-data) events)))))
      (funcall receiver
               (gemini::object :event--type "interaction.created"
                               :interaction (gemini::object :id "int_live")))
      (funcall receiver
               (gemini::object :event--type "step.delta"
                               :delta (gemini::object :type "text" :text "STREAM_OK")
                               :index 1))
      (funcall receiver
               (gemini::object :event--type "interaction.completed"
                               :interaction (gemini::object
                                             :id "int_live"
                                             :environment--id "env_live"
                                             :steps #())))
      (let ((rev-events (nreverse events)))
        (is (= 3 (length rev-events)))
        (is (eq :interaction-created (first (first rev-events))))
        (is (eq :step-delta (first (second rev-events))))
        (is (equal "STREAM_OK" (second (second rev-events))))
        (is (eq :interaction-completed (first (third rev-events)))))
      (is (equal "int_live" (gemini:runtime-session-interaction-id session)))
      (is (equal "env_live" (gemini:runtime-session-environment-id session))))))

(test test-provider-stream-processor-supports-provider-hooks
  "Verify the provider-neutral SSE processor composes provider-specific event and session hooks."
  (let ((session (gemini:make-runtime-session))
        (events '()))
    (let ((receiver (gemini::make-provider-stream-processor
                     session
                     (lambda (event-type parsed-data raw)
                       (declare (ignore raw))
                       (push (list event-type parsed-data) events))
                     :event-type-reader (lambda (event)
                                          (gemini::adapter-field event "type" :type))
                     :event-type-parser (lambda (event-type-str)
                                          (intern (string-upcase (substitute #\- #\. event-type-str)) "KEYWORD"))
                     :event-payload-parser (lambda (event-type event)
                                             (declare (ignore event-type))
                                             (gemini::adapter-field event "payload" :payload))
                     :session-updater (lambda (runtime-session event)
                                        (let ((stream-id (gemini::adapter-field event "stream_id" :stream_id :stream-id)))
                                          (when stream-id
                                            (setf (gemini:runtime-session-interaction-id runtime-session) stream-id)))))))
      (funcall receiver (gemini::object :type "chat.start"
                                        :stream-id "lmstudio_stream_1"
                                        :payload (gemini::object :status "ok")))
      (let ((rev-events (nreverse events)))
        (is (= 1 (length rev-events)))
        (is (eq :CHAT-START (first (first rev-events))))
        (is (equal "ok"
                   (gemini::adapter-field (second (first rev-events)) "status" :status)))
        (is (equal "lmstudio_stream_1"
                   (gemini:runtime-session-interaction-id session)))))))

(test test-lmstudio-stream-processor-parses-named-events
  "Verify the LM Studio SSE adapter parses named events and captures the final response id."
  (let ((session (gemini:make-runtime-session))
        (events '()))
    (let ((receiver (gemini::make-lmstudio-stream-processor
                     session
                     (lambda (event-type parsed-data raw)
                       (declare (ignore raw))
                       (push (list event-type parsed-data) events)))))
      (funcall receiver
               (gemini::object :type "reasoning.delta"
                               :content "Need to"))
      (funcall receiver
               (gemini::object :type "message.delta"
                               :content "STREAM_OK"))
      (funcall receiver
               (gemini::object :type "chat.end"
                               :result (gemini::object
                                        :response_id "resp_lmstudio_1"
                                        :output (vector (gemini::object :type "message" :content "STREAM_OK")))))
      (let ((rev-events (nreverse events)))
        (is (= 3 (length rev-events)))
        (is (eq :reasoning-delta (first (first rev-events))))
        (is (equal "Need to" (second (first rev-events))))
        (is (eq :message-delta (first (second rev-events))))
        (is (equal "STREAM_OK" (second (second rev-events))))
        (is (eq :chat-end (first (third rev-events))))
        (is (equal "resp_lmstudio_1"
                   (gemini:runtime-session-interaction-id session)))))))

(test test-lmstudio-stream-processor-preserves-tool-call-events
  "Verify LM Studio tool_call stream events are forwarded intact through the named-event adapter."
  (let ((session (gemini:make-runtime-session))
        (events '()))
    (let ((receiver (gemini::make-lmstudio-stream-processor
                     session
                     (lambda (event-type parsed-data raw)
                       (declare (ignore raw))
                       (push (list event-type parsed-data) events)))))
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
               (gemini::object :type "tool_call.failure"
                               :reason "Cannot find tool with name missingTool."
                               :metadata (gemini::object :type "invalid_name"
                                                        :tool_name "missingTool")))
      (funcall receiver
               (gemini::object :type "chat.end"
                               :result (gemini::object
                                        :response_id "resp_lmstudio_tool_1"
                                        :output (vector
                                                 (gemini::object :type "tool_call"
                                                                 :tool "machineType"
                                                                 :arguments (gemini::object :detail "full")
                                                                 :output "[{\"type\":\"text\",\"text\":\"x86_64\"}]")
                                                 (gemini::object :type "message"
                                                                 :content "STREAM_OK")))))
      (let ((rev-events (nreverse events)))
        (is (= 5 (length rev-events)))
        (is (eq :tool-call-start (first (first rev-events))))
        (is (equal "machineType"
                   (gemini::adapter-field (second (first rev-events)) "tool" :tool)))
        (is (eq :tool-call-arguments (first (second rev-events))))
        (is (equal "full"
                   (gemini::adapter-field
                    (gemini::adapter-field (second (second rev-events)) "arguments" :arguments)
                    "detail" :detail)))
        (is (eq :tool-call-success (first (third rev-events))))
        (is (search "x86_64"
                    (gemini::adapter-field (second (third rev-events)) "output" :output)))
        (is (eq :tool-call-failure (first (fourth rev-events))))
        (is (search "missingTool"
                    (gemini::adapter-field (second (fourth rev-events)) "reason" :reason)))
        (is (eq :chat-end (first (fifth rev-events))))
        (is (equal "resp_lmstudio_tool_1"
                   (gemini:runtime-session-interaction-id session)))))))

(test test-interactions-backend-reconstructs-response-from-streamed-steps
  "Verify the full backend path can reconstruct a result from streamed steps when interaction.completed carries no final steps."
  (let ((session (gemini:make-runtime-session))
        (orig-post-stream #'gemini::google-interactions-post-streaming))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::google-interactions-post-streaming)
                   (lambda (uri api-key payload receiver &key verbose read-timeout connect-timeout)
                     (declare (ignore uri api-key verbose read-timeout connect-timeout))
                     (is (gethash "stream" payload))
                     (funcall receiver (gemini::object :event-type "interaction.created"
                                                       :interaction-id "int_stream_reconstruct"))
                     (funcall receiver (gemini::object :event-type "step.start"
                                                       :step (gemini::object :type "model_output" :index 0)))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :index 0
                                                       :delta (gemini::object :text "STREAM_")))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :index 0
                                                       :delta (gemini::object :text "OK")))
                     (funcall receiver (gemini::object :event-type "step.stop"
                                                       :step (gemini::object :type "model_output" :index 0)))
                     (funcall receiver (gemini::object :event-type "interaction.completed"
                                                       :interaction (gemini::object
                                                                     :id "int_stream_reconstruct"
                                                                     :environment-id "env_stream_reconstruct"
                                                                     :steps #()
                                                                     :usage-metadata (gemini::object
                                                                                      :prompt-token-count 4
                                                                                      :candidates-token-count 2))))
                     nil))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (is (equal "STREAM_OK"
                            (gemini:get-text
                             (first (gemini:get-parts
                                     (gemini:get-content (first (gemini:get-candidates response))))))))
                 (is (equal "int_stream_reconstruct" (gemini::get-response-id response)))
                 (is (= 4 (gemini:get-prompt-token-count usage)))
                 (is (= 2 (gemini:get-candidates-token-count usage)))
                 (is (equal "int_stream_reconstruct" (gemini:runtime-session-interaction-id session)))
                 (is (equal "env_stream_reconstruct" (gemini:runtime-session-environment-id session))))))
        (setf (fdefinition 'gemini::google-interactions-post-streaming) orig-post-stream)))))

(test test-interactions-backend-prefers-streamed-steps-over-empty-completed-step
  "Verify the full backend path ignores empty completed step payloads when streamed deltas contain the real output."
  (let ((session (gemini:make-runtime-session))
        (orig-post-stream #'gemini::google-interactions-post-streaming))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::google-interactions-post-streaming)
                   (lambda (uri api-key payload receiver &key verbose read-timeout connect-timeout)
                     (declare (ignore uri api-key verbose read-timeout connect-timeout))
                     (funcall receiver (gemini::object :event-type "interaction.created"
                                                       :interaction-id "int_stream_empty_completed"))
                     (funcall receiver (gemini::object :event-type "step.start"
                                                       :step (gemini::object :type "model_output" :index 0)))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :index 0
                                                       :delta (gemini::object :text "STREAM_OK")))
                     (funcall receiver (gemini::object :event-type "step.stop"
                                                       :step (gemini::object :type "model_output" :index 0)))
                     (funcall receiver (gemini::object :event-type "interaction.completed"
                                                       :interaction (gemini::object
                                                                     :id "int_stream_empty_completed"
                                                                     :steps (vector (gemini::object
                                                                                     :type "model_output"
                                                                                     :index 0
                                                                                     :content (gemini::object
                                                                                               :parts (vector (gemini::object :text ""))))))))
                     nil))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (declare (ignore usage))
                 (is (equal "STREAM_OK"
                            (gemini:get-text
                             (first (gemini:get-parts
                                     (gemini:get-content (first (gemini:get-candidates response)))))))))))
        (setf (fdefinition 'gemini::google-interactions-post-streaming) orig-post-stream)))))
