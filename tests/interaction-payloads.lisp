;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite interaction-payload-tests)

(test test-lmstudio-result->gemini-response
  "Verify LM Studio chat.end results normalize into Gemini-style response parts and usage."
  (multiple-value-bind (response usage)
      (gemini::lmstudio-result->gemini-response
       (gemini::object
        :model_instance_id "openai/gpt-oss-20b"
        :response_id "resp_123"
        :output (vector
                 (gemini::object :type "reasoning" :content "Need to call function.")
                 (gemini::object :type "tool_call"
                                 :tool "model_search"
                                 :arguments (gemini::object :limit 1)
                                 :output "[{\"type\":\"text\",\"text\":\"Showing first 1 models...\"}]"
                                 :provider_info (gemini::object :type "ephemeral_mcp"
                                                                :server_label "huggingface"))
                 (gemini::object :type "message" :content "The current top-trending model is..."))
        :stats (gemini::object
                :input_tokens 329
                :total_output_tokens 268
                :reasoning_output_tokens 5)))
    (let* ((candidate (first (gemini:get-candidates response)))
           (content (gemini:get-content candidate))
           (parts (gemini:get-parts content))
           (function-calls (gemini::extract-function-calls-from-results response)))
      (is (equal "resp_123" (gemini::get-response-id response)))
      (is (equal "openai/gpt-oss-20b" (gemini:get-model-version response)))
      (is (= 2 (length parts)))
      (is (gemini:thought-part? (first parts)))
      (is (gemini:text-part? (second parts)))
      (is (equal "Need to call function." (gemini:get-text (first parts))))
      (is (equal "The current top-trending model is..."
                 (gemini:get-text (second parts))))
      (is (null function-calls))
      (is (= 329 (gemini:get-prompt-token-count usage)))
      (is (= 5 (gemini:get-thoughts-token-count usage)))
      (is (= 263 (gemini:get-candidates-token-count usage))))))

(test test-lmstudio-result->gemini-response-allows-missing-stats
  "Verify LM Studio chat.end normalization tolerates results without usage stats."
  (multiple-value-bind (response usage)
      (gemini::lmstudio-result->gemini-response
       (gemini::object
        :model_instance_id "qwen/qwen3.6-27b"
        :response_id "resp_no_stats"
        :output (vector (gemini::object :type "message" :content "OK"))))
    (is (equal "resp_no_stats" (gemini::get-response-id response)))
    (is (equal "OK"
               (gemini:get-text
                (first (gemini:get-parts
                        (gemini:get-content (first (gemini:get-candidates response))))))))
    (is (null usage))))

(test test-lmstudio-result->gemini-response-accepts-cl-json-keywords
  "Verify LM Studio non-stream decoding works with CL-JSON keyword names like :RESPONSE--ID."
  (multiple-value-bind (response usage)
      (gemini::lmstudio-result->gemini-response
       (list (cons :model--instance--id "qwen/qwen3.6-27b")
             (cons :response--id "resp_kw")
             (cons :output (list (list (cons :type "message")
                                       (cons :content "OK"))))
             (cons :stats (list (cons :input--tokens 15)
                                (cons :total--output--tokens 110)
                                (cons :reasoning--output--tokens 106)))))
    (is (equal "resp_kw" (gemini::get-response-id response)))
    (is (equal "qwen/qwen3.6-27b" (gemini:get-model-version response)))
    (is (= 15 (gemini:get-prompt-token-count usage)))
    (is (= 106 (gemini:get-thoughts-token-count usage)))
    (is (= 4 (gemini:get-candidates-token-count usage)))))

(test test-interactions-input-shape-matrix
  "Probe a range of Interactions input shapes and identify which ones serialize cleanly."
  (labels ((text-part (text)
             (gemini::object :type "text" :text text))
           (text-content (text)
             (make-instance 'gemini::text-content :text text))
           (user-input-step (&rest parts)
             (make-instance 'gemini::user-input-step :content parts))
           (serialize-input (input)
             (gemini::request-body->interaction-payload
              (make-instance 'gemini::request-body
                             :model "models/gemini-3.5-flash"
                             :input input)))
           (serializes-cleanly-p (input)
             (handler-case
                 (progn
                   (serialize-input input)
                   t)
               (error () nil))))
    (let ((cases (list (list "single user_input step"
                             (list (user-input-step (text-content "hello")))
                             t)
                       (list "plain string input"
                             "hello"
                             t)
                       (list "two user_input turns"
                             (list (user-input-step (text-content "hello"))
                                   (user-input-step (text-content "follow up")))
                             nil)
                       (list "user_input with multiple parts"
                             (list (user-input-step (text-content "hello")
                                                    (text-content "world")))
                             t)
                       (list "bare text parts"
                             (list (text-part "hello"))
                             t)
                       (list "legacy content objects"
                             (list (gemini::object :role "user"
                                                   :parts (vector (text-content "hello"))))
                             nil))))
      (dolist (case cases)
        (destructuring-bind (name input expected) case
          (let ((actual (serializes-cleanly-p input)))
            (format t "~&~A => ~A~%" name (if actual "valid" "invalid"))
            (is (eq expected actual))))))))

(test test-interactions-input-rejects-legacy-content-object
  "Regression test: the Interactions input must use user_input steps, not legacy content objects."
  (let ((payload (make-instance 'gemini::request-body
                                :model "models/gemini-3.5-flash"
                                :input (list (gemini::object
                                              :role "user"
                                              :parts (vector (make-instance 'gemini::text-content
                                                                            :text "hello")))))))
    (signals error
      (gemini::request-body->interaction-payload payload))))

(test test-normalize-lmstudio-input-accepts-dehashified-chatbot-history
  "Verify LM Studio input normalization accepts dehashified/alist chatbot history entries."
  (let* ((history (list (gemini::dehashify
                         (gemini::content :role "user"
                                          :parts (list (part "**This is conversation #1.**"))))
                        (gemini::dehashify
                         (gemini::content :role "user"
                                          :parts (list (part "Reply with exactly: TURN ONE OK"))))))
         (normalized (gemini::normalize-lmstudio-input history)))
    (is (search "**This is conversation #1.**" normalized))
    (is (search "Reply with exactly: TURN ONE OK" normalized))))
