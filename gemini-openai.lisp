;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun %%invoke-openai (model-id payload &key (url "http://localhost:1234/v1/chat/completions") (read-timeout 300) (connect-timeout 60))
  "Invoke a local or OpenAI-compatible backend. No Google-tax required."
  (let ((response 
          (report-elapsed-time (format nil "OpenAI model `~a`" model-id)
                (dex:post url
                          :headers '(("Content-Type" . "application/json")
                                     ;; Even local runners sometimes want a dummy key
                                     ("Authorization" . "Bearer lm-studio"))
                          :content (cl-json:encode-json-to-string payload)
                          :read-timeout read-timeout
                          :connect-timeout connect-timeout))))
    response))

(defun parse-openai-response (json-string)
  "Reach into the OpenAI choices array and grab the goddamn signal."
  (let* ((data (cl-json:decode-json-from-string json-string))
         (choice (elt (gethash "choices" data) 0))
         (message (gethash "message" choice))
         (content (gethash "content" message)))
    content))

(defun openai-role->gemini-role (role)
  (cond ((and role (string-equal role "assistant")) "model")
        ((and role (string-equal role "tool")) "function")
        ((and role (string-equal role "system")) "user")
        ((and role (string-equal role "user")) "user")
        (t "model")))

(defun gemini-role->openai-role (role)
  (cond ((and role (string-equal role "model")) "assistant")
        ((and role (string-equal role "function")) "tool")
        ((and role (string-equal role "system")) "system")
        ((and role (string-equal role "user")) "user")
        (t "user")))

(defun openai-field (object &rest keys)
  "Gets the first present key from OBJECT, trying both string and keyword forms."
  (cond ((hash-table-p object)
         (dolist (key keys)
           (let ((value (gethash key object)))
             (when value
               (return-from openai-field value)))))
        ((consp object)
         (dolist (key keys)
           (let ((value (assoc key object :test #'equal)))
             (when value
               (return-from openai-field (cdr value))))))))

(defun part->openai-text (part)
  "Converts a Gemini part to an OpenAI message text fragment."
  (cond ((text-part? part) (get-text part))
        ((function-response-part? part)
         (format nil "~s" (dehashify (get-function-response part))))
        ((function-call-part? part)
         (format nil "~s" (dehashify (get-function-call part))))
        ((file-data-part? part) "[File data omitted]")
        ((inline-data-part? part) "[Inline data omitted]")
        ((executable-code-part? part)
         (format nil "~s" (dehashify (get-executable-code part))))
        ((code-execution-result-part? part)
         (format nil "~s" (dehashify (get-code-execution-result part))))
        (t nil)))

(defun content->openai-message (content)
  "Converts a Gemini content object into one OpenAI chat message object."
  (let* ((parts (openai-field content :parts "parts"))
         (part-list (typecase parts
                      (cons parts)
                      (vector (coerce parts 'list))
                      (t nil)))
         (text-fragments (remove nil (mapcar #'part->openai-text part-list)))
         (text (if text-fragments
                   (str:join "\n\n" text-fragments)
                   "")))
    (object :role (gemini-role->openai-role (openai-field content :role "role"))
            :content text)))

(defun build-openai-payload (model-id payload)
  "Converts a Gemini payload to an OpenAI chat-completions payload."
  (let* ((contents (get-contents payload))
         (system-instruction (get-system-instruction payload)) ;; Grab the soul
         (content-list (typecase contents
                         (cons contents)
                         (vector (coerce contents 'list))
                         (t nil)))
         (messages (remove nil (mapcar #'content->openai-message content-list)))
         (generation-config (get-generation-config payload)))

    ;; Prepend the system instruction if it exists
    (when system-instruction
      (push (object :role "system" 
                    :content (content->text system-instruction)) 
            messages))
            
    (let ((openai-payload (object :model model-id :messages messages)))

      (when generation-config
        (let ((temperature (get-temperature generation-config))
              (top-p (get-top-p generation-config))
              (frequency-penalty (get-frequency-penalty generation-config))
              (presence-penalty (get-presence-penalty generation-config))
              (max-output-tokens (get-max-output-tokens generation-config))
              (candidate-count (get-candidate-count generation-config))
              (stop-sequences (get-stop-sequences generation-config)))
          (when temperature
            (setf (get-temperature openai-payload) temperature))
          (when top-p
            (setf (get-top-p openai-payload) top-p))
          (when frequency-penalty
            (setf (get-frequency-penalty openai-payload) frequency-penalty))
          (when presence-penalty
            (setf (get-presence-penalty openai-payload) presence-penalty))
          (when max-output-tokens
            (setf (get-max-tokens openai-payload) max-output-tokens))
          (when candidate-count
            (setf (get-candidate-count openai-payload) candidate-count))
          (when stop-sequences
            (setf (get-stop-sequences openai-payload) stop-sequences))))
      openai-payload)))

(defun openai-usage->gemini-usage (usage)
  "Normalizes OpenAI usage object fields to Gemini usage metadata keys."
  (when usage
    (let* ((prompt-tokens (or (openai-field usage "prompt_tokens" :prompt_tokens)
                              (openai-field usage "promptTokens" :promptTokens)
                              (openai-field usage "input_tokens" :input_tokens)))
           (completion-tokens (or (openai-field usage "completion_tokens" :completion_tokens)
                                  (openai-field usage "completionTokens" :completionTokens)
                                  (openai-field usage "total_output_tokens" :response_tokens :responseTokens)))
           ;; Grab the reasoning details
           (details (openai-field usage "completion_tokens_details" :completion_tokens_details))
           (reasoning-tokens (or (openai-field usage "reasoning_tokens" :reasoning_tokens)
                                 (and (hash-table-p details) 
                                      (openai-field details "reasoning_tokens" :reasoning_tokens)))))
      (let ((usage-metadata (object)))
        (when prompt-tokens
          (setf (get-prompt-token-count usage-metadata) prompt-tokens))
        (when completion-tokens
          (setf (get-candidates-token-count usage-metadata) completion-tokens))
        (when reasoning-tokens
          (setf (get-thoughts-token-count usage-metadata) reasoning-tokens))
        (unless (zerop (hash-table-count usage-metadata))
          usage-metadata)))))

(defun openai-response->gemini-response (json-string)
  "Converts an OpenAI chat response JSON string into Gemini-style response objects."
  (handler-case
      (let* ((data (cl-json:decode-json-from-string json-string))
             (choices (or (openai-field data "choices" :choices) #()))
             (choice-list (typecase choices
                            (cons choices)
                            (vector (coerce choices 'list))
                            (t nil)))
             (candidates
               (mapcar
                (lambda (choice)
                  (let* ((message (openai-field choice "message" :message))
                         (role (openai-role->gemini-role (openai-field message "role" :role)))
                         (raw-content (or (openai-field message "content" :content) ""))
                         ;; NEW: Extract thoughts if they exist
                         (raw-thoughts (openai-field message "reasoning_content" :reasoning_content :reasoning))
                         (text (if (stringp raw-content)
                                   raw-content
                                   (format nil "~s" (dehashify raw-content))))
                         ;; Build the parts list properly
                         (parts (remove nil 
                                        (list (when (and (stringp raw-thoughts) (not (string= "" raw-thoughts)))
                                                (thought raw-thoughts))
                                              (part text))))
                         (candidate (object :content (content :role role
                                                              :parts parts))))
                    ;; ... (rest of the index and finish-reason logic)
                    candidate))
                choice-list))
             (response (object :candidates candidates))
             (usage-metadata (openai-usage->gemini-usage (openai-field data "usage" :usage)))
             (model-name (or (openai-field data "model" :model)
                             (openai-field data "model_name" :model_name))))
        (when model-name
          (setf (get-model-version response) model-name))
        (values response usage-metadata))
    (error (c)
      (format *trace-output* "~&;; WARNING: Failed to decode OpenAI response: ~a~%" c)
      (values (object :candidates (list (object :content (content :role "model"
                                                                  :parts (list (part json-string))))))
              nil))))
