;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun adapter-as-list (value)
  "Converts VALUE to a list for uniform adapter traversal." 
  (typecase value
    (null nil)
    (cons value)
    (vector (coerce value 'list))
    (t (list value))))

(defun adapter-field (object &rest keys)
  "Returns the first present field value from OBJECT for any of KEYS.
Supports hash-tables and alists with mixed string/keyword keys."
  (cond ((hash-table-p object)
         (dolist (key keys)
           (multiple-value-bind (value presentp)
               (gethash key object)
             (when presentp
               (return-from adapter-field value)))))
        ((consp object)
         (dolist (key keys)
           (let ((entry (assoc key object :test #'equal)))
             (when entry
               (return-from adapter-field (cdr entry))))))
        (t nil)))

(defun validate-gemini-payload-shape (payload)
  "Performs lightweight adapter preflight checks on Gemini-style payload shape."
  (unless (hash-table-p payload)
    (error "Payload must be a hash-table object, got ~s" (type-of payload)))
  (let* ((contents (get-contents payload))
         (content-list (adapter-as-list contents)))
    (dolist (entry content-list)
      (unless (content? entry)
        (error "Payload contains non-content entry: ~s" entry)))))

(defun openai-role->gemini-role (role)
  "Maps OpenAI chat role strings to Gemini roles."
  (cond ((and role (string-equal role "assistant")) "model")
        ((and role (string-equal role "tool")) "function")
        ((and role (string-equal role "system")) "user")
        ((and role (string-equal role "user")) "user")
        (t "model")))

(defun gemini-role->openai-role (role)
  "Maps Gemini role strings to OpenAI chat roles."
  (cond ((and role (string-equal role "model")) "assistant")
        ((and role (string-equal role "function")) "tool")
        ((and role (string-equal role "system")) "system")
        ((and role (string-equal role "user")) "user")
        (t "user")))

(defun gemini-part->openai-text (part)
  "Converts a Gemini part to OpenAI-compatible message text." 
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

(defun gemini-content->openai-message (content)
  "Converts a Gemini content object into one OpenAI chat message object."
  (let* ((parts (adapter-field content :parts "parts"))
         (part-list (adapter-as-list parts))
         (text-fragments (remove nil (mapcar #'gemini-part->openai-text part-list)))
         (text (if text-fragments
                   (str:join "\n\n" text-fragments)
                   "")))
    (object :role (gemini-role->openai-role (adapter-field content :role "role"))
            :content text)))

(defun gemini-tools->openai-tools (gemini-tools)
  "Translates Gemini function declarations into OpenAI tool descriptors."
  (let ((openai-tools nil))
    (dolist (tool (adapter-as-list gemini-tools))
      (let ((declarations (adapter-as-list (get-function-declarations tool))))
        (dolist (decl declarations)
          (push (object :type "function"
                        :function (object :name (get-name decl)
                                          :description (get-description decl)
                                          :parameters (or (get-parameters decl)
                                                          (get-parameters-json-schema decl))))
                openai-tools))))
    (nreverse openai-tools)))

(defun apply-gemini-generation-config-to-openai-payload (openai-payload generation-config)
  "Applies supported generation settings from Gemini config to OpenAI payload."
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
  openai-payload)

(defun openai-usage->gemini-usage (usage)
  "Normalizes OpenAI usage fields to Gemini usage metadata keys."
  (when usage
    (let* ((prompt-tokens (or (adapter-field usage "prompt_tokens" :prompt_tokens :prompt-tokens :prompt--tokens)
                              (adapter-field usage "promptTokens" :promptTokens)
                              (adapter-field usage "input_tokens" :input_tokens)))
           (completion-tokens (or (adapter-field usage "completion_tokens" :completion_tokens :completion-tokens :completion--tokens)
                                  (adapter-field usage "completionTokens" :completionTokens)
                                  (adapter-field usage "total_output_tokens" :response_tokens :responseTokens)))
           (details (adapter-field usage "completion_tokens_details" :completion_tokens_details :completion-tokens-details :completion--tokens--details))
           (reasoning-tokens (or (adapter-field usage "reasoning_tokens" :reasoning_tokens :reasoning-tokens :reasoning--tokens)
                                 (and (hash-table-p details)
                                      (adapter-field details "reasoning_tokens" :reasoning_tokens :reasoning-tokens :reasoning--tokens)))))
      (let ((usage-metadata (object)))
        (when prompt-tokens
          (setf (get-prompt-token-count usage-metadata) prompt-tokens))
        (when completion-tokens
          (setf (get-candidates-token-count usage-metadata)
                (if reasoning-tokens
                    (max 0 (- completion-tokens reasoning-tokens))
                    completion-tokens)))
        (when reasoning-tokens
          (setf (get-thoughts-token-count usage-metadata) reasoning-tokens))
        (unless (zerop (hash-table-count usage-metadata))
          usage-metadata)))))

(defun openai-choice->gemini-candidate (choice)
  "Converts one OpenAI choice object to one Gemini candidate object."
  (let* ((message (adapter-field choice "message" :message))
         (finish-reason (adapter-field choice "finish_reason" :finish_reason :finish--reason :finish-reason :finishReason))
         (role (openai-role->gemini-role (adapter-field message "role" :role)))
         (raw-content (adapter-field message "content" :content))
         (raw-thoughts (adapter-field message "reasoning_content" :reasoning_content :reasoning-content :reasoning--content :reasoning))
         (text (if (and (stringp raw-content) (not (string= "" raw-content)))
                   raw-content
                   nil))
         (tool-calls (adapter-field message "tool_calls" :tool_calls :tool-calls :tool--calls))
         (function-call-parts
           (mapcar
            (lambda (tool-call)
              (let* ((function-obj (adapter-field tool-call "function" :function))
                     (name (adapter-field function-obj "name" :name))
                     (arguments-str (adapter-field function-obj "arguments" :arguments))
                     (parsed-args (handler-case
                                      (with-decoder-jrm-semantics
                                        (cl-json:decode-json-from-string arguments-str))
                                    (error () (object)))))
                (part (function-call :name name :args parsed-args))))
            (adapter-as-list tool-calls)))
         (parts (remove nil
                        (append (list (when (and (stringp raw-thoughts)
                                                 (not (string= "" raw-thoughts)))
                                        (thought raw-thoughts))
                                      (when text
                                        (part text)))
                                function-call-parts))))
    (unless parts
      (setf parts (list (part ""))))
    (let ((candidate (object :content (content :role role :parts parts))))
      (when finish-reason
        (setf (get-finish-reason candidate) finish-reason))
      candidate)))

(defun openai-response-hash->gemini-response (data)
  "Converts decoded OpenAI response DATA into Gemini response + usage metadata values."
  (let* ((choices (or (adapter-field data "choices" :choices) #()))
         (candidates (mapcar #'openai-choice->gemini-candidate (adapter-as-list choices)))
         (response (object :candidates candidates))
         (usage-metadata (openai-usage->gemini-usage (adapter-field data "usage" :usage)))
         (model-name (or (adapter-field data "model" :model)
                         (adapter-field data "model_name" :model_name :model-name :model--name))))
    (when model-name
      (setf (get-model-version response) model-name))
    (values response usage-metadata)))
