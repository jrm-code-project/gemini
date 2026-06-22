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
  "Performs detailed preflight checks on Gemini-style payload structure, required fields, and types."
  (labels ((v-error (path fmt &rest args)
             (error "Payload validation error at ~a: ~?" path fmt args))
           
           (check-string (value path field-name)
             (unless (stringp value)
               (v-error path "field ~a must be a string, got ~s" field-name value)))
           
           (check-integer (value path field-name &key min)
             (unless (integerp value)
               (v-error path "field ~a must be an integer, got ~s" field-name value))
             (when (and min (< value min))
               (v-error path "field ~a must be >= ~d, got ~d" field-name min value)))
               
           (check-number (value path field-name &key min max)
             (unless (numberp value)
               (v-error path "field ~a must be a number, got ~s" field-name value))
             (when (and min (< value min))
               (v-error path "field ~a must be >= ~f, got ~f" field-name min value))
             (when (and max (> value max))
               (v-error path "field ~a must be <= ~f, got ~f" field-name max value)))

           (validate-blob (blob path)
             (unless (or (hash-table-p blob) (consp blob))
               (v-error path "blob must be an object, got ~s" (type-of blob)))
             (let ((mime-type (adapter-field blob :mime-type "mimeType"))
                   (data (adapter-field blob :data "data")))
               (unless mime-type
                 (v-error path "blob is missing required field 'mimeType'"))
               (check-string mime-type (format nil "~a.mimeType" path) "mimeType")
               (unless data
                 (v-error path "blob is missing required field 'data'"))
               (check-string data (format nil "~a.data" path) "data")))

           (validate-file-data (fd path)
             (unless (or (hash-table-p fd) (consp fd))
               (v-error path "fileData must be an object, got ~s" (type-of fd)))
             (let ((file-uri (adapter-field fd :file-uri "fileUri"))
                   (mime-type (adapter-field fd :mime-type "mimeType")))
               (unless file-uri
                 (v-error path "fileData is missing required field 'fileUri'"))
               (check-string file-uri (format nil "~a.fileUri" path) "fileUri")
               (when mime-type
                 (check-string mime-type (format nil "~a.mimeType" path) "mimeType"))))

           (validate-function-call (fc path)
             (unless (or (hash-table-p fc) (consp fc))
               (v-error path "functionCall must be an object, got ~s" (type-of fc)))
             (let ((name (adapter-field fc :name "name"))
                   (args (adapter-field fc :args "args")))
               (unless name
                 (v-error path "functionCall is missing required field 'name'"))
               (check-string name (format nil "~a.name" path) "name")
               (when args
                 (unless (or (hash-table-p args) (consp args))
                   (v-error (format nil "~a.args" path) "function args must be an object/alist, got ~s" (type-of args))))))

           (validate-function-response (fr path)
             (unless (or (hash-table-p fr) (consp fr))
               (v-error path "functionResponse must be an object, got ~s" (type-of fr)))
             (let ((name (adapter-field fr :name "name"))
                   (response (adapter-field fr :response "response")))
               (unless name
                 (v-error path "functionResponse is missing required field 'name'"))
               (check-string name (format nil "~a.name" path) "name")
               (unless response
                 (v-error path "functionResponse is missing required field 'response'"))
               (unless (or (hash-table-p response) (consp response))
                 (v-error (format nil "~a.response" path) "function response must be an object/alist, got ~s" (type-of response)))))

           (validate-part (part path)
             (unless (or (hash-table-p part) (consp part))
               (v-error path "part must be an object, got ~s" (type-of part)))
             (let ((text (adapter-field part :text "text"))
                   (inline-data (adapter-field part :inline-data "inlineData"))
                   (file-data (adapter-field part :file-data "fileData"))
                   (function-call (adapter-field part :function-call "functionCall"))
                   (function-response (adapter-field part :function-response "functionResponse")))
               (cond (text
                      (check-string text (format nil "~a.text" path) "text"))
                     (inline-data
                      (validate-blob inline-data (format nil "~a.inlineData" path)))
                     (file-data
                      (validate-file-data file-data (format nil "~a.fileData" path)))
                     (function-call
                      (validate-function-call function-call (format nil "~a.functionCall" path)))
                     (function-response
                      (validate-function-response function-response (format nil "~a.functionResponse" path)))
                     (t
                      (v-error path "part must contain one of: text, inlineData, fileData, functionCall, functionResponse")))))

           (validate-content (content path)
             (unless (or (hash-table-p content) (consp content))
               (v-error path "content entry must be an object, got ~s" (type-of content)))
             (let ((role (adapter-field content :role "role"))
                   (parts (adapter-field content :parts "parts")))
               (when role
                 (check-string role (format nil "~a.role" path) "role"))
               (unless parts
                 (v-error path "content entry is missing required field 'parts'"))
               (let ((part-list (adapter-as-list parts)))
                 (unless part-list
                   (v-error (format nil "~a.parts" path) "parts list cannot be empty"))
                 (loop for part in part-list
                       for i from 0
                       do (validate-part part (format nil "~a.parts[~d]" path i))))))

           (validate-generation-config (config path)
             (unless (or (hash-table-p config) (consp config))
               (v-error path "generationConfig must be an object, got ~s" (type-of config)))
             (let ((temp (adapter-field config :temperature "temperature"))
                   (top-p (adapter-field config :top-p "topP"))
                   (top-k (adapter-field config :top-k "topK"))
                   (max-tokens (adapter-field config :max-output-tokens "maxOutputTokens"))
                   (candidate-count (adapter-field config :candidate-count "candidateCount"))
                   (stop-seqs (adapter-field config :stop-sequences "stopSequences")))
               (when temp (check-number temp (format nil "~a.temperature" path) "temperature" :min 0.0 :max 2.0))
               (when top-p (check-number top-p (format nil "~a.topP" path) "topP" :min 0.0 :max 1.0))
               (when top-k (check-integer top-k (format nil "~a.topK" path) "topK" :min 1))
               (when max-tokens (check-integer max-tokens (format nil "~a.maxOutputTokens" path) "maxOutputTokens" :min 1))
               (when candidate-count (check-integer candidate-count (format nil "~a.candidateCount" path) "candidateCount" :min 1))
               (when stop-seqs
                 (let ((seq-list (adapter-as-list stop-seqs)))
                   (loop for seq in seq-list
                         for i from 0
                         do (check-string seq (format nil "~a.stopSequences[~d]" path i) "stopSequence")))))))

    ;; Top level checks
    (unless (or (hash-table-p payload) (consp payload))
      (v-error "<root>" "Payload must be a hash-table or alist object, got ~s" (type-of payload)))
    
    (let ((contents (adapter-field payload :contents "contents"))
          (gen-config (adapter-field payload :generation-config "generationConfig"))
          (sys-inst (adapter-field payload :system-instruction "systemInstruction")))
      
      (when contents
        (let ((content-list (adapter-as-list contents)))
          (loop for entry in content-list
                for i from 0
                do (validate-content entry (format nil "contents[~d]" i)))))
              
      (when gen-config
        (validate-generation-config gen-config "generationConfig"))
        
      (when sys-inst
        (validate-content sys-inst "systemInstruction")))))

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

(defun openai-schema-type-string (type)
  "Normalizes a schema type value to the lowercase string expected by OpenAI tool schemas."
  (cond ((integerp type)
         (ecase type
           (0 "unspecified")
           (1 "string")
           (2 "number")
           (3 "integer")
           (4 "boolean")
           (5 "array")
           (6 "object")))
        ((keywordp type)
         (string-downcase (symbol-name type)))
        ((symbolp type)
         (string-downcase (symbol-name type)))
        ((stringp type)
         (string-downcase type))
        (t type)))

(defun openai-required-name (item)
  "Normalizes a REQUIRED entry to the string form expected by OpenAI tool schemas."
  (cond ((and (vectorp item)
              (= (length item) 1))
         (openai-required-name (aref item 0)))
        ((and (consp item)
              (null (cdr item)))
         (openai-required-name (car item)))
        ((keywordp item)
         (keyword->keystring item))
        ((symbolp item)
         (keyword->keystring (->keyword item)))
        ((stringp item)
         (if (position #\- item)
             (keyword->keystring (->keyword item))
             item))
        (t
         (keyword->keystring (->keyword (format nil "~a" item))))))

(defun openai-required-vector (required)
  "Flattens REQUIRED into a vector of OpenAI schema field names."
  (labels ((flatten-required (value)
             (cond ((null value)
                    nil)
                   ((stringp value)
                    (list (openai-required-name value)))
                   ((vectorp value)
                    (loop for entry across value append (flatten-required entry)))
                   ((listp value)
                    (loop for entry in value append (flatten-required entry)))
                   (t
                    (list (openai-required-name value))))))
    (coerce (flatten-required required) 'vector)))

(defun normalize-openai-schema (schema)
  "Recursively converts an internal schema object to an OpenAI-compatible JSON schema."
  (cond ((hash-table-p schema)
         (let ((normalized (object)))
           (maphash (lambda (key value)
                      (setf (gethash key normalized)
                            (cond ((or (equal key :type)
                                       (equal key "type"))
                                   (openai-schema-type-string value))
                                  ((or (equal key :required)
                                       (equal key "required"))
                                   (openai-required-vector value))
                                  (t
                                   (normalize-openai-schema value)))))
                    schema)
           normalized))
        ((consp schema)
         (mapcar (lambda (entry)
                   (if (consp entry)
                       (cons (car entry)
                             (cond ((or (equal (car entry) :type)
                                        (equal (car entry) "type"))
                                    (openai-schema-type-string (cdr entry)))
                                   ((or (equal (car entry) :required)
                                        (equal (car entry) "required"))
                                    (openai-required-vector (cdr entry)))
                                   (t
                                    (normalize-openai-schema (cdr entry)))))
                       entry))
                 schema))
        ((vectorp schema)
         (map 'vector #'normalize-openai-schema schema))
        (t schema)))

(defun gemini-tools->openai-tools (gemini-tools)
  "Translates Gemini function declarations into OpenAI tool descriptors."
  (let ((openai-tools nil))
    (dolist (tool (adapter-as-list gemini-tools))
      (let ((declarations (adapter-as-list (get-function-declarations tool))))
        (dolist (decl declarations)
          (push (object :type "function"
                        :function (object :name (get-name decl)
                                          :description (get-description decl)
                                          :parameters (normalize-openai-schema
                                                       (or (get-parameters-json-schema decl)
                                                           (get-parameters decl)))))
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
