;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +default-model+ "gemini-2.5-flash"
  "The default model to use for the Gemini API.
   This can be overridden by the MODEL keyword argument in `invoke-gemini`.")

(defparameter
  +gemini-api-base-url+
  "https://generativelanguage.googleapis.com/v1beta/models/"
  "The base URL for the Gemini API endpoints.")

(defun %invoke-gemini (model-id payload)
  "Invokes the Gemini API with the specified MODEL-ID and PAYLOAD.
   Returns the response from the API as a decoded JSON object.
   This is an internal helper function."
  (let ((response (dex:post (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
                            :headers `(("Content-Type" . "application/json")
                                       ("x-goog-api-key" . ,(google-api-key)))
                            :content (cl-json:encode-json-to-string payload))))
    (if (stringp response)
        (cl-json:decode-json-from-string response)
        (cl-json:decode-json-from-string
          (flex:octets-to-string response :external-format :utf-8)))))

(defun get-handler (name function-and-handler-list)
  (cdr (assoc name function-and-handler-list :test #'equal :key #'get-name)))

(defun default-function-declarations ()
  (if (boundp '*function-declarations*)
      *function-declarations*
      ;; Add default function declarations here
      ;; Example:
      (map 'list #'car (standard-functions-and-handlers))))

(defun default-tools ()
  "Returns the value of *TOOLS* if it is bound, otherwise NIL.
   Provides a default tools object for generation."
  (if (boundp '*tools*)
      *tools*
    (let ((tools (object)))
      (let ((function-declarations (default-function-declarations)))
        (when function-declarations
          (setf (get-function-declarations tools) function-declarations)))
      (unless (zerop (hash-table-count tools))
        tools))))

(defun default-generation-config ()
  "Returns a default generation configuration object.
   It constructs a hash table by combining various default settings
   related to candidate generation, safety, and response formatting."
  (if (boundp '*generation-config*)
      *generation-config*
      (let ((gen-config (object)))
        (let ((candidate-count (default-candidate-count)))
          (when candidate-count
            (setf (get-candidate-count gen-config) candidate-count)))
        (let ((enable-advanced-civic-answers (default-enable-advanced-civic-answers)))
          (when enable-advanced-civic-answers
            (setf (get-enable-advanced-civic-answers gen-config) enable-advanced-civic-answers)))
        (let ((frequency-penalty (default-frequency-penalty)))
          (when frequency-penalty
            (setf (get-frequency-penalty gen-config) frequency-penalty)))
        (let ((max-output-tokens (default-max-output-tokens)))
          (when max-output-tokens
            (setf (get-max-output-tokens gen-config) max-output-tokens)))
        (let ((media-resolution (default-media-resolution)))
          (when media-resolution
            (setf (get-media-Resolution gen-config) media-resolution)))
        (let ((presence-penalty (default-presence-penalty)))
          (when presence-penalty
            (setf (get-presence-penalty gen-config) presence-penalty)))
        (let ((response-logprobs (default-response-logprobs)))
          (when response-logprobs
            (setf (get-response-logprobs gen-config) response-logprobs)))
        (let ((logprobs (default-logprobs)))
          (when logprobs
            (assert (get-response-logprobs gen-config)
                    () "Response logprobs must be set when logprobs is set.")
            (setf (get-logprobs gen-config) logprobs)))
        (let ((response-mime-type (default-response-mime-type)))
          (when response-mime-type
            (setf (get-response-mime-type gen-config) response-mime-type)))
        (let ((response-modalities (default-response-modalities)))
          (when response-modalities
            (setf (get-response-modalities gen-config) response-modalities)))
        (let ((response-schema (default-response-schema)))
          (when response-schema
            (assert (get-response-mime-type gen-config)
                    () "Response MIME type must be set.")
            (setf (get-response-schema gen-config) response-schema)))
        (let ((response-json-schema (default-response-json-schema)))
          (when response-json-schema
            (assert (get-response-mime-type gen-config)
                    () "Response MIME type must be set.")
            (assert (not (get-response-schema gen-config))
                    () "Response schema must not be set when response JSON schema is set.")
            (setf (get-response-json-schema gen-config) response-json-schema)))
        (let ((seed (default-seed)))
          (when seed
            (setf (get-seed gen-config) seed)))
        (let ((speech-config (default-speech-config)))
          (when speech-config
            (setf (get-speech-config gen-config) speech-config)))
        (let ((stop-sequences (default-stop-sequences)))
          (when stop-sequences
            (setf (get-stop-Sequences gen-config) stop-sequences)))
        (let ((temperature (default-temperature)))
          (when temperature
            (setf (get-temperature gen-config) temperature)))
        (let ((thinking-config (default-thinking-config)))
          (when thinking-config
            (setf (get-thinking-config gen-config) thinking-config)))
        (let ((top-k (default-top-k)))
          (when top-k
            (setf (get-top-k gen-config) top-k)))
        (let ((top-p (default-top-p)))
          (when top-p
            (setf (get-top-p gen-config) top-p)))
        (unless (zerop (hash-table-count gen-config))
          gen-config))))

(defun default-process-part (part)
  "Processes a single part object. If it's a text object, it extracts
   and returns the text. Otherwise, it returns the part as is."
  (cond ((text-part? part) (get-text part))
        (t nil)))

(defun process-thought (thought)
  "Processes a thought part object.
   If the thought is a text part, it formats the text and outputs it to *trace-output*."
  (format *trace-output* "~&~a~%"
          (reflow-comment
           (mapcar #'str:trim
                   (str:split #\newline (get-text thought))))))

(defun process-thoughts (thoughts)
  "Processes a list of thought part objects.
   Each thought is processed by the `process-thought` function."
  (mapc #'process-thought thoughts))

(defun default-process-arg-value (arg schema)
  "Processes a single argument value based on the provided schema.
   Returns the processed value according to the type specified in the schema."
  (ecase (get-type schema)
    (:integer (if (integerp arg) arg (error "Expected integer, got ~s" arg)))
    (:string arg)))

(defun default-process-arg (arg schema)
  "Processes a single argument based on the provided schema.
   Returns a list containing the argument name and its processed value."
  (list (car arg)
        (default-process-arg-value
         (cdr arg)
         (funcall (object-ref-function (car arg)) schema))))
          
(defun default-process-args (args schema)
  "Processes a list of arguments based on the provided schema.
   Returns a list of processed arguments."
  (mappend (lambda (arg) (default-process-arg arg schema)) args))

(defun default-process-function-call (function-call-part)
  (let* ((name (get-name function-call-part))
         (args (get-args function-call-part))
         (functions (standard-functions-and-handlers))
         (entry (assoc name functions :key #'get-name :test #'equal))
         (schema (and entry
                      (get-properties
                       (get-parameters
                        (car entry)))))
         (handler (and entry (cdr entry))))
    (format *trace-output* "~&;; Processing function call: ~a~%" name)
    (object :function-response
            (function-response
             :name name
             :response (cond ((null entry)
                              (object :error (format nil "No entry for ~s." name)))
                             ((null handler)
                              (object :error (format nil "No handler for ~s." name)))
                             ((not (functionp handler))
                              (object :error (format nil "Handler for ~s is not a function." name)))
                             (t (object :result (apply handler (default-process-args args schema)))))))))

(defun default-process-function-calls (parts)
  "Processes a list of function call part objects.
   Returns a list of processed function call responses."
  (let ((function-calls
          (remove-if-not #'function-call-part? parts)))
    (mapc (lambda (part)
            (when (text-part? part)
              (format t "~&~a"
                      (get-text part))))
          (remove-if #'function-call-part? parts))
    (if (null function-calls)
        (error "No function calls found in parts: ~s" parts)
        (invoke-gemini
         (content :parts
                  (mapcar (lambda (part)
                            (default-process-function-call (get-function-call part)))
                          function-calls)
                  :role "function")))))

(defun default-process-content (content)
  "Processes a content object. If the role is 'model' and it contains
   a single part, it processes that part. Otherwise, it returns the
   content as is."
  (if (equal (get-role content) "model")
      (let* ((*history* (cons content *history*))
             (raw-parts (get-parts content))
             (thoughts (remove-if-not #'thought-part? raw-parts))
             (parts (remove-if #'thought-part? raw-parts)))
        (setq *prior-history* *history*)
        ;(format t "~&History: ~s~%" *history*)
        (process-thoughts thoughts)
        (cond ((some #'function-call-part? parts) (default-process-function-calls parts))
              ((every #'text-part? parts)
               (dolist (part (butlast parts) (get-text (car (last parts))))
                 (format t "~&~a~%" (get-text part))))
              ((singleton-list-of-parts? parts) (default-process-part (first parts)))
              (t parts)))
      content))

(defun default-process-candidate (candidate)
  "Processes a candidate object from the API response.
   Asserts that the 'finishReason' is 'STOP'.
   Then processes the content of the candidate."
  (unless (equal (get-finish-reason candidate) "STOP")
    (error "Invalid finish reason: ~s" candidate))
  (default-process-content (get-content candidate)))

(defun process-usage-metadata (usage-metadata)
  "Processes usage metadata from the API response.
   Outputs the usage information to *trace-output*."
  (format *trace-output* "~&;; Prompt Tokens:    ~7,' d~%~
                            ;; Thoughts Tokens:  ~7,' d~%~
                            ;; Candidate Tokens: ~7,' d~%~
                            ;; Total Tokens:     ~7,' d~%"
          (get-prompt-token-count usage-metadata)
          (or (get-thoughts-token-count usage-metadata) 0)
          (get-candidates-token-count usage-metadata)
          (get-total-token-count usage-metadata)))

(defun default-process-response (response)
  "Processes an API response object.
   If the response contains one candidate, process that candidate.
   Otherwise return the list of candidates."
  (if (gemini-response? response)
      (unwind-protect
           (let ((candidates (get-candidates response)))
             (if (singleton-list-of-candidates? candidates)
                 (default-process-candidate (car candidates))
                 candidates))
        (let ((usage-metadata (get-usage-metadata response)))
          (when usage-metadata
            (process-usage-metadata usage-metadata))))
      (error "Unrecognized Gemini response ~s" response)))

(defvar *output-processor* #'default-process-response
  "Function to process the output of the Gemini API.
   Can be set to a custom function to handle the response differently.
   Defaults to DEFAULT-PROCESS-RESPONSE.")

(defun invoke-gemini (contents &key
                      ((:model *model*) +default-model+)
                      (cached-content (default-cached-content))
                      (generation-config (default-generation-config))
                      (tools (default-tools))
                      (tool-config (default-tool-config))
                      (safety-settings (default-safety-settings))
                      (system-instruction (default-system-instruction)))
  "Invokes the Gemini API with the specified MODEL and CONTENTS.
   Optional arguments allow for custom CACHED-CONTENT, GENERATION-CONFIG,
   TOOLS, TOOL-CONFIG, SAFETY-SETTINGS, and SYSTEM-INSTRUCTION.
   The CONTENTS argument can be a content object, a part object, a string,
   a list of content objects, a list of part objects, or a list of strings.
   Returns the processed response from the API, as determined by
   *OUTPUT-PROCESSOR*."
  ;;(format t "~&Contents: ~s~%" (dehashify contents))
  (setq *prior-model* *model*)
  (let* ((*history*
           (cond ((content? contents) (cons contents *history*))
                 ((part? contents) (cons (content :parts (list contents) :role "user") *history*))
                 ((stringp contents) (cons (content :parts (list (part contents)) :role "user") *history*))
                 ((list-of-content? contents) (revappend contents *history*))
                 ((list-of-parts? contents) (cons (content :parts contents :role "user") *history*))
                 ((list-of-strings? contents) (cons (content :parts (mapcar #'part contents) :role "user") *history*))
                 (t (error "Unrecognized contents: ~s" contents))))
         (payload (object :contents (reverse *history*))))
    (setq *prior-history* *history*)
    (when cached-content
      (setf (get-cached-content payload) cached-content))
    (when generation-config
      (setf (get-generation-config payload) generation-config))
    (when safety-settings
      (setf (get-safety-settings payload) safety-settings))
    (when system-instruction
      (setf (get-system-instruction payload) system-instruction))
    (when tools
      (setf (get-tools payload) tools))
    (when tool-config
      (setf (get-tool-config payload) tool-config))
    (funcall *output-processor* (%invoke-gemini *model* payload))))

(defun gemini-continue (content)
  "Continues the conversation with the Gemini model using the provided CONTENT.
   CONTENT can be a content object, a part object, a string, a list of content objects,
   a list of part objects, or a list of strings.
   Returns the processed response from the API."
  (let ((*history* *prior-history*))
    (invoke-gemini content :model *prior-model*)))
