;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +default-model+ "gemini-2.5-flash"
  "The default model to use for the Gemini API.
   This can be overridden by the MODEL keyword argument in `invoke-gemini`.")

(defparameter +gemini-api-base-url+
  "https://generativelanguage.googleapis.com/v1beta/models/"
  "The base URL for the Gemini API endpoints.")

(defun %invoke-gemini (model-id payload)
  "Invokes the Gemini API with the specified MODEL-ID and PAYLOAD.
   Returns the response from the API as a decoded JSON object.
   This is an internal helper function."
  (let ((start-time (local-time:now))
        (aborted t))
    (unwind-protect
         (progn
           (format *trace-output* "~&;; Invoking Gemini API model `~a`...~%" model-id)
           (finish-output *trace-output*)
           (prog1 (google:google-post
                   (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
                   (google:gemini-api-key)
                   payload)
             (setq aborted nil)))
      (let ((elapsed-time (local-time:timestamp-difference (local-time:now) start-time)))
        (format *trace-output* "~&;; Gemini API ~:[finished in~;aborted after~] ~,2f seconds.~%" aborted
                elapsed-time)))))

(defun file->part (path &key (mime-type (guess-mime-type path)))
  "Reads a file from PATH, base64 encodes its content, and returns a content PART object
   suitable for the Gemini API."
  (part
   (object
    :data (file->blob path)
    :mime-type mime-type)))

(defun prepare-file-parts (files)
  "Converts a list of file specifications into a list of PART objects.
   Each element in FILES should be a path string or a list (PATH &optional MIME-TYPE)."
  (map nil (lambda (file-spec)
             (destructuring-bind (path &optional mime-type)
                    (if (listp file-spec) file-spec (list file-spec))
               (file->part path :mime-type mime-type)))
       files))

(defun merge-user-prompt-and-files (prompt-contents file-parts)
  "Merges file-parts into the first user content object in the prompt-contents list.
   If no user content exists, it creates one."
  (if (and prompt-contents (equal (get-role (car prompt-contents)) "user"))
      (let* ((first-user-content (car prompt-contents))
             (existing-parts (coerce (get-parts first-user-content) 'list))
             (new-parts (append existing-parts file-parts)))
        (list* (content :parts new-parts :role "user")
               (cdr prompt-contents)))
      ;; If the prompt was empty or non-user, create a new user content object.
      (list (content :parts (append file-parts (list (part "Please analyze the attached files."))) :role "user"))))

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
      (let ((gen-config (object )))
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
  (format *trace-output* "~&~%~{;; ~a~%~}~%"
          (reverse
           (let ((rev (reverse (mappend #'reflow-line (str:split #\newline (get-text thought))))))
                (if (and rev (string= "" (car rev)))
                    (cdr rev)
                    rev)))))

(defun process-thoughts (thoughts)
  "Processes a list of thought part objects.
   Each thought is processed by the `process-thought` function."
  (map nil #'process-thought thoughts))

(defun default-process-arg-value (arg schema)
  "Processes a single argument value based on the provided schema.
   Returns the processed value according to the type specified in the schema."
  (if (null schema)
      arg
      (ecase (get-type-enum schema)
        (:array (let ((item-schema (get-items schema)))
                  (map 'vector (lambda (item)
                                 (default-process-arg-value item item-schema))
                       arg)))
        (:boolean arg)
        (:integer (unless (integerp arg) (warn "Expected integer, got ~s" arg)) arg)
        (:number arg)
        (:object arg)
        (:string arg))))

(defun default-process-arg (arg schema)
  "Processes a single argument based on the provided schema.
   Returns a list containing the argument name and its processed value."
  (let ((name (car arg))
        (value (default-process-arg-value
                (cdr arg)
                (funcall (object-ref-function (car arg)) schema))))
    ;; (format t "~&;;      Processing arg: ~a = ~s~%" name value)
    (list name value)))
          
(defun default-process-args (args schema)
  "Processes a list of arguments based on the provided schema.
   Returns a list of processed arguments."
  (mappend (lambda (arg) (default-process-arg arg schema)) (hash-table-alist args)))

(defun default-process-function-call (function-call-part)
  (let* ((name (get-name function-call-part))
         (args (get-args function-call-part))
         (functions (standard-functions-and-handlers))
         (entry (assoc name functions :key #'get-name :test #'equal))
         (schema (and entry
                      (get-properties
                       (get-parameters
                        (car entry)))))
         (handler (and entry (cdr entry)))
         (arglist (default-process-args args schema)))
    ;; (format *trace-output* "~&;; Processing function call: ~s~%" (dehashify function-call-part))
    (format *trace-output* "~&;; Invoking function: ~a(~{~a~^, ~})~%" name arglist)
    (let ((response
            (object :function-response
                     (object 
                      :name name
                      :response (cond ((null entry)
                                       (object :error (format nil "Function `~s` does not exist." name)
                                                ))
                                      ((null handler)
                                       (object :error (format nil "Function `~s` has no handler." name)
                                                ))
                                      ((not (functionp handler))
                                       (object :error (format nil "Handler for `~s` is not a function." name)
                                                ))
                                      (t 
                                       (let ((answers nil)
                                             (output-string nil)
                                             (error-string nil))
                                         (handler-case
                                             (progn
                                               (setq output-string
                                                     (with-output-to-string (out)
                                                       (let ((*standard-output* out))
                                                         (setq error-string
                                                               (with-output-to-string (err)
                                                                 (let ((*error-output* err))
                                                                   (setq answers (multiple-value-list (apply handler arglist)))))))))
                                               (if (consp answers)
                                                   (if (consp (cdr answers))
                                                       (object :result (car answers)
                                                               :additional-results (coerce (cdr answers) 'vector)
                                                               :standard-output output-string
                                                               :error-output error-string)
                                                       (object :result (car answers)
                                                               :standard-output output-string
                                                               :error-output error-string))
                                                   (object :result jsonx:+json-null+
                                                           :standard-output output-string
                                                           :error-output error-string)))
                                           (error (e)
                                             (object :error (format nil "~a" e)
                                                     :standard-output output-string
                                                     :error-output error-string))))))))))
      ;; (format *trace-output* "~&;; Function call response: ~s~%" (dehashify response))
      response)))

(defvar *accumulated-prompt-tokens* 0
  "Accumulated prompt token count across multiple API calls.")
(defvar *accumulated-response-tokens* 0
  "Accumulated response token count across multiple API calls.")

(defun process-usage-metadata (usage-metadata)
  "Processes usage metadata from the API response.
   Outputs the usage information to *trace-output*."
  (incf *accumulated-prompt-tokens* (get-prompt-token-count usage-metadata))
  (incf *accumulated-response-tokens* (or (get-thoughts-token-count usage-metadata) 0))
  (incf *accumulated-response-tokens* (or (get-candidates-token-count usage-metadata) 0))
  (format *trace-output* "~&;; Prompt Tokens:      ~9,' :d~%~
                            ;; Thoughts Tokens:    ~9,' :d~%~
                            ;; Candidate Tokens:   ~9,' :d~%~
                            ;; Accumulated Prompt Tokens:   ~12,' :d~%~
                            ;; Accumulated Response Tokens: ~12,' :d~%"
          (get-prompt-token-count usage-metadata)
          (or (get-thoughts-token-count usage-metadata) 0)
          (or (get-candidates-token-count usage-metadata) 0)
          *accumulated-prompt-tokens*
          *accumulated-response-tokens*))

(defun ->prompt (thing)
  "Converts a thing into a list of content objects."
  (cond ((content? thing) (list thing))
        ((part? thing) (list (content :parts (list thing) :role "user")))
        ((stringp thing) (list (content :parts (list (part thing)) :role "user")))
        ((list-of-content? thing) thing)
        ((list-of-parts? thing) (list (content :parts thing :role "user")))
        ((list-of-strings? thing)
         (list (content :parts (mapcar #'part thing) :role "user")))
        (t (error "Unrecognized type for prompt: ~s" thing))))
  
(defparameter +max-prompt-tokens+ (expt 2 18)
  "The maximum number of tokens allowed in the prompt context before compression is needed.")

(defun %count-tokens (model-id payload)
  "Invokes the Gemini API's countTokens endpoint."
  (google:google-post
   (concatenate 'string +gemini-api-base-url+ model-id ":countTokens")
   (google:gemini-api-key)
   payload))

(defun compress-context (&optional (model *model*))
  "Compresses the global *context* variable by summarizing its middle parts."
  (let* ((prompt (car *context*))
         (header (car (last *context*)))
         (history-to-summarize (butlast (cdr *context*))))

    (when history-to-summarize
      (assert (string= (get-role prompt) "user"))
      (assert (string= (get-role header) "model"))

      (format *trace-output* "~&;; Compressing ~d messages.~%" (length history-to-summarize))

      (let ((summarization-payload
              (object
               :contents (reverse history-to-summarize)
               :system-instruction (content
                                    :parts (list (part "You are a world class summarizer. Your summaries are concise, but thorough, preserving important details and key points. You use bullet points where appropriate."))
                                    :role "system")
               :generation-config (object :max-output-tokens (- +max-prompt-tokens+ 2048)))))
        (let retry ((temp 0.0))
          (setf (get-temperature (get-generation-config summarization-payload)) temp)
          (let* ((summary-results (%invoke-gemini model summarization-payload))
                 (error (get-error summary-results)))
            (if error
                (if (< temp 2.0)
                    (progn
                      (warn "Summarization failed at temp ~a. Retrying. Error: ~a" temp (get-message error))
                      (retry (+ temp 0.25)))
                    (error "Failed to compress context even at max temperature."))
                (let* ((candidates (get-candidates summary-results))
                       (first-candidate (when (and candidates (> (length (coerce candidates 'list)) 0)) (elt (coerce candidates 'list) 0)))
                       (compressed-content (when first-candidate (get-content first-candidate))))
                  (if compressed-content
                      (progn
                        (format *trace-output* "~&;; Context successfully compressed.~%")
                        (setf (get-role compressed-content) "model")
                        (setq *context* (list prompt compressed-content header)))
                      (error "Summarization produced no content."))))))))))

(defun extend-conversation (new-content)
  (push new-content *context*)
  (setq *prior-context* *context*)
  (save-transcript *context*)
  new-content)

(defun extend-conversation-with-first-candidate (results)
  "Extends the conversation context with the first candidate from the results.
   Returns the results unchanged."
  (let ((candidates (get-candidates results)))
    (when candidates
      (let ((first-candidate (if (consp candidates)
                                 (car candidates)
                                 (and (vectorp candidates)
                                      (> (length candidates) 0)
                                      (svref candidates 0)))))
        (when first-candidate
          (let ((content (get-content first-candidate)))
            (when content
              (extend-conversation content)))))))
  results)

(defun print-token-usage (results)
  "Prints the token usage information from the results.
     Returns the results unchanged."
  (let ((usage-metadata (get-usage-metadata results)))
    (when usage-metadata
      (process-usage-metadata usage-metadata)))
  results)

(defun strip-thoughts-from-part (part)
  (if (thought-part? part)
      (progn (process-thought part)
             nil)
      part))

(defun strip-thoughts-from-parts (parts)
  (remove nil (map 'list #'strip-thoughts-from-part parts)))

(defun strip-thoughts-from-content (content)
  (let* ((parts* (strip-thoughts-from-parts (get-parts content))))
    (when parts*
      (let ((stripped (object :parts parts*)))
        (when (get-role content)
          (setf (get-role stripped) (get-role content)))
        stripped))))

(defun strip-thoughts-from-candidate (candidate)
  (let ((content* (strip-thoughts-from-content (get-content candidate))))
    (when content*
      (let ((stripped (object :content content*)))
        (when (get-finish-reason candidate)
          (setf (get-finish-reason stripped) (get-finish-reason candidate)))
        (when (get-index candidate)
          (setf (get-index stripped) (get-index candidate)))
        (when (get-citation-metadata candidate)
          (setf (get-citation-metadata stripped) (get-citation-metadata candidate)))
        stripped))))

(defun strip-thoughts-from-candidates (candidates)
  (remove nil (map 'list #'strip-thoughts-from-candidate candidates)))

(defun strip-and-print-thoughts (results)
  (let ((candidates* (strip-thoughts-from-candidates (get-candidates results))))
    (when candidates*
      (let ((stripped (object :candidates candidates*)))
        (when (get-model-version results)
          (setf (get-model-version stripped) (get-model-version results)))
        (when (get-response-id results)
          (setf (get-response-id stripped) (get-response-id results)))
        (when (get-usage-metadata results)
          (setf (get-usage-metadata stripped) (get-usage-metadata results)))
        stripped))))

(defun print-text (results)
  "Prints the text parts from the results to *trace-output*.
   Returns the results unchanged."
  (let ((candidates (get-candidates results)))
    (when candidates
      (dolist (candidate (if (consp candidates)
                             candidates
                             (and (vectorp candidates)
                                  (> (length candidates) 0)
                                  (coerce candidates 'list))))
        (let ((content (get-content candidate)))
          (when content
            (dolist (part (coerce (get-parts content) 'list))
              (when (text-part? part)
                (format *trace-output* "~&~a~%" (get-text part)))))))))
  results)

(defun extract-function-calls-from-candidate (candidate)
  (let ((content (get-content candidate)))
    (when content
      (remove-if-not #'function-call-part? (coerce (get-parts content) 'list)))))

(defun extract-function-calls-from-results (results)
  "Extracts function calls from the results.
   Returns a list of function call parts if present, otherwise NIL."
  (let ((candidates (get-candidates results)))
    (cond ((and (consp candidates)
                (null (cdr candidates)))
           (extract-function-calls-from-candidate (car candidates)))
          ((and (vectorp candidates)
                (= (length candidates) 1))
           (extract-function-calls-from-candidate (svref candidates 0))))))

(defun candidate-as-text-string (candidate)
  (let ((content (get-content candidate)))
    (and content
         (let ((parts (coerce (get-parts content) 'list)))
           (apply #'concatenate 'string (map 'list #'get-text (remove-if-not #'text-part? parts)))))))

(defun as-singleton-text-string (results)
  (let ((candidates (get-candidates results)))
    (cond ((and (consp candidates)
                (null (cdr candidates)))
           (candidate-as-text-string (car candidates)))
          ((and (vectorp candidates)
                (= (length candidates) 1))
           (candidate-as-text-string (svref candidates 0))))))

(defun tail-call-functions (results)
  (let ((function-calls (extract-function-calls-from-results results)))
    (if function-calls
        (let ((function-results (map 'list (compose #'default-process-function-call #'get-function-call)
                                     function-calls)))
          (assert (list-of-parts? function-calls) () "Expected function-calls to be a list of parts.")
          (assert (list-of-parts? function-results) () "Expected function-results to be a list of parts.")
          (invoke-gemini
           (list (content :parts function-results
                          :role "function"))))
        (or (and *return-text-string* (as-singleton-text-string results))
            results))))

(defun error-check (results)
  (if (get-error results)
      (error "Error from Gemini (code ~d): ~a"
             (get-code (get-error results))
             (get-message (get-error results)))
      results))

(defun debug-response (response)
  (format *trace-output* "~&;; Full response: ~s~%" (dehashify response))
  response)

(defparameter *output-processor* (compose #'tail-call-functions
                                          #'print-text
                                          #'strip-and-print-thoughts
                                          #'print-token-usage
                                          #'extend-conversation-with-first-candidate)
  "The default output processor for the Gemini API.")

(defun current-context ()
  (if (null *context*)
      (list (content :parts (list (part (format nil "**The following is conversation #~d.**" (get-universal-time)))
                                  (part (format nil "**The topic of conversation is ~a.**" *conversation-topic*)))
                     :role "model"))
      *context*))

(defun current-topic ()
  (let* ((context (or *context* *prior-context*))
         (first-message (car (last context)))
         (parts (and first-message (get-parts first-message)))
         (last-part (and parts (car (last (coerce parts 'list))))))
    (and last-part (get-text last-part))))

(defun (setf current-topic) (new-topic)
  (let* ((context (or *context* *prior-context*))
         (first-message (car (last context)))
         (parts (and first-message (get-parts first-message)))
         (last-part (and parts (car (last (coerce parts 'list))))))
    (when last-part
      (setf (get-text last-part)
            (format nil "**The topic of conversation is ~a.**" new-topic)))))

(defun invoke-gemini (prompt &key
                               ((:model *model*) +default-model+)
                               (files nil)
                               (cached-content (default-cached-content))
                               (generation-config (default-generation-config))
                               (tools (default-tools))
                               (tool-config (default-tool-config))
                               (safety-settings (default-safety-settings))
                               (system-instruction (default-system-instruction)))
  (let* ((file-parts (when files (prepare-file-parts files)))
         (prompt-contents-base (->prompt prompt))
         (prompt-contents (if file-parts
                              (merge-user-prompt-and-files prompt-contents-base file-parts)
                              prompt-contents-base)))
    (assert (list-of-content? prompt-contents)
            () "Prompt must be a list of content objects.")
    (setq *prior-model* *model*)
    (let* ((*context* (revappend prompt-contents (current-context)))
           (payload (object :contents (reverse *context*))))
      ;; Add other payload parts before counting tokens
      (when cached-content (setf (get-cached-content payload) cached-content))
      (when generation-config (setf (get-generation-config payload) generation-config))
      (when safety-settings (setf (get-safety-settings payload) safety-settings))
      ;; (when system-instruction (setf (get-system-instruction payload) system-instruction))
      ;; (when tools (setf (get-tools payload) tools))
      ;; (when (and tools tool-config) (setf (get-tool-config payload) tool-config))

      ;; Check token count and compress if necessary
      (let* ((token-count-response (%count-tokens *model* payload))
             (total-tokens (get-total-tokens token-count-response)))
        (when (> total-tokens +max-prompt-tokens+)
          (format *trace-output* "~&;; Prompt token count (~d) exceeds ~d tokens, compressing context.~%" total-tokens +max-prompt-tokens+)
          (compress-context *model*)
          ;; Rebuild payload with compressed context
          (setq payload (object :contents (reverse *context*)))
          ;; Re-add other payload parts
          (when cached-content (setf (get-cached-content payload) cached-content))
          (when generation-config (setf (get-generation-config payload) generation-config))
          (when safety-settings (setf (get-safety-settings payload) safety-settings))))
      (when system-instruction (setf (get-system-instruction payload) system-instruction))
      (when tools (setf (get-tools payload) tools))
      (when (and tools tool-config) (setf (get-tool-config payload) tool-config))

      (save-transcript *context*)
      (setq *prior-context* *context*)
      (or (funcall (compose *output-processor* #'error-check) (%invoke-gemini *model* payload))
          (reinvoke-gemini)))))

(defun reinvoke-gemini (&key
                          (reinvocation-count 0)
                          ((:model *model*) +default-model+)
                          (cached-content (default-cached-content))
                          (generation-config (default-generation-config))
                          (tools (default-tools))
                          (tool-config (default-tool-config))
                          (safety-settings (default-safety-settings))
                          (system-instruction (default-system-instruction)))
  (let ((payload (object :contents (reverse *prior-context*)))
        (*temperature* (+ (if (boundp '*temperature*)
                              (symbol-value '*temperature*)
                              1)
                          (/ (- 2.0 (if (boundp '*temperature*)
                                        (symbol-value '*temperature*)
                                        1))
                             8.0))))
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
    (when (and tools tool-config)
      (setf (get-tool-config payload) tool-config))
    (or (funcall (compose *output-processor* #'error-check) (%invoke-gemini *model* payload))
        (reinvoke-gemini :reinvocation-count (1+ reinvocation-count)))))

(defun continue-gemini (content)
  "Continues the conversation with the Gemini model using the provided CONTENT.
   CONTENT can be a content object, a part object, a string, a list of content objects,
   a list of part objects, or a list of strings.
   Returns the processed response from the API."
  (let ((*context* *prior-context*))
    (invoke-gemini content :model *prior-model*)))
