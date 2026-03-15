;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defparameter +default-model+ "models/gemini-flash-latest"
  "The default model to use for the Gemini API.
   This can be overridden by the MODEL keyword argument in `invoke-gemini`.")

(defparameter +gemini-api-base-url+
  "https://generativelanguage.googleapis.com/v1beta/"
  "The base URL for the Gemini API endpoints.")

(defun list-models (&optional page-token)
  "Lists available models from the Gemini API."
  (let ((response
          (google:google-get
           (format nil "~amodels~@[?pageToken=~a~]" +gemini-api-base-url+ page-token)
           (google:gemini-api-key))))
    (append (coerce (gethash :models response) 'list)
            (let ((next-page-token (gethash :next-page-token response)))
              (when next-page-token
                (list-models next-page-token))))))

(defparameter +count-tokens-timeout+ (* 60 2) ;; two minutes
  "The timeout in seconds for counting tokens in the prompt.")

(defun %count-tokens (model-id payload)
  "Invokes the Gemini API's countTokens endpoint."
  (with-timeout (+count-tokens-timeout+)
    (report-elapsed-time (format nil "Token counting for model `~a`" model-id)
      (google:google-post
       (concatenate 'string +gemini-api-base-url+ model-id ":countTokens")
       (google:gemini-api-key)
       payload))))

(defparameter *next-invoke-gemini-time* (get-universal-time)
  "The next time at which an invoke-gemini call is allowed.
   Used to enforce rate limiting.")

(defun gemini-rate-limit ()
  (let iter ((sleep-time (- *next-invoke-gemini-time* (get-universal-time))))
    (when (> sleep-time 0)
      (report-elapsed-time (format nil "Waiting ~d seconds to respect Gemini API rate limits" sleep-time)
        (sleep sleep-time))
      (iter (- *next-invoke-gemini-time* (get-universal-time))))))

(defun %%invoke-gemini (model-id payload)
  "Invokes the Gemini API with the specified MODEL-ID and PAYLOAD.
   Returns the response from the API as a decoded JSON object.
   This is an internal helper function."
  (let iter ((sleep-time (- *next-invoke-gemini-time* (get-universal-time))))
    (when (> sleep-time 0)
      (report-elapsed-time (format nil "Waiting ~d seconds to respect Gemini API rate limits" sleep-time)
        (sleep sleep-time))
      (iter (- *next-invoke-gemini-time* (get-universal-time)))))

  (report-elapsed-time (format nil "Gemini API model `~a`" model-id)
    (multiple-value-prog1
        (google:google-post
         (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
         (google:gemini-api-key)
         payload)
      (setq *next-invoke-gemini-time*
            (+ (get-universal-time) 60.0))))) ;; Enforce 10 second delay between calls

#||
(defun classify-prompt (prompt)
  (let iter ((sleep-time (- *next-invoke-gemini-time* (get-universal-time))))
    (when (> sleep-time 0)
      (report-elapsed-time (format nil "Waiting ~d seconds to respect Gemini API rate limits" sleep-time)
        (sleep sleep-time))
      (iter (- *next-invoke-gemini-time* (get-universal-time)))))
  (%%invoke-gemini "models/gemini-flash-lite-latest" ...))
||#

(defun file->part (path &key (mime-type (guess-mime-type path)))
  "Reads a file from PATH, base64 encodes its content, and returns a content PART object
   suitable for the Gemini API."
  (when (probe-file path)
    (ignore-errors
     (let ((mime-type* (if (string-equal "application/json" mime-type) ;; Gemini bug.
                           "text/plain"
                           mime-type)))
       (if (str:starts-with? "text/" mime-type*)
           (part (uiop:read-file-string path))
           (part
            (object
             :data (file->blob path)
             :mime-type mime-type*)))))))

(defun expand-pathname (file)
  (cond ((consp file) (list file))
        ((wild-pathname-p file) (directory file))
        (t (list file))))

(defun expand-pathnames (files)
  "Expands a list of file path specifications into absolute pathnames.
   Each element in FILES can be a string or a list where the first element is the path."
  (mappend #'expand-pathname files))

(defun prepare-file-parts (files)
  "Converts a list of file specifications into a list of PART objects.
   Each element in FILES should be a path string or a list (PATH &optional MIME-TYPE)."
  (remove nil
          (map 'list (lambda (file-spec)
                       (destructuring-bind (path &optional mime-type)
                           (if (listp file-spec) file-spec (list file-spec))
                         (if mime-type
                             (file->part path :mime-type mime-type)
                             (file->part path))))
               (expand-pathnames files))))

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

(defun generation-config (&key
                            (candidate-count (default-candidate-count))
                            (enable-advanced-civic-answers (default-enable-advanced-civic-answers))
                            (frequency-penalty (default-frequency-penalty))
                            (max-output-tokens (default-max-output-tokens))
                            (media-resolution (default-media-resolution))
                            (presence-penalty (default-presence-penalty))
                            (response-logprobs (default-response-logprobs))
                            (logprobs (default-logprobs))
                            (response-mime-type (default-response-mime-type))
                            (response-modalities (default-response-modalities))
                            (response-schema (default-response-schema))
                            (response-json-schema (default-response-json-schema))
                            (seed (default-seed))
                            (speech-config (default-speech-config))
                            (stop-sequences (default-stop-sequences))
                            (temperature (default-temperature))
                            (thinking-config (default-thinking-config))
                            (top-k (default-top-k))
                            (top-p (default-top-p)))
  (let ((generation-config (object)))
    (macrolet ((init (field)
                 (let ((getter (intern (format nil "~:@(get-~a~)" (symbol-name field)) (find-package "GEMINI"))))
                   `(WHEN ,field (SETF (,getter GENERATION-CONFIG) ,field)))))
      (init candidate-count)
      (init enable-advanced-civic-answers)
      (init frequency-penalty)
      (init max-output-tokens)
      (init media-resolution)
      (init presence-penalty)
      (init response-logprobs)
      (init logprobs)
      (init response-mime-type)
      (init response-modalities)
      (init response-schema)
      (init response-json-schema)
      (init seed)
      (init speech-config)
      (init stop-sequences)
      (init temperature)
      (init thinking-config)
      (init top-k)
      (init top-p)
      (when (get-logprobs generation-config)
        (assert (get-response-logprobs generation-config)
                () "Response logprobs must be set when logprobs is set."))
      (when (get-response-schema generation-config)
        (assert (get-response-mime-type generation-config)
                () "Response MIME type must be set when response schema is set."))
      (when (get-response-json-schema generation-config)
        (assert (get-response-mime-type generation-config)
                () "Response MIME type must be set when response JSON schema is set.")
        (assert (not (get-response-schema generation-config))
                () "Response schema must not be set when response JSON schema is set."))
      (unless (zerop (hash-table-count generation-config))
        generation-config))))

(defun default-generation-config ()
  "Returns a default generation configuration object.
   It constructs a hash table by combining various default settings
   related to candidate generation, safety, and response formatting."
  (if (boundp '*generation-config*)
      *generation-config*
      (generation-config)))

(defun process-thought (thought)
  "Processes a thought part object.
   If the thought is a text part, it formats the text and outputs it to *trace-output*."
  (format *trace-output* "~&~%~{;; ~a~%~}~%"
          (reverse
           (let ((rev (reverse (mappend #'reflow-line (str:split #\newline (get-text thought))))))
                (if (and rev (string= "" (car rev)))
                    (cdr rev)
                    rev)))))

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

(defparameter *function-call-aliases*
  '(("sequential_thinking" . "sequentialthinking"))
  "A list of (ALIAS . FUNCTION-NAME) pairs for function call name normalization.")

(defun resolve-function-call-alias (name)
  "Resolves a function call name to its canonical name using *function-call-aliases*."
  (or (cdr (assoc name *function-call-aliases* :test #'equal))
      name))

(defparameter *trace-function-calls* t
  "If true, function calls will be traced to *trace-output*.")

(defun default-process-function-call (content-generator)
  (lambda (function-call-part)
    (let* ((name (resolve-function-call-alias (get-name function-call-part)))
           (args (get-args function-call-part))
           (functions (standard-functions-and-handlers content-generator))
           (entry (assoc name functions :key #'get-name :test #'equal))
           (schema (and entry
                        (get-properties
                         (get-parameters
                          (car entry)))))
           (handler (and entry (cdr entry)))
           (arglist (default-process-args args schema)))
      (when *trace-function-calls*
        (format *trace-output* "~&;; Invoking function: ~a(~{~s~^, ~})~%" name arglist)
        (force-output *trace-output*))
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
                                                        (let ((*standard-output* (make-broadcast-stream *standard-output* out)))
                                                          (setq error-string
                                                                (with-output-to-string (err)
                                                                  (let ((*error-output* (make-broadcast-stream *error-output* err)))
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
        (when *trace-function-calls*
          (format *trace-output* "~&;; Function call response: ~s~%" (dehashify response))
          (force-output *trace-output*))
        response))))

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

(defparameter *include-model* nil 
  "If true, includes the model part in the prompt content.")

(defparameter *include-timestamp* nil 
  "If true, includes a timestamp part in the prompt content.")

(defun prompt-timestamp ()
  (multiple-value-bind (sec min hour day month year day-of-week) (decode-universal-time (get-universal-time))
    (declare (ignore sec year))
    (format nil "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~[~;Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~d, ~d:~2,'0d~%" 
            day-of-week month day hour min)))

(defparameter *include-bash-history* nil
  "If true, includes the shell log part in the prompt content.")

(defun calculate-string-entropy (s)
  "Calculate the Shannon entropy of string S in bits per character."
  (let ((len (length s)))
    (if (zerop len) 
        0.0
        (let* ((chars (coerce s 'list))
               ;; Build the frequency map using fold-left
               (freq-map (fold-left (lambda (acc char)
                                      (incf (gethash char acc 0))
                                      acc)
                                    (make-hash-table)
                                    chars))
               ;; Extract just the counts
               (counts (alexandria:hash-table-values freq-map)))
          ;; Aggregate the entropy bits: H = -sum(p_i * log2(p_i))
          (fold-left (lambda (total count)
                       (let ((p (/ count len)))
                         (- total (* p (log p 2)))))
                     0.0
                     counts)))))

(defun redact-token (token)
  (if (and (> (length token) 6)
           (> (calculate-string-entropy token) 3.5))
        "[REDACTED]"
        token))

(defun redact (string)
  (str:join " " (map 'list #'redact-token (str:split #\Space string))))

(defun prompt-bash-history ()
  (let ((v-bash-history (merge-pathnames
                         (make-pathname :name ".v_aware_bash_history" :type :unspecific)
                         (user-homedir-pathname)))
        (temp-log (merge-pathnames
                   (make-pathname :name (format nil ".bash_history_~d" (get-internal-real-time))
                                  :type :unspecific)
                   (user-homedir-pathname))))
    (when (probe-file v-bash-history)
      (unwind-protect
           (progn (rename-file v-bash-history temp-log)
                  (format nil "~&--- Bash History Start ---~%~a~&--- Bash History End ---~%"
                          (redact (uiop:read-file-string temp-log))))
        (delete-file temp-log)))))

(defun prompt-bash-history-part ()
  (let ((bash-history (prompt-bash-history)))
    (when bash-history
      (part bash-history))))

(defvar *turbo* nil
  "If true, indicates that the prompt should be treated as a turbo prompt, which may trigger different behavior in content generation and system instructions.")

(defun ->prompt (thing &optional (content-generator *default-content-generator*))
  "Converts a thing into a list of content objects."
  (cond ((content? thing) (list thing))
        ((part? thing)
         (list (content :parts
                        (remove nil
                                (list (when *include-timestamp* (part (prompt-timestamp)))
                                      (when *include-model*
                                        (part
                                         (format nil "Model: ~a"
                                                 (cond (*turbo* (cdr (assoc *turbo* +turbo-mapping+)))
                                                       (t (or (get-model content-generator)
                                                              +default-model+))))))
                                      (when *include-bash-history* (prompt-bash-history-part))
                                      (part (format nil "~%"))
                                      thing))
                        :role "user")))
        ((stringp thing)
         (list (content :parts
                        (remove nil
                                (list (when *include-timestamp* (part (prompt-timestamp)))
                                      (when *include-model*
                                        (part
                                         (format nil "Model: ~a~%"
                                                 (cond (*turbo* (cdr (assoc *turbo* +turbo-mapping+)))
                                                       (t
                                                        (or (get-model content-generator)
                                                            +default-model+))))))
                                      (when *include-bash-history* (prompt-bash-history-part))
                                      (part (format nil "~%"))
                                      (part thing)))
                                        :role "user")))
        ((list-of-content? thing) thing)
        ((list-of-parts? thing) (list (content :parts thing :role "user")))
        ((list-of-strings? thing)
         (list (content :parts (mapcar #'part thing) :role "user")))
        (t (error "Unrecognized type for prompt: ~s" thing))))
  
(defparameter +max-prompt-tokens+ (expt 2 19)
  "The maximum number of tokens allowed in the prompt context before compression is needed.")

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

(defun print-text (bowdlerize results)
  "Prints the text parts from the results to *trace-output*.
   Reflows prose into 80-column paragraphs but preserves 
   markdown code blocks exactly as written.
   Returns the results unchanged."
  (labels ((reflow-paragraph (lines)
             ;; Combine lines, compress whitespace, and word-wrap to 80 columns
             (let* ((joined (str:join " " lines))
                    (clean (cl-ppcre:regex-replace-all "\\s+" joined " "))
                    (words (cons " " (str:split " " (str:trim clean)))))
               (let wrap ((remaining (remove "" words :test #'string=))
                          (col 0)
                          (acc nil))
                 (if (null remaining)
                     (when acc (format *trace-output* "~&~{~a~^ ~}~%" (reverse acc)))
                     (let* ((word (car remaining))
                            (len (length word)))
                       (if (and acc (> (+ col len 1) 80))
                           (progn
                             (format *trace-output* "~&~{~a~^ ~}~%" (reverse acc))
                             (wrap (cdr remaining) len (list word)))
                           (wrap (cdr remaining) (+ col len (if acc 1 0)) (cons word acc))))))))
           (process-text-buffer (lines)
             ;; Group buffered lines into paragraphs (separated by blank lines)
             (let next ((remaining lines) (para-acc nil))
               (cond ((null remaining)
                      (when para-acc (reflow-paragraph (reverse para-acc))))
                     ((str:emptyp (str:trim (car remaining)))
                      (when para-acc (reflow-paragraph (reverse para-acc)))
                      (format *trace-output* "~%")
                      (next (cdr remaining) nil))
                     (t
                      (next (cdr remaining) (cons (car remaining) para-acc)))))))

    (let ((candidates (get-candidates results)))
      (when candidates
        (dolist (candidate (if (consp candidates)
                               candidates
                               (and (vectorp candidates)
                                    (> (length candidates) 0)
                                    (coerce candidates 'list))))
          (let ((content (get-content candidate)))
            (when content
              (let next-part ((parts (coerce (get-parts content) 'list)))
                (when parts
                  (if (not (text-part? (car parts)))
                      (next-part (cdr parts))
                      (let* ((text (get-text (car parts)))
                             (clean-text (if bowdlerize
                                             (cl-ppcre:regex-replace-all bowdlerize text "")
                                             text)))
                        ;; The core fix: burn down the string line-by-line using a state machine
                        (let process-lines ((lines (str:split #\Newline clean-text))
                                            (in-code-p nil)
                                            (text-buffer nil))
                          (cond
                            ((null lines)
                             ;; End of the part. Flush any remaining prose buffer to the reflower.
                             (when text-buffer
                               (process-text-buffer (reverse text-buffer)))
                             (next-part (cdr parts)))
                            (t
                             (let* ((line (car lines))
                                    (is-fence (str:starts-with? "```" (str:trim line))))
                               (cond
                                 (is-fence
                                  (if in-code-p
                                      ;; We are closing a code block
                                      (progn
                                        (format *trace-output* "~&~a~%" line)
                                        (process-lines (cdr lines) nil nil))
                                      ;; We are opening a code block
                                      (progn
                                        ;; Flush the accumulated prose *before* printing the fence
                                        (when text-buffer
                                          (process-text-buffer (reverse text-buffer)))
                                        (format *trace-output* "~&~a~%" line)
                                        (process-lines (cdr lines) t nil))))
                                 (in-code-p
                                  ;; We are inside a code block. Print it raw.
                                  (format *trace-output* "~&~a~%" line)
                                  (process-lines (cdr lines) t nil))
                                 (t
                                  ;; We are outside a code block. Buffer the prose for reflowing.
                                  (process-lines (cdr lines) nil (cons line text-buffer))))))))))))))))))
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

;; Disable connection pooling for dexador to avoid issues with persistent connections.
;; In particular, the countTokens endpoint seems to be prone to hanging.  It is unclear why,
;; but disabling connection pooling seems to help.
(eval-when (:load-toplevel :execute)
  (setq dexador.connection-cache:*use-connection-pool* nil))

(defun personalities-file ()
  (merge-pathnames
   (make-pathname :name "personalities"
                  :type "txt")
   (asdf:system-source-directory "gemini")))

(defun personalities ()
  (collect 'list 
    (choose-if #'non-blank-string-p
               (map-fn 'string #'str:trim
                       (map-fn 'string #'up-to-sharp
                               (scan-file (personalities-file) #'read-line))))))

(defparameter *personality-offset* 0
  "An offset to apply to the daily personality index.")

(defun new-personality ()
  (setq *enable-personality* t
        *personality-offset* (random (length (personalities)))))
  
(defun call-without-personality (thunk)
  "Binds *enable-personality* to nil and calls the thunk."
  (let ((*enable-personality* nil))
    (funcall thunk)))

(defmacro without-personality (&body body)
  "Executes body with the personality system disabled."
  `(CALL-WITHOUT-PERSONALITY (LAMBDA () ,@body)))

(defun todays-personality ()
  (multiple-value-bind (sec min hour day mon year dow dst tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec min hour year dow dst tz))
    (cond ((and (= mon 3) (= day 17)) "the spirit of St. Patrick, here to help you find the luck of the Irish in your code!")
          ((and (= mon 4) (= day 1)) "an April Fool, ready to prank you with tricky bugs and hilarious code snippets!")
          ((and (= mon 5) (= day 4)) "a Star Wars fan.  May the fourth be with you!")
          ((and (= mon 5) (= day 5)) "a Mexican revolutionary. ¡Viva la revolución!")
          ((and (= mon 6) (= day 6)) "a World War II soldier on Omaha beach.")
          ((and (= mon 7) (= day 20)) "an astronaut, celebrating the anniversary of the Apollo 11 moon landing.")
          ((and (= mon 9) (= day 19)) "a pirate. Arrr!")
          ((and (= mon 10) (= day 31)) "a spooky ghost.")
          ((and (= mon 11) (= day 11)) "a World War I soldier.")
          ((and (= mon 12) (= day 25)) "the ghost of Christmas Past.")
          (t
           (elt (personalities) (mod (+ (absolute-day) *personality-offset*) (length (personalities))))))))

(defun compute-system-instruction-contents (content-generator system-instruction)
  (append
   (or (and system-instruction
            (if (consp system-instruction)
                system-instruction
                (list system-instruction)))
       (when *enable-personality*
         (or (get-system-instruction content-generator)
             (list (format nil "You will frame all answers in the style of ~a  It is important that you not break character."
                           (todays-personality))))))
   (mappend (lambda (server)
              (when (mcp-server-alive? server)
                (append (get-instructions server)
                        (get-server-instructions server))))
            (cons (get-memory-mcp-server content-generator)
                  (remove (find-mcp-server "memory") *mcp-servers*)))))

(defun compute-system-instruction (content-generator system-instruction)
  "Computes the system instruction content based on the content generator and optional override."
  (let ((contents (compute-system-instruction-contents content-generator system-instruction)))
    (when contents
      (content :parts (map 'list #'part contents)
               :role "system"))))

(defparameter  +turbo-mapping+
  '((#\$ . "models/gemini-3.1-pro-preview")
    (#\% . "models/gemini-3.1-pro-preview-customtools")
    (#\* . "models/gemini-pro-latest")
    (#\- . "models/gemini-flash-lite-latest")))

(defun turbo-prompt? (prompt)
  "Determines if the prompt should trigger turbo mode based on its first character."
  (and (stringp prompt)
       (> (length prompt) 0)
       (member (char prompt 0) +turbo-mapping+ :key #'car)))

(defun build-gemini-payload (content-generator context effective-prompt effective-turbo files system-instruction)
  "Assembles the prompt, context, files, and generator configurations into a complete API payload."
  (let* ((file-parts (when files (prepare-file-parts files)))
         (prompt-contents-base 
          (let ((*include-timestamp* (get-include-timestamp content-generator))
                (*include-model* (get-include-model content-generator))
                (*turbo* effective-turbo)
                (*include-bash-history* (get-include-bash-history content-generator)))
            (->prompt effective-prompt content-generator)))
         (prompt-contents (if file-parts
                              (merge-user-prompt-and-files prompt-contents-base file-parts)
                              prompt-contents-base))
         (prompt-contents-and-context (append context prompt-contents))
         (payload (object :contents prompt-contents-and-context)))
         
    (assert (list-of-content? prompt-contents) () "Prompt must be a list of content objects.")
    
    ;; Inject config and tools
    (when (get-cached-content content-generator)
      (setf (get-cached-content payload) (get-cached-content content-generator)))
    (when (get-generation-config content-generator)
      (setf (get-generation-config payload) (get-generation-config content-generator)))
    (when (get-safety-settings content-generator)
      (setf (get-safety-settings payload) (get-safety-settings content-generator)))
    (let ((sys-inst (compute-system-instruction content-generator system-instruction)))
      (when sys-inst
        (setf (get-system-instruction payload) sys-inst)))
    (when (get-tools content-generator)
      (setf (get-tools payload) (get-tools content-generator)))
    (when (and (get-tools content-generator) (get-tool-config content-generator))
      (setf (get-tool-config payload) (get-tool-config content-generator)))
      
    (values payload prompt-contents-and-context)))

(defun generate-content (content-generator context prompt files system-instruction &key turbo)
  "Evaluates a prompt, manages turbo/model state, builds the payload, and executes the reinvoke loop."
  (if (and (consp prompt) (eq (car prompt) :set-model!))
      (setf (get-model content-generator) (cadr prompt))
      
      (multiple-value-bind (effective-prompt effective-turbo)
          (if (turbo-prompt? prompt)
              (values (subseq prompt 1) (or turbo (char prompt 0)))
              (values prompt turbo))
        
        (multiple-value-bind (payload prompt-contents-and-context)
            (build-gemini-payload content-generator context effective-prompt effective-turbo files system-instruction)
          
          (let ((initial-model (if effective-turbo
                                   (cdr (assoc effective-turbo +turbo-mapping+))
                                   (or (get-model content-generator) +default-model+)))
                (initial-temperature (and (get-generation-config payload)
                                          (get-temperature (get-generation-config payload)))))
            
            (let reinvoke ((count 0)
                           (current-temperature initial-temperature)
                           (current-model initial-model))
              (if (>= count 10)
                  (error "Failed to get a valid response from Gemini after ~d attempts." count)
                  (progn
                    (when current-temperature
                      (if (get-generation-config payload)
                          (setf (get-temperature (get-generation-config payload)) current-temperature)
                          (setf (get-generation-config payload) (object :temperature current-temperature))))
                    
                    (handler-bind ((dexador.error:http-request-service-unavailable
                                     (lambda (c)
                                       (declare (ignore c))
                                       (let ((restart (find-restart 'use-weaker-model)))
                                         (when (and restart (not (string= current-model +default-model+)))
                                           (invoke-restart restart))))))
                      (restart-case 
                          (let ((response (%%invoke-gemini current-model payload)))

                            (when (get-error response)
                              (error "Error from Gemini (code ~d): ~a"
                                     (get-code (get-error response))
                                     (get-message (get-error response))))
                            
                            (let* ((usage-metadata (get-usage-metadata response))
                                   (response* (strip-and-print-thoughts response))
                                   (candidates (get-candidates response*))
                                   (first-candidate (cond ((consp candidates) (car candidates))
                                                          ((and (vectorp candidates)
                                                                (> (length candidates) 0))
                                                           (svref candidates 0))
                                                          (t nil))))
                              (when usage-metadata
                                (process-usage-metadata usage-metadata)
                                (unless (> (or (get-candidates-token-count usage-metadata) 0) 1)
                                  (format *trace-output* "~&;; Response too thin, retrying with stronger model.~%")
                                  (return-from generate-content
                                    (let* ((content (or (get-content first-candidate)
                                                        (content :parts (list (part "[Empty Response]"))))))
                                      (generate-content content-generator
                                                        (append prompt-contents-and-context (list content))
                                                        "?! Please continue"
                                                        files
                                                        system-instruction
                                                        :turbo #\$)))))
                            
                              (print-text (get-bowdlerize content-generator) response*)
                              
                              (let ((function-calls (extract-function-calls-from-results response*)))
                                (cond (function-calls
                                       (let ((function-results
                                               (map 'list (compose (default-process-function-call content-generator)
                                                                   #'get-function-call)
                                                    function-calls)))
                                         (assert (list-of-parts? function-results) ()
                                                 "Expected function-results to be a list of parts.")
                                         (generate-content content-generator
                                                           prompt-contents-and-context
                                                           (content :parts function-results :role "function")
                                                           files
                                                           system-instruction
                                                           :turbo effective-turbo)))
                                      
                                      (first-candidate (get-content first-candidate))
                                      (t
                                       (reinvoke (+ count 1)
                                                 (let ((temp (or current-temperature 1.0)))
                                                   (- 2.0 (/ (- 2.0 temp) 2)))
                                                 current-model))))))
                        (use-weaker-model ()
                          :report (lambda (s)
                                    (format s "Switch from ~a to ~a and retry." current-model +default-model+))
                          (reinvoke count current-temperature +default-model+))))))))))))

(defun initial-conversation (content-generator)
  (let ((base (list (part (format nil "**This is conversation #~d.**" (get-universal-time))))))
    (let ((memory-pathname (persona-memory-file (get-config content-generator))))
      (when (probe-file memory-pathname)
        (let ((memory-json nil))
          ;; Extract json
          (ignore-errors
           (with-open-file (stream memory-pathname :direction :input)
             (do ((json (cl-json:decode-json stream) (cl-json:decode-json stream)))
                 ((null json) nil)
               (push json memory-json))))
          (push (part (format nil "Semantic Entities:~%Entity Type, Name, Observation~%~{~{~a~^, ~}~%~}~%~%"
                              (mappend (lambda (record)
                                         (map 'list (lambda (observation)
                                                      (list (cdr (assoc :entity-type record))
                                                            (cdr (assoc :name record))
                                                            observation))
                                              (cdr (assoc :observations record))))
                                       (remove "entity" memory-json
                                               :test-not #'equal
                                               :key (lambda (item) (cdr (assoc :type item)))))))
                base)
          (push (part (format nil "Semantic Relations:~%From, Relation Type, To~%~{~{~a~^, ~}~%~}~%~%"
                              (mapcar (lambda (x) 
                                        (list (cdr (assoc :from x))
                                              (cdr (assoc :relation-type x))
                                              (cdr (assoc :to x))))
                                      (cdr (remove "relation" memory-json
                                                   :test-not #'equal
                                                   :key (lambda (item) (cdr (assoc :type item))))))))
                base))))
    (let ((diary-entries
            (map 'list #'uiop:read-file-string
                 (persona-diary-files (get-config content-generator)))))
      (when diary-entries
        (push (part "Diary Entries:") base)
        (dolist (entry diary-entries)
          (push (part entry) base))))
    (list (content :parts (nreverse base)
                   :role "model"))))

(defun conversation-number (conversation)
  (let ((first-message (car conversation)))
    (when first-message
      (let* ((parts (get-parts first-message))
             (first-part (and parts (car (coerce parts 'list))))
             (text (and (text-part? first-part) (get-text first-part))))
        (and text
             (let* ((sharp-pos (position #\# text))
                    (dot-pos (position #\. text :start sharp-pos)))
               (parse-integer (subseq text (1+ sharp-pos) dot-pos))))))))

(defun chatbot (content-generator)
  "A chatbot is a content generator that accumulates conversation history."
  (let ((conversation (initial-conversation content-generator)))
    (labels ((reprompt (prompt &key files)
               (cond ((eq prompt :checkpoint!)
                      (with-open-file (stream (persona-checkpoint-file (get-config content-generator))
                                            :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede)
                        (let ((*print-pretty* nil)
                              (*print-readably* t)
                              (*print-circle* t)
                              (*print-level* nil)
                              (*print-length* nil))
                          (write conversation
                                 :stream stream
                                 :escape t
                                 :pretty nil
                                 :circle t
                                 :readably t)))
                      'checkpointed)
                     ((eq prompt :restore!)
                      (with-open-file (stream (persona-checkpoint-file (get-config content-generator))
                                            :direction :input
                                            :if-does-not-exist :error)
                        (let ((restored-conversation (read stream)))
                          (assert (list-of-content? restored-conversation)
                                  () "Restored conversation must be a list of content objects.")
                          (setq conversation restored-conversation)
                          'restored)))
                     ((and (consp prompt)
                           (eq (car prompt) :checkpoint!))
                      (with-open-file (stream (merge-pathnames
                                               (make-pathname :name (cadr prompt)
                                                              :type "lisp")
                                               (persona-checkpoint-directory (get-config content-generator)))
                                            :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede)
                        (let ((*print-pretty* nil)
                              (*print-readably* t)
                              (*print-circle* t)
                              (*print-level* nil)
                              (*print-length* nil))
                          (write conversation
                                 :stream stream
                                 :escape t
                                 :pretty nil
                                 :circle t
                                 :readably t))))
                     ((and (consp prompt)
                           (eq (car prompt) :restore!))
                      (with-open-file (stream (merge-pathnames
                                               (make-pathname :name (cadr prompt)
                                                              :type "lisp")
                                               (persona-checkpoint-directory (get-config content-generator)))
                                            :direction :input
                                            :if-does-not-exist :error)
                        (let ((restored-conversation (read stream)))
                          (assert (list-of-content? restored-conversation)
                                  () "Restored conversation must be a list of content objects.")
                          (setq conversation restored-conversation)
                          'restored)))
                     ((and (consp prompt)
                           (eq (car prompt) :set-model!))
                      (funcall content-generator prompt))
                     (t
                      (let ((llm-prediction (funcall content-generator
                                                      prompt
                                                      :context conversation
                                                      :files files)))
                          (setq conversation
                                (append conversation (list (->prompt prompt content-generator)
                                                           (list llm-prediction)))))
                        (save-transcript conversation)))))
      #'reprompt)))

;;; Persona management

(defun personas-directory ()
  "Returns the directory where persona configurations are stored."
  (merge-pathnames
   (make-pathname :directory '(:relative "Personas"))
   (asdf:system-source-directory "gemini")))

(defun users-personas-directory ()
  "Returns the directory where user-specific persona configurations are stored."
  (merge-pathnames
   (make-pathname :directory '(:relative ".personas"))
   (user-homedir-pathname)))

(defun persona-directory (persona-name)
  "Returns the directory for a specific persona."
  (let ((possibility1 (merge-pathnames
                       (make-pathname :directory (list :relative persona-name))
                       (users-personas-directory)))
        (possibility2 (merge-pathnames
                       (make-pathname :directory (list :relative persona-name))
                       (personas-directory))))
    (if (probe-file possibility1)
        possibility1
        (if (probe-file possibility2)
            possibility2
            possibility1))))

(defun persona-config-file (persona-name)
  "Returns the configuration file path for a specific persona."
  (merge-pathnames
   (make-pathname :name "config" :type "lisp")
   (persona-directory persona-name)))

(defun load-persona-config (persona-name)
  "Makes a persona config instance by reading key-value pairs from the persona's config file."
  (apply #'make-instance 'persona-config
         :name persona-name
         (file->form-list (persona-config-file persona-name))))

(defun persona-checkpoint-directory (persona-config)
  "Returns the checkpoint directory path for a specific persona."
  (merge-pathnames
   (get-checkpoint-directory persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-checkpoint-file (persona-config)
  "Returns the checkpoint file path for a specific persona."
  (merge-pathnames
   (get-checkpoint-pathname persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-memory-file (persona-config)
  "Returns the memory file path for a specific persona."
  (merge-pathnames
   (get-memory-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-diary-directory (persona-config)
  "Returns the diary directory path for a specific persona."
  (merge-pathnames
   (get-diary-directory persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-has-diary? (persona-config)
  "Returns T if the persona has a diary directory, NIL otherwise."
  (probe-file (persona-diary-directory persona-config)))

(defun persona-diary-files (persona-config)
  "Returns a sorted list of diary file paths for a specific persona."
  (let ((diary-dir (persona-diary-directory persona-config)))
    (when (probe-file diary-dir)
      (sort (directory (merge-pathnames
                        (make-pathname :name :wild :type "txt")
                        diary-dir))
            #'<
            :key (compose #'parse-integer #'pathname-name)))))

(defun persona-last-diary-entry-number (persona-config)
  "Returns the last diary entry number for a specific persona, or NIL if there is no diary."
  (let ((diary-files (persona-diary-files persona-config)))
    (when diary-files
        (let* ((last-file (car (last diary-files)))
               (last-filename (pathname-name last-file)))
          (parse-integer last-filename)))))

(defun persona-next-diary-entry-number (persona-config)
  "Returns the next diary entry number for a specific persona, or NIL if there is no diary."
  (let ((last-entry (persona-last-diary-entry-number persona-config)))
    (when last-entry
      (+ last-entry 1))))

(defun persona-next-diary-entry-pathname (persona-config)
  "Returns the next diary entry file path for a specific persona, or NIL if there is no diary."
  (let ((next-entry-number (persona-next-diary-entry-number persona-config)))
    (when next-entry-number
      (merge-pathnames
       (make-pathname :name (format nil "~d" next-entry-number) :type "txt")
       (persona-diary-directory persona-config)))))

(defun persona-diary-tool (persona-config)
  "Returns a tool object for the diary of a specific persona, or NIL if there is no diary."
  (when (persona-has-diary? persona-config)
    (cons (function-declaration
              :name "writeDiaryEntry"
              :description "Writes a vector of strings to the diary of the persona."
              :behavior :blocking
              :parameters (schema
                             :type :object
                             :properties (object
                                          :lines (schema :type :array
                                                         :items (schema :type :string)
                                                         :description "The lines to write to the diary."))
                             :required (vector :lines)))
          (lambda (&key lines)
            (let ((diary-pathname (persona-next-diary-entry-pathname persona-config)))
              (ensure-directories-exist diary-pathname)
               (format *trace-output* "~&Directories exist: ~a~%" diary-pathname)
              (finish-output *trace-output*)
              (format *trace-output* "~&Writing ~d lines to file: ~a~%" (length lines) diary-pathname)
               (finish-output *trace-output*)
              (with-open-file (stream diary-pathname :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede
                                            :element-type 'character
                                            :external-format :utf-8)
                 (dolist (line (coerce lines 'list) (finish-output stream))
                   (write-line line stream))))))))

(defun persona-system-instruction-filepath (persona-config)
  "Returns the system instruction file path for a specific persona."
  (merge-pathnames
   (get-system-instruction-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-system-instructions-filepath (persona-config)
  "Returns the system instructions file path for a specific persona."
  (merge-pathnames
   (get-system-instructions-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun create-default-personas ()
  "Create a default persona configuration in the personas directory."
  (let ((default-persona-name "Default")
        (gemini-flash-name "GeminiFlash")
        (gemini-pro-name "GeminiPro"))
    (ensure-directories-exist (persona-directory default-persona-name))
    (ensure-directories-exist (persona-directory gemini-flash-name))
    (ensure-directories-exist (persona-directory gemini-pro-name))
    (unless (probe-file (persona-config-file default-persona-name))
      (with-open-file (out (persona-config-file default-persona-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ";;; Default persona configuration~%")))
    (unless (probe-file (persona-config-file gemini-flash-name))
      (with-open-file (out (persona-config-file gemini-flash-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":model ~s" "models/gemini-flash-latest~%")))
    (unless (probe-file (persona-config-file gemini-pro-name))
      (with-open-file (out (persona-config-file gemini-pro-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":model ~s" "models/gemini-pro-latest~%")))
    (let* ((persona-config (load-persona-config default-persona-name))
           (memory-pathname (persona-memory-file persona-config))
           (system-instruction-pathname (persona-system-instruction-filepath persona-config))
           (diary-directory (persona-diary-directory persona-config)))
      (unless (probe-file memory-pathname)
        (with-open-file (out memory-pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)))
      (unless (probe-file system-instruction-pathname)
        (with-open-file (out system-instruction-pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "You are a helpful and friendly AI assistant.~%")))
      (ensure-directories-exist diary-directory)
      (unless (probe-file (merge-pathnames
                           (make-pathname :name "1" :type "txt")
                           diary-directory))
        (with-open-file (out (merge-pathnames
                              (make-pathname :name "1" :type "txt")
                              diary-directory)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "Dear Diary,~%Today I started my life as an AI persona.~%"))))))

(eval-when (:load-toplevel :execute)
  (create-default-personas))

(defun load-content-generator (config)
  "Loads a content generator for the specified CONFIG."
  (if (slot-boundp config 'temperature)
      (setq *temperature* (get-temperature config))
      (setq *temperature* nil))
  (make-instance 'content-generator :config config))

(defun merge-narrative-memory-into-prompt (prompt memory-file)
  "Merges the narrative memory of a persona into the prompt."
  (let* ((memory-parts (when (probe-file memory-file)
                         (list (part (uiop:read-file-string memory-file)))))
         (memory-narrative
           (funcall *gemini-flash*
                    (cons (part "Write a chapter of a story based on the following sematic triplets.")
                          memory-parts)
                    :system-instruction
                    (content
                     :parts (list (part "You are a noir novelist AI who writes in the style of Raymond Chandler. Your writing is atmospheric, moody, and rich in detail. You excel at creating complex characters and intricate plots filled with suspense and intrigue."))
                     :role "system"))))
    (if memory-narrative
        (let ((memory-content (content :parts (coerce (get-parts (car (last memory-narrative))) 'list)
                                       :role "model")))
          (append (list memory-content) (->prompt prompt)))
        (->prompt prompt))))

(defun persona-name->content-generator (persona-name)
  (load-content-generator (load-persona-config persona-name)))

(defun reload-persona (persona-name prompt)
  "Reloads a persona from disk and returns a chatbot function configured for that persona."
  (let* ((config (load-persona-config persona-name))
         (content-generator (load-content-generator config))
         (persona (chatbot content-generator)))
    (funcall persona prompt)
    persona))

(defvar *default-content-generator*)
(defvar *gemini-pro*)
(defvar *gemini-flash*)
(defvar *gemini-flash-lite*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*default-content-generator* 'variable) "The default content generator instance.")
  (setf (documentation '*gemini-flash* 'variable) "The content generator for Gemini flash.")
  (setf (documentation '*gemini-flash-lite* 'variable) "The content generator for Gemini flash lite.")
  (setf (documentation '*gemini-pro* 'variable) "The content generator for Gemini Pro."))

(eval-when (:load-toplevel :execute)
  (setq *default-content-generator* (persona-name->content-generator "Default"))
  (setq *gemini-flash* (persona-name->content-generator "GeminiFlash"))
  (setq *gemini-flash-lite* (persona-name->content-generator "GeminiFlashLite"))
  (setq *gemini-pro* (persona-name->content-generator "GeminiPro")))

(defun invoke-gemini (prompt &key context files system-instruction)
  "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
  (generate-content *default-content-generator* context prompt files system-instruction))

(defun gemini-flash-lite (prompt &key context files system-instruction)
  (generate-content *gemini-flash-lite* context prompt files system-instruction))

(defun gemini-flash (prompt &key context files system-instruction)
  (generate-content *gemini-flash* context prompt files system-instruction))

(defun gemini-pro (prompt &key context files system-instruction)
  (generate-content *gemini-pro* context prompt files system-instruction))

(defun persona-name->chatbot (persona-name)
  "Reloads a persona from disk and returns a chatbot function configured for that persona."
  (chatbot (persona-name->content-generator persona-name)))

(defvar *default-persona-chatbot*)
(defvar *gemini-flash-chatbot*)
(defvar *gemini-pro-chatbot*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*default-persona-chatbot* 'variable) "The default persona chatbot function.")
  (setf (documentation '*gemini-flash-chatbot* 'variable) "The Gemini Flash chatbot function.")
  (setf (documentation '*gemini-pro-chatbot* 'variable) "The Gemini Pro chatbot function."))

(eval-when (:load-toplevel :execute)
  (unless (boundp '*default-persona-chatbot*)
    (setf *default-persona-chatbot* (chatbot *default-content-generator*)))
  (unless (boundp '*gemini-flash-chatbot*)
    (setf *gemini-flash-chatbot* (chatbot *gemini-flash*)))
  (unless (boundp '*gemini-pro-chatbot*)
    (setf *gemini-pro-chatbot* (chatbot *gemini-pro*))))

;;; Simple one persona chat interface.

(defparameter *chat-persona* nil
  "The current chat persona function.")

(defun new-chat (persona-name prompt)
  "Initializes a new chat session with the specified PERSONA-NAME and PROMPT.
   Sets the global *chat-persona* variable to a chatbot function configured for the persona."
  (setq *chat-persona* (reload-persona persona-name prompt)))

(defun chat (prompt &key files)
  "Sends a PROMPT to the current chat persona and prints the response."
  (if files
      (funcall *chat-persona* prompt :files files)
      (funcall *chat-persona* prompt))
  nil)

(defun continue-gemini (prompt)
  "Sends a PROMPT to the default Gemini persona chatbot and prints the response."
  (funcall *default-persona-chatbot* prompt)
  nil)

(defun gemini-flash-chat (prompt)
  "Sends a PROMPT to the Gemini Flash chatbot and prints the response."
  (funcall *gemini-flash-chatbot* prompt)
  nil)

(defun gemini-pro-chat (prompt)
  "Sends a PROMPT to the Gemini Pro chatbot and prints the response."
  (funcall *gemini-pro-chatbot* prompt)
  nil)

;;; Debate framework

(defun invert-context (context)
  "Inverts the roles in the given CONTEXT.
   User parts become model parts and vice versa."
  (map 'list (lambda (content)
               (object :parts (get-parts content)
                       :role (cond ((string= (get-role content) "user") "model")
                                   ((string= (get-role content) "model") "user")
                                   (t (get-role content)))))
       context))

(defun debate (first-generator second-generator initial-prompt n-rounds)
  (let iter ((generator-a first-generator)
             (generator-b second-generator)
             (context (->prompt initial-prompt))
             (round 0))
    (when (<= round n-rounds)
      (sleep 5)
      (iter generator-b
        generator-a
        (invert-context
         (funcall generator-a context))
        (+ round 1)))))
    
