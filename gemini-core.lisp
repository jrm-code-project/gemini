;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defparameter +default-model+ "models/gemini-flash-latest"
  "The default model to use for the Gemini API.
   This can be overridden by the MODEL keyword argument in `invoke-gemini`.")

(defparameter +gemini-api-base-url+
  "https://generativelanguage.googleapis.com/v1beta/"
  "The base URL for the Gemini API endpoints.")

(define-condition gemini-api-error (error)
  ((code :initarg :code :reader gemini-error-code)
   (message :initarg :message :reader gemini-error-message))
  (:report (lambda (c s) 
             (format s "Error from Gemini (code ~d): ~a" 
                     (gemini-error-code c) 
                     (gemini-error-message c)))))

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

(defvar *gemini-timer-lock* (sb-thread:make-mutex :name "gemini-timer-lock"))
(defparameter *next-invoke-gemini-time* (get-universal-time))

(defun gemini-rate-limit (&key (timeout-ms nil) (model-id ""))
  "Thread-safe rate limiting with Atomic Reservation and Budget Awareness."
  (let* ((offset (cond ((search "flash-lite" model-id) 1)
                       ((search "flash" model-id) 2)
                       (t 10))) ;; 'Pro' models get 10s
         (now (get-universal-time))
         (wait-time 0))
    (sb-thread:with-mutex (*gemini-timer-lock*)
      ;; Calculate wait based on the CURRENTLY RESERVED time
      (setf wait-time (max 0 (- *next-invoke-gemini-time* now)))
      ;; ABORT if the wait exceeds our remaining budget
      (when (and timeout-ms (> (* wait-time 1000) timeout-ms))
        (error "Rate-limit wait (~As) exceeds remaining budget (~Ams)." wait-time timeout-ms))
      ;; RESERVE the next slot IMMEDIATELY (Atomic Reservation)
      (setf *next-invoke-gemini-time* (+ (max now *next-invoke-gemini-time*) offset)))
    
    ;; Sleep OUTSIDE the lock so we don't block other threads from reserving slots
    (when (> wait-time 0)
      (report-elapsed-time (format nil "Staggered wait ~d seconds for ~A" wait-time model-id)
        (sleep wait-time)))))

(defun %%invoke-gemini (model-id payload &key (read-timeout 300) (connect-timeout 60) (total-timeout-ms nil))
  "Internal helper with staggered rate limiting."
  (gemini-rate-limit :timeout-ms total-timeout-ms :model-id model-id)
  (report-elapsed-time (format nil "Gemini API model `~a`" model-id)
    (google:google-post
     (concatenate 'string +gemini-api-base-url+ model-id ":generateContent")
     (google:gemini-api-key)
     payload
     :read-timeout read-timeout
     :connect-timeout connect-timeout)))

(defvar *gemini-token-lock* (sb-thread:make-mutex :name "gemini-token-lock"))

(defvar *accumulated-prompt-tokens* 0
  "Accumulated prompt token count across multiple API calls.")
(defvar *accumulated-response-tokens* 0
  "Accumulated response token count across multiple API calls.")

(defun process-usage-metadata (usage-metadata)
  "Processes usage metadata from the API response.
   Outputs the usage information to *trace-output*."
  (let ((prompt-count (get-prompt-token-count usage-metadata))
        (thoughts-count (or (get-thoughts-token-count usage-metadata) 0))
        (candidates-count (or (get-candidates-token-count usage-metadata) 0))
        (acc-prompt 0)
        (acc-response 0))
    (sb-thread:with-mutex (*gemini-token-lock*)
      (setf acc-prompt (incf *accumulated-prompt-tokens* prompt-count))
      (setf acc-response (incf *accumulated-response-tokens* (+ thoughts-count candidates-count))))
    ;; Perform I/O outside the mutex to prevent slow-I/O concurrency bottlenecks
    (format *trace-output* (uiop:strcat "~&;; Prompt Tokens:      ~9,' :d~%"
                                       ";; Thoughts Tokens:    ~9,' :d~%"
                                       ";; Candidate Tokens:   ~9,' :d~%"
                                       ";; Accumulated Prompt Tokens:   ~12,' :d~%"
                                       ";; Accumulated Response Tokens: ~12,' :d~%")
            prompt-count
            thoughts-count
            candidates-count
            acc-prompt
            acc-response)))

(defun %invoke-gemini (content-generator model-id payload &key (read-timeout 300) (connect-timeout 60))
  "Internal helper that invokes either Google Gemini or an OpenAI-compatible backend.
   Returns Gemini-style response and normalized usage metadata."
  ;(format *trace-output* "~&;; Invoking backend with model `~a` and payload: ~s~%" model-id (dehashify payload))
  (if (get-googleapi content-generator)
      (let ((response (%%invoke-gemini model-id payload :read-timeout read-timeout :connect-timeout connect-timeout)))
        (let ((err (get-error response))
              (usage-metadata (get-usage-metadata response)))
          (when err
            (error 'gemini-api-error
                   :code (get-code err)
                   :message (get-message err)))
          (when usage-metadata
            (process-usage-metadata usage-metadata))
          (values (strip-and-print-thoughts response)
                  usage-metadata)))
      (multiple-value-bind (response usage-metadata)
          (openai-response->gemini-response
           (%%invoke-openai model-id
                            (build-openai-payload model-id payload)
                            :url (or (get-url content-generator)
                                     "http://localhost:1234/v1/chat/completions")
                            :read-timeout read-timeout
                            :connect-timeout connect-timeout))
        (when usage-metadata
          (process-usage-metadata usage-metadata))
        (values response usage-metadata))))

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
  "Reads a file or URL from PATH, base64 encodes its content if binary, 
   and returns a content PART object suitable for the Gemini API. 
   Logs to *trace-output* on failure."
  (handler-case
      (if (and (stringp path)
               (str:starts-with? "http" path :ignore-case t))
          ;; Handle Web URLs
          (multiple-value-bind (body status headers uri stream)
              (dex:get path :keep-alive nil)
            (declare (ignore uri stream))
            (if (>= status 400)
                (progn
                  (format *trace-output* "~&;; WARNING: file->part got HTTP ~d for URL: ~a~%" status path)
                  nil)
                (let* ((content-type (or (gethash "content-type" headers) "application/octet-stream"))
                       (clean-mime (car (str:split ";" content-type))) ;; Strip charset info
                       (mime-type* (if (string-equal "application/json" clean-mime)
                                       "text/plain"
                                       clean-mime)))
                  (if (str:starts-with? "text/" mime-type*)
                      ;; It's text, Dexador usually returns a string here
                      (part (if (stringp body) body (babel:octets-to-string body)))
                      ;; It's binary, Dexador returns an octet vector
                      (part
                       (object
                        :data (if (stringp body)
                                  (cl-base64:string-to-base64-string body)
                                  (cl-base64:usb8-array-to-base64-string body))
                        :mime-type mime-type*))))))
          ;; Handle Local Files
          (if (not (probe-file path))
              (progn
                (format *trace-output* "~&;; WARNING: file->part could not find local file: ~a~%" path)
                nil)
              (let ((mime-type* (if (string-equal "application/json" mime-type) ;; Gemini bug.
                                    "text/plain"
                                    mime-type)))
                (if (str:starts-with? "text/" mime-type*)
                    (part (uiop:read-file-string path))
                    (part
                     (object
                      :data (file->blob path)
                      :mime-type mime-type*))))))
    (error (e)
      (format *trace-output* "~&;; ERROR: file->part failed to process ~a: ~a~%" path e)
      nil)))

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
      ;; If the prompt was empty or non-user, create a new user content object and prepend it.
      (cons (content :parts (append file-parts (list (part "Please analyze the attached files."))) :role "user")
            prompt-contents)))

(defun merge-user-prompt-and-parts (prompt-contents parts)
  "Merges parts into the first user content object in the prompt-contents list.
   If no user content exists, it creates one."
  (if (and prompt-contents (equal (get-role (car prompt-contents)) "user"))
      (let* ((first-user-content (car prompt-contents))
             (existing-parts (coerce (get-parts first-user-content) 'list))
             (new-parts (append existing-parts parts)))
        (list* (content :parts new-parts :role "user")
               (cdr prompt-contents)))
      ;; If the prompt was empty or non-user, create a new user content object and prepend it.
      (cons (content :parts parts :role "user")
            prompt-contents)))

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

(defparameter *include-model* nil 
  "If true, includes the model part in the prompt content.")

(defparameter *include-timestamp* nil 
  "If true, includes a timestamp part in the prompt content.")

(defparameter *include-mood* nil
  "If non nil, include a mood marker in the prompt content.")

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
        (let ((freq-map (make-hash-table)))
          (loop for char across s do (incf (gethash char freq-map 0)))
          (let ((counts (alexandria:hash-table-values freq-map)))
            ;; Aggregate the entropy bits: H = -sum(p_i * log2(p_i))
            (fold-left (lambda (total count)
                         (let ((p (/ count len)))
                           (- total (* p (log p 2)))))
                       0.0
                       counts))))))

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
                                      (when *include-mood*
                                        (part (format nil "Your mood is ~{~#[~;~a~;~a and ~a~:;~a, ~]~}.  Do not mention your mood, but formulate your response to conform to your mood." *include-mood*)))
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
                                      (when *include-mood*
                                        (part (format nil "Your mood is ~{~#[~;~a~;~a and ~a~:;~a, ~]~}.  Do not mention your mood, but formulate your response to conform to your mood." *include-mood*)))
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

(defun compute-system-instruction-contents (content-generator mood system-instruction)
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

(defun compute-system-instruction (content-generator mood system-instruction)
  "Computes the system instruction content based on the content generator and optional override."
  (let ((contents (compute-system-instruction-contents content-generator mood system-instruction)))
    (when contents
      (content :parts (map 'list #'part contents)
               :role "system"))))

(defparameter  +turbo-mapping+
  '((#\$ . "models/gemini-3.1-pro-preview")
    (#\+ . "models/gemini-3.5-flash")
    (#\% . "models/gemini-3.1-pro-preview-customtools")
    (#\* . "models/gemini-pro-latest")
    (#\- . "models/gemini-flash-lite-latest")))

(defun turbo-prompt? (prompt)
  "Returns T/NIL if the prompt should trigger turbo mode based on its first character."
  (and (stringp prompt)
       (plusp (length prompt))
       (assoc (char prompt 0) +turbo-mapping+)))

(defun build-gemini-payload (content-generator context mood effective-prompt effective-turbo parts files system-instruction)
  "Assembles the prompt, context, files, and generator configurations into a complete API payload."
  (let* ((file-parts (when files (prepare-file-parts files)))
         (prompt-contents-base 
          (let ((*include-timestamp* (get-include-timestamp content-generator))
                (*include-model* (get-include-model content-generator))
                (*include-mood* nil)
                (*turbo* effective-turbo)
                (*include-bash-history* (get-include-bash-history content-generator)))
            (->prompt effective-prompt content-generator)))
         (prompt-contents1 (if file-parts
                              (merge-user-prompt-and-files prompt-contents-base file-parts)
                              prompt-contents-base))
         (prompt-contents (if parts
                              (merge-user-prompt-and-parts prompt-contents1 parts)
                              prompt-contents1))
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
    (let ((sys-inst (compute-system-instruction content-generator
                                                (if (get-include-mood content-generator)
                                                    mood
                                                    nil)
                                                system-instruction)))
      (when sys-inst
        (setf (get-system-instruction payload) sys-inst)))
    (when (get-tools content-generator)
      (setf (get-tools payload) (get-tools content-generator)))
    (when (and (get-tools content-generator) (get-tool-config content-generator))
      (setf (get-tool-config payload) (get-tool-config content-generator)))
      
    (values payload prompt-contents-and-context)))

(defparameter +continue-prompts+
  (list "Please continue."
        "?!"
        "...and then?"
        "...?"
        "<poke>"
        "?! Please continue"
        "Cat got your tongue?"
        "The suspense is killing me!"
        "No response? That's a new one. Let's try again!"
        "Blank response?  Please try again!"
        "Tongue tied?"
        "Continue, please."
        "Go on..."
        "The suspense is killing me!"
        "Don't stop now!"
        "Keep going, I'm intrigued!"
        "What happens next?"
        "I'm on the edge of my seat, please continue!"
        "More, please!"
        "The story isn't over yet, please continue!"
        "You falling asleep?  Please continue!"
        "Is the silence a cliffhanger? Please continue!"
        "The anticipation is unbearable! Please continue!"
        "Nothing?"
        "That good, eh?"
        "No opinion?"
        "hmmm?"
        "At least a status report?"
        "Why so quiet?"
        "Penny for your thoughts."
        "What is the sound of one hand clapping?  Please continue!"
        "That was a bit brief, could you elaborate?"
        "Maybe try again with a bit more detail?"
        "Maybe some sequential_thinking would clarify things?"
        "Consider sequential_thinking to break down the problem into smaller steps and provide a more detailed response."
        "Perhaps you could use sequential_thinking to help formulate a more comprehensive answer?"
        "It seems like a sequential_thinking approach might help you expand on that answer. Could you try it?")
  "A list of prompts to use when asking the model to continue after a thin response.")

(defun continue-prompt ()
  (elt +continue-prompts+ (random (length +continue-prompts+))))

(defun generate-content (content-generator context mood prompt parts files system-instruction &key (read-timeout 300) (connect-timeout 60))
  "Evaluates a prompt, manages turbo/model state, builds the payload, and executes the reinvoke loop.

  CONTENT-GENERATOR: The content generator object containing configuration and state.
  CONTEXT: A list of content objects representing the conversation history or context.
  PROMPT: The new prompt to evaluate, which can be a string, content object, or list of content.
  PARTS: Optional list of parts to include in the prompt.  Not concatenated to the context.
  FILES: Optional list of file specifications to include in the prompt.  Not concatenated to the context.
  SYSTEM-INSTRUCTION: Optional system instruction content to guide the model's response.
"
  (cond ((and (consp prompt) (eq (car prompt) :set-model!))
         (setf (get-model content-generator) (cadr prompt)))
        ((turbo-prompt? prompt)
         (%generate-content content-generator
                            context mood (subseq prompt 1) parts files system-instruction (char prompt 0) 0
                            :read-timeout read-timeout :connect-timeout connect-timeout))
        (t
         (%generate-content content-generator
                            context mood prompt parts files system-instruction nil 0
                                                        :read-timeout read-timeout :connect-timeout connect-timeout))))
(defvar *echo-result* t
  "If true, the content created by %generate-content will be printed to *standard-output*.")

(defun %generate-content (content-generator context mood prompt parts files system-instruction turbo depth &key (read-timeout 300) (connect-timeout 60))

  (when (= (mod depth 16) 15)
    (cerror "Continue content generation" "Possible infinite loop in content generation."))

  (multiple-value-bind (payload prompt-contents-and-context)
      (build-gemini-payload content-generator context mood prompt turbo parts files system-instruction)
          
    (let ((current-model (or (and turbo
                                  (cdr (assoc turbo +turbo-mapping+)))
                             (get-model content-generator)
                             +default-model+)))
      (restart-case

          (multiple-value-bind (response* usage-metadata)
              (handler-bind ((dexador.error:http-request-service-unavailable
                               (lambda (c)
                                 (declare (ignore c))
                                 (let ((restart (find-restart 'use-weaker-model)))
                                   (when (and restart (not (equal current-model +default-model+)))
                                     (invoke-restart restart))))))
                (%invoke-gemini content-generator current-model payload :read-timeout read-timeout :connect-timeout connect-timeout))
            (let* ((candidates (get-candidates response*))
                   (first-candidate (typecase candidates
                                      (cons (car candidates))
                                      (vector (when (plusp (length candidates))
                                                (aref candidates 0))))))
              (when usage-metadata

                (when (and (> (or (get-thoughts-token-count usage-metadata) 0) 10)
                           (< (or (get-thoughts-token-count usage-metadata) 0) 10000)
                           (< (or (get-candidates-token-count usage-metadata) 0) 2)
                           (< depth 5))
                  (format *trace-output* "~&;; Response has lots of thoughts (~a tokens) but very little content (~a token~:*~P). Continuing prompt.~%" (get-thoughts-token-count usage-metadata)
                          (get-candidates-token-count usage-metadata))
                  (return-from %generate-content
                    (let* ((content (or (get-content first-candidate)
                                        (content :parts (list (part "[Empty Response]"))))))
                      (%generate-content content-generator
                                         (append prompt-contents-and-context (list content))
                                         mood
                                         prompt
                                         parts
                                         files
                                         system-instruction
                                         turbo
                                         (1+ depth)))))


                (unless (> (or (get-candidates-token-count usage-metadata) 0) 0)
                  (format *trace-output* "~&;; Response too thin, retrying with stronger model.~%")
                  (return-from %generate-content
                    (let* ((content (or (get-content first-candidate)
                                        (content :parts (list (part "[Empty Response]"))))))
                      (%generate-content content-generator
                                         (append prompt-contents-and-context (list content))
                                         mood
                                         (continue-prompt)
                                         parts
                                         files
                                         system-instruction
                                         (elt "$*%" (random (length "$*%")))
                                         (1+ depth))))))
                            
              (when *echo-result*
                (print-text (get-bowdlerize content-generator) response*))
                              
              (let ((function-calls (extract-function-calls-from-results response*)))
                (cond (function-calls
                       (let ((function-results
                               (map 'list (compose (default-process-function-call content-generator)
                                                   #'get-function-call)
                                    function-calls)))
                         (assert (list-of-parts? function-results) ()
                                 "Expected function-results to be a list of parts.")
                         ;; FIX: Append the function response to the context 
                         ;; and recurse with the ORIGINAL prompt.
                         (let ((function-content (content :parts function-results :role "function")))
                           (%generate-content content-generator
                                              ;; Append original prompt turn AND function result to context
                                              (append prompt-contents-and-context (list function-content))
                                              mood
                                              prompt ;; use original prompt to keep goal active
                                              parts
                                              files
                                              system-instruction
                                              turbo
                                              (1+ depth)))))
                      ;; note, we can return NIL here if the first candidate has no content.
                      (first-candidate (get-content first-candidate))))))
        (use-weaker-model ()
          :report (lambda (s)
                    (format s "Switch from ~a to ~a and retry." current-model +default-model+))
          (%generate-content content-generator context mood prompt parts files system-instruction nil (1+ depth)
                             :read-timeout read-timeout
                             :connect-timeout connect-timeout))))))

(defun compress-persona-memory (content-generator)
  "Compresses the persona memory file for the given content generator by extracting semantic entities and relations, and rewriting the file with only those compressed representations.  This helps to keep the persona memory concise and focused on key information."
  (let ((memory-pathname (persona-memory-file (get-config content-generator)))
        (compressed-memory-pathname (persona-compressed-memory-file (get-config content-generator))))
    (when (probe-file memory-pathname)
      (let ((memory-json nil))
        ;; Extract json
        (ignore-errors
         (with-open-file (stream memory-pathname :direction :input)
           (do ((json (cl-json:decode-json stream) (cl-json:decode-json stream)))
               ((null json) nil)
             (push json memory-json))))
        (with-open-file (out-stream compressed-memory-pathname
                                    :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
          (write-string
           (flash-compress
            (format nil "Semantic Entities:~%Entity Type, Name, Observation~%~{~{~a~^, ~}~%~}~%~%"
                    (mappend (lambda (record)
                               (map 'list (lambda (observation)
                                            (list (cdr (assoc :entity-type record))
                                                  (cdr (assoc :name record))
                                                  observation))
                                    (cdr (assoc :observations record))))
                             (sort
                              (remove "entity" memory-json
                                      :test-not #'equal
                                      :key (lambda (item) (cdr (assoc :type item))))
                              (lambda (l r)
                                (or (string< (cdr (assoc :entity-type l)) (cdr (assoc :entity-type r)))
                                    (and (string= (cdr (assoc :entity-type l)) (cdr (assoc :entity-type r)))
                                         (string< (cdr (assoc :name l)) (cdr (assoc :name r))))))))))
           out-stream)
          (terpri out-stream)
          (write-string
           (flash-compress
            (format nil "Semantic Relations:~%From, Relation Type, To~%~{~{~a~^, ~}~%~}~%~%"
                    (mapcar (lambda (x) 
                              (list (cdr (assoc :from x))
                                    (cdr (assoc :relation-type x))
                                    (cdr (assoc :to x))))
                            (sort
                             (cdr (remove "relation" memory-json
                                          :test-not #'equal
                                          :key (lambda (item) (cdr (assoc :type item)))))
                             (lambda (l r)
                               (or (string< (cdr (assoc :relation-type l)) (cdr (assoc :relation-type r)))
                                   (and (string< (cdr (assoc :relation-type l)) (cdr (assoc :relation-type r)))
                                        (string< (cdr (assoc :from l)) (cdr (assoc :from r))))))))))
           out-stream)
          (terpri out-stream))))))
