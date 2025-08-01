;;; -*- Lisp -*-

(in-package "GEMINI")

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

(defun default-functions ()
  (list (cons "lispEcho" (lambda (&key text)
                                       (if text
                                           (format nil "the literal string \"~s\"" text)
                                           "No text provided.")))
        (cons "lispImplementationType" #'lisp-implementation-type)
        (cons "lispImplementationType" #'lisp-implementation-type)
        (cons "lispImplementationVersion" #'lisp-implementation-version)
        (cons "listPackages" (lambda () (map 'list #'package-name (list-all-packages))))
        (cons "packageDocumentation" (lambda (&key package)
                                       ;(format t "~&Package: ~a~%" package)
                                       (let ((p (find-package (string-upcase package))))
                                         (or (and p
                                                  (or (sb-kernel:package-doc-string p)
                                                      ""))
                                             ""))))
        (cons "ping" (lambda () (values)))
        (cons "symbolValue" (lambda (&key symbol)
                                       (let ((sym (find-symbol symbol)))
                                         (if sym
                                             (format nil "~s" (symbol-value sym))
                                             (format nil "Symbol ~a not found." symbol)))))
        ))

(defun default-function-declarations ()
  (if (boundp '*function-declarations*)
      *function-declarations*
      ;; Add default function declarations here
      ;; Example:
      (list
       (function-declaration
        :name "lispEcho"
        :description "Echoes the argument."
        :behavior :blocking
        :parameters (schema :type :unspecified)
        :response (schema :type :string))

       (function-declaration
        :name "lispImplementationType"
        :description (or (documentation 'lisp-implementation-type 'function)
                        "Returns the type of the Lisp implementation.")
        :behavior :blocking
        :response (schema :type :string))

       (function-declaration
        :name "lispImplementationVersion"
        :description (or (documentation 'lisp-implementation-version 'function)
                        "Returns the version of the Lisp implementation.")
        :behavior :blocking
        :response (schema :type :string))

       (function-declaration
        :name "listPackages"
        :description "Returns a list of all packages in the Lisp environment."
        :behavior :blocking
        :response (schema :type :array
                          :items (schema :type :string)))

       (function-declaration
        :name "noHandler"
        :description "This function is missing its handler.")

       (function-declaration
        :name "packageDocumentation"
        :description "Returns the documentation string for a package."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list (cons "package" (schema :type :string)))
                            :required (list "package"))
        :response (schema :type :string))

       (function-declaration
        :name "ping"
        :description "Detects whether the model client responds to function calls.")

       (function-declaration
        :name "symbolValue"
        :description "Returns the value of a symbol in the Lisp environment."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list (cons "symbol" (schema :type :string)))
                            :required (list "symbol"))
        :response (schema :type :string
                          :description "The printed representation of the value of symbol, or an error message if the symbol is not found."))
       )))

(defun default-tools ()
  "Returns the value of *TOOLS* if it is bound, otherwise NIL.
   Provides a default tools object for generation."
  (if (boundp '*tools*)
      *tools*
    (let ((tools (make-hash-table :test 'equal)))
      (let ((function-declarations (default-function-declarations)))
        (when function-declarations
          (setf (gethash "functionDeclarations" tools) function-declarations)))
      (unless (zerop (hash-table-count tools))
        tools))))

(defun default-generation-config ()
  "Returns a default generation configuration object.
   It constructs a hash table by combining various default settings
   related to candidate generation, safety, and response formatting."
  (if (boundp '*generation-config*)
      *generation-config*
      (let ((gen-config (make-hash-table :test 'equal)))
        (let ((candidate-count (default-candidate-count)))
          (when candidate-count
            (setf (gethash "candidateCount" gen-config) candidate-count)))
        (let ((enable-advanced-civic-answers (default-enable-advanced-civic-answers)))
          (when enable-advanced-civic-answers
            (setf (gethash "enableAdvancedCivicAnswers" gen-config) enable-advanced-civic-answers)))
        (let ((frequency-penalty (default-frequency-penalty)))
          (when frequency-penalty
            (setf (gethash "frequencyPenalty" gen-config) frequency-penalty)))
        (let ((max-output-tokens (default-max-output-tokens)))
          (when max-output-tokens
            (setf (gethash "maxOutputTokens" gen-config) max-output-tokens)))
        (let ((media-resolution (default-media-resolution)))
          (when media-resolution
            (setf (gethash "mediaResolution" gen-config) media-resolution)))
        (let ((presence-penalty (default-presence-penalty)))
          (when presence-penalty
            (setf (gethash "presencePenaly" gen-config) presence-penalty)))
        (let ((response-logprobs (default-response-logprobs)))
          (when response-logprobs
            (setf (gethash "responseLogprobs" gen-config) response-logprobs)))
        (let ((logprobs (default-logprobs)))
          (when logprobs
            (assert (gethash "responseLogprobs" gen-config)
                    () "Response logprobs must be set when logprobs is set.")
            (setf (gethash "logprobs" gen-config) logprobs)))
        (let ((response-mime-type (default-response-mime-type)))
          (when response-mime-type
            (setf (gethash "responseMimeType" gen-config) response-mime-type)))
        (let ((response-modalities (default-response-modalities)))
          (when response-modalities
            (setf (gethash "responseModalities" gen-config) response-modalities)))
        (let ((response-schema (default-response-schema)))
          (when response-schema
            (assert (gethash "responseMimeType" gen-config)
                    () "Response MIME type must be set.")
            (setf (gethash "responseSchema" gen-config) response-schema)))
        (let ((response-json-schema (default-response-json-schema)))
          (when response-json-schema
            (assert (gethash "responseMimeType" gen-config)
                    () "Response MIME type must be set.")
            (assert (not (gethash "responseSchema" gen-config))
                    () "Response schema must not be set when response JSON schema is set.")
            (setf (gethash "responseJsonSchema" gen-config) response-json-schema)))
        (let ((seed (default-seed)))
          (when seed
            (setf (gethash "seed" gen-config) seed)))
        (let ((speech-config (default-speech-config)))
          (when speech-config
            (setf (gethash "speechConfig" gen-config) speech-config)))
        (let ((stop-sequences (default-stop-sequences)))
          (when stop-sequences
            (setf (gethash "stopSequences" gen-config) stop-sequences)))
        (let ((temperature (default-temperature)))
          (when temperature
            (setf (gethash "temperature" gen-config) temperature)))
        (let ((thinking-config (default-thinking-config)))
          (when thinking-config
            (setf (gethash "thinkingConfig" gen-config) thinking-config)))
        (let ((top-k (default-top-k)))
          (when top-k
            (setf (gethash "topK" gen-config) top-k)))
        (let ((top-p (default-top-p)))
          (when top-p
            (setf (gethash "topP" gen-config) top-p)))
        (unless (zerop (hash-table-count gen-config))
          gen-config))))

(defun default-process-part (part)
  "Processes a single part object. If it's a text object, it extracts
   and returns the text. Otherwise, it returns the part as is."
  (cond ((text-part? part) (get-text part))
        (t nil)))

(defun process-thought (thought)
  (format *trace-output* "~&~{;; ~a~%~}"
          (mapcar #'str:trim
                  (str:split #\newline (get-text thought)))))

(defun process-thoughts (thoughts)
  (mapc #'process-thought thoughts))

(defun default-process-arg-value (arg schema)
  (ecase (get-type schema)
    (:string arg)))

(defun default-process-arg (arg schema)
  (list (car arg)
        (default-process-arg-value
         (cdr arg)
         (cdr (assoc (car arg) schema
                     :key #'->keyword)))))
          
(defun default-process-args (args schema)
  (mappend (lambda (arg) (default-process-arg arg schema)) args))

(defun default-process-function-call (function-call-part)
  (let* ((name (get-name function-call-part))
         (args (get-args function-call-part))
         (schema (get-properties
                  (get-parameters
                   (find name (default-function-declarations)
                         :key #'get-name :test #'equal))))
         (handler (cdr (assoc name (default-functions) :test #'equal)))
         (response-part (make-hash-table :test 'equal)))
    (setf (gethash "functionResponse" response-part)
          (function-response
           :name name
           :response (cond ((null handler)
                            `((:error . ,(format nil "No handler for ~s." name))))
                           ((not (functionp handler))
                            `((:error . ,(format nil "Handler for ~s is not a function." name))))
                           (t (let* ((answer (apply handler (default-process-args args schema)))
                                     (response (make-hash-table :test 'equal)))
                                (setf (gethash "result" response) answer)
                                response)))))
    response-part))

(defun default-process-function-calls (parts)
  "Processes a list of function call part objects.
   Returns a list of processed function call responses."
  (invoke-gemini *model*
                 (content :parts
                          (mapcar (lambda (part)
                                    (default-process-function-call (get-function-call part)))
                                  parts)
                          :role "function")))

(defun default-process-content (content)
  "Processes a content object. If the role is 'model' and it contains
   a single part, it processes that part. Otherwise, it returns the
   content as is."
  (if (equal (get-role content) "model")
      (let* ((*history* (cons content *history*))
             (raw-parts (get-parts content))
             (thoughts (remove-if-not #'thought-part? raw-parts))
             (parts (remove-if #'thought-part? raw-parts)))
        ;(format t "~&History: ~s~%" *history*)
        (process-thoughts thoughts)
        (cond ((list-of-function-calls? parts) (default-process-function-calls parts))
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

(defun default-process-response (response)
  "Processes an API response object.
   If the response contains one candidate, process that candidate.
   Otherwise return the list of candidates."
  (if (gemini-response? response)
      (let ((candidates (get-candidates response)))
        (if (singleton-list-of-candidates? candidates)
            (default-process-candidate (car candidates))
            candidates))
      (error "Unrecognized Gemini response ~s" response)))

(defvar *output-processor* #'default-process-response
  "Function to process the output of the Gemini API.
   Can be set to a custom function to handle the response differently.
   Defaults to DEFAULT-PROCESS-RESPONSE.")

(defun invoke-gemini (*model* contents &key
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
  (let* ((payload (make-hash-table :test 'equal))
         (*history*
           (cond ((content? contents) (cons contents *history*))
                 ((part? contents) (cons (content :parts (list contents) :role "user") *history*))
                 ((stringp contents) (cons (content :parts (list (part contents)) :role "user") *history*))
                 ((list-of-content? contents) (revappend contents *history*))
                 ((list-of-parts? contents) (cons (content :parts contents :role "user") *history*))
                 ((list-of-strings? contents) (cons (content :parts (mapcar #'part contents) :role "user") *history*))
                 (t (error "Unrecognized contents: ~s" contents)))))
    (setf (gethash "contents" payload) (reverse *history*))
    (when cached-content
      (setf (gethash "cachedContent" payload) cached-content))
    (when generation-config
      (setf (gethash "generationConfig" payload) generation-config))
    (when safety-settings
      (setf (gethash "safetySettings" payload) safety-settings))
    (when system-instruction
      (setf (gethash "systemInstruction" payload) system-instruction))
    (when tools
      (setf (gethash "tools" payload) tools))
    (when tool-config
      (setf (gethash "toolConfig" payload) tool-config))
    (funcall *output-processor* (%invoke-gemini *model* payload))))
