;;; -*- Lisp -*-

(in-package "GEMINI")

(deff get-args (object-ref-function :args)
    "Retrieves the 'args' field from an object, typically used in function calls.")

(deff get-candidates (object-ref-function :candidates)
    "Retrieves the 'candidates' field from an object.")

(deff get-candidates-token-count (object-ref-function :candidates-token-count)
    "Retrieves the 'candidatesTokenCount' field from an object, typically used in API responses.")

(deff get-content (object-ref-function :content)
    "Retrieves the 'content' field from an object.")

(deff get-finish-reason (object-ref-function :finish-reason)
    "Retrieves the 'finishReason' field from an object.")

(deff get-function-call (object-ref-function :function-call)
    "Retrieves the 'functionCall' field from an object.")

(deff get-name (object-ref-function :name)
    "Retrieves the 'name' field from an object.")

(deff get-parameters (object-ref-function :parameters)
    "Retrieves the 'parameters' field from an object, typically used in function calls.")

(deff get-parts (object-ref-function :parts)
    "Retrieves the 'parts' field from an object.")

(deff get-properties (object-ref-function :properties)
    "Retrieves the 'properties' field from an object.")

(deff get-prompt-token-count (object-ref-function :prompt-token-count)
    "Retrieves the 'promptTokenCount' field from an object, typically used in API responses.")

(deff get-role (object-ref-function :role)
    "Retrieves the 'role' field from an object.")

(deff get-text (object-ref-function :text)
    "Retrieves the 'text' field from an object.")

(deff get-thought-flag (object-ref-function :thought)
    "Retrieves the 'thought' field from an object.")

(deff get-thoughts-token-count (object-ref-function :thoughts-token-count)
    "Retrieves the 'thoughtsTokenCount' field from an object, typically used in API responses.")

(deff get-total-token-count (object-ref-function :total-token-count)
    "Retrieves the 'totalTokenCount' field from an object, typically used in API responses.")

(deff %get-type (object-ref-function :type)
    "Retrieves the 'type' field from an object, typically used in schemas.")

(deff get-usage-metadata (object-ref-function :usage-metadata)
    "Retrieves the 'usageMetadata' field from an object.")

(defun decode-schema-type-enum (code)
  "Returns a schema type for the given CODE."
  (ecase code
    (0 :unspecified)
    (1 :string)
    (2 :number)
    (3 :integer)
    (4 :boolean)
    (5 :array)
    (6 :object)))

(defun encode-schema-type-enum (type)
  "Returns a schema type for the given TYPE.
   Valid types are 'string', 'number', 'integer', 'boolean', 'object', 'array'."
  (ecase type
    (:unspecified 0)
    (:string 1)
    (:number 2)
    (:integer 3)
    (:boolean 4)
    (:array 5)
    (:object 6)))

(defun get-type (schema)
  "Retrieves the type of a schema object."
  (decode-schema-type-enum (%get-type schema)))

(deff blob?
    (is-object-test '(:data :mime-type))
    "Predicate to check if a thing is a valid blob object (inline data).")

(deff candidate?
    (is-object-test '(:content) '(:finish-reason :index))
    "Predicate to check if a thing is a valid candidate object from the API response.")

(deff singleton-list-of-candidates? (singleton-list-of-test #'candidate?))

(deff content?
    (is-object-test '(:parts) '(:role))
    "Predicate to check if a thing is a valid content object.")

(deff list-of-content? (list-of-test #'content?)
    "Predicate to check if a thing is a list of content objects.")

(deff singleton-list-of-content? (singleton-list-of-test #'content?)
    "Predicate to check if a thing is a singleton list of a content object.")

(deff function-call?
  (is-object-test '(:name) '(:args :id))
    "Predicate to check if a thing is a valid function call.")

(deff function-response?
  (is-object-test '(:name :response)
                  '(:id :scheduling :will-continue))
    "Predicate to check if a thing is a valid function response.")

(deff file-data?
    (is-object-test '(:file-uri) '(:mime-type))
    "Predicate to check if a thing is a valid file data.")

(deff executable-code?
  (is-object-test '(:code :language))
  "Predicate to check if a thing is a valid executable code.")

(deff code-execution-result?
    (is-object-test '(:outcome) '(:output))
    "Predicate to check if a thing is a valid code execution result.")

(deff text-part?
    (is-object-test '(:text) '(:thought :thought-signature))
    "Predicate to check if a thing is a text part.")

(defun thought-part? (thing)
  (and (text-part? thing)
       (get-thought-flag thing)))

(deff code-execution-result-part?
    (is-object-test '(:code-execution-result))
    "Predicate to check if a thing is a code execution result part object.")

(deff executable-code-part?
    (is-object-test '(:executable-code))
    "Predicate to check if a thing is an executable code part object.")

(deff file-data-part?
    (is-object-test '(:file-data))
    "Predicate to check if a thing is a file data part object.")

(deff function-call-part?
    (is-object-test '(:function-call) '(:thought-signature))
    "Predicate to check if a thing is a function call part object.")

(deff list-of-function-calls?
    (list-of-test #'function-call-part?)
    "Predicate to check if a thing is a list of function call part objects.")

(deff function-response-part?
    (is-object-test '(:function-response))
    "Predicate to check if a thing is a function response part object.")

(deff inline-data-part?
    (is-object-test '(:inline-data))
    "Predicate to check if a thing is an inline data part object.")

(defun part? (thing)
  "Predicate to check if a thing is any valid part object."
  (or (text-part? thing)
      (inline-data-part? thing)
      (function-call-part? thing)
      (function-response-part? thing)
      (file-data-part? thing)
      (executable-code-part? thing)
      (code-execution-result-part? thing)))

(deff list-of-parts? (list-of-test #'part?)
  "Predicate to check if a thing is a list of part objects.")

(deff singleton-list-of-parts? (singleton-list-of-test #'part?)
  "Predicate to check if a thing is a singleton list of a part object.")

(deff list-of-strings? (list-of-test #'stringp)
    "Predicate to check if a thing is a list of strings.")

(deff gemini-response?
    (is-object-test '(:candidates) '(:model-version :response-id :usage-metadata))
    "Predicate to check if thing is a gemini response.")

(defun content (&key role (parts (error "Content must contain PARTS.")))
  "Creates a content object with the specified ROLE and PARTS.
   Returns a hash table representing the content structure for API requests."
  (assert (or (null role) (stringp role))
          () "Role must be a string or NIL.")
  (assert (list-of-parts? parts)
          () "Parts must be a list or NIL.")
  (let ((content (make-hash-table :test 'equal)))
    (when role
      (setf (gethash "role" content) role))
    (setf (gethash "parts" content) parts)
    content))

(defun function-call (&key name args)
  "Creates a function call object with the specified NAME and ARGS.
   Returns a hash table representing the function call structure."
  (let ((call (make-hash-table :test 'equal)))
    (setf (gethash "name" call) name)
    (setf (gethash "args" call) args)
    call))

(defun function-declaration (&key name description behavior
                               (parameters nil parameters-supplied-p) parametersJsonSchema response responseJsonSchema)
  "Creates a function declaration object with the specified NAME, DESCRIPTION,
   BEHAVIOR, PARAMETERS, PARAMETERS-JSON-SCHEMA, RESPONSE, and RESPONSE-JSON-SCHEMA."
  (let ((declaration (make-hash-table :test 'equal)))
    (setf (gethash "name" declaration) name)
    (setf (gethash "description" declaration) description)
    (when behavior
      (unless (eq behavior :blocking)
        (error "Only :blocking behavior is supported, got ~s." behavior))
      (setf (gethash "behavior" declaration) behavior))
    (when parameters-supplied-p
      (setf (gethash "parameters" declaration) parameters))
    (when parametersJsonSchema
      (setf (gethash "parametersJsonSchema" declaration) parametersJsonSchema))
    (when response
      (setf (gethash "response" declaration) response))
    (when responseJsonSchema
      (setf (gethash "responseJsonSchema" declaration) responseJsonSchema))
    declaration))

(defun function-response (&key name response)
  "Creates a function response object with the specified NAME and RESPONSE.
   Returns a hash table representing the function response structure."
  (let ((resp (make-hash-table :test 'equal)))
    (setf (gethash "name" resp) name)
    (setf (gethash "response" resp) response)
    resp))

(defun part (data &key metadata thought thought-signature)
  "Creates a part object for content. The DATA can be a string (text),
   a blob, a function call, a function response, file data, executable code,
   or a code execution result. Optional METADATA, THOUGHT, and THOUGHT-SIGNATURE
   can be included. Returns a hash table representing the part."
  (let ((part (make-hash-table :test 'equal)))
    (cond ((stringp data) (setf (gethash "text" part) data))
          ((blob? data) (setf (gethash "inlineData" part) data))
          ((function-call? data) (setf (gethash "functionCall" part) data))
          ((function-response? data) (setf (gethash "functionResponse" part) data))
          ((file-data? data) (setf (gethash "fileData" part) data))
          ((executable-code? data) (setf (gethash "executableCode" part) data))
          ((code-execution-result? data) (setf (gethash "codeExecutionResult" part) data))
          (t (error "Unrecognized data: ~s" data)))
    (when metadata
      (setf (gethash "metadata" part) metadata))
    (when thought
      (setf (gethash "thought" part) thought))
    (when thought-signature
      (setf (gethash "thoughtSignature" part) thought-signature))
    part))

(defun schema (&key type format title description
                 (nullable nil nullable-supplied-p)
                 (enum nil enum-supplied-p)
                 (max-items 0 max-items-supplied-p)
                 (min-items 0 min-items-supplied-p)
                 (properties nil properties-supplied-p)
                 (required nil required-supplied-p)
                 (min-properties 0 min-properties-supplied-p)
                 (max-properties 0 max-properties-supplied-p)
                 (min-length 0 min-length-supplied-p)
                 (max-length 0 max-length-supplied-p)
                 pattern
                 example
                 any-of
                 property-ordering
                 default
                 items
                 (minimum 0 minimum-supplied-p)
                 (maximum 0 maximum-supplied-p))
  "Creates a JSON schema object with the specified TYPE, FORMAT, TITLE,
   DESCRIPTION, PROPERTIES, and REQUIRED fields."
  (declare (ignore enum enum-supplied-p
                   max-items max-items-supplied-p
                   min-items min-items-supplied-p
                   min-properties min-properties-supplied-p
                   max-properties max-properties-supplied-p
                   min-length min-length-supplied-p
                   max-length max-length-supplied-p
                   pattern
                   example
                   any-of
                   property-ordering
                   default
                   minimum minimum-supplied-p
                   maximum maximum-supplied-p))

  (let ((schema (make-hash-table :test 'equal)))
    (setf (gethash "type" schema) (encode-schema-type-enum type))
    (when format
      (setf (gethash "format" schema) format))
    (when items
      (setf (gethash "items" schema) items))
    (when title
      (setf (gethash "title" schema) title))
    (when description
      (setf (gethash "description" schema) description))
    (when nullable-supplied-p
      (setf (gethash "nullable" schema) nullable))
    (when properties-supplied-p
      (setf (gethash "properties" schema) properties))
    (when required-supplied-p
      (setf (gethash "required" schema) required))
    schema))


