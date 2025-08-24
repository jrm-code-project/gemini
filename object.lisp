;;; -*- Lisp -*-

(in-package "GEMINI")

(define-standard-fields
  :args
  :autonym
  :behavior
  :blob
  :cached-content
  :candidate-count
  :candidates
  :candidates-token-count
  :capabilities
  :citation-metadata
  :client-info
  :code-execution-result
  :command
  :content
  :contents
  :data
  :description
  :enable-advanced-civic-answers
  :entities
  :enum
  :env
  :executable-code
  :file-data
  :finish-reason
  :format
  :frequency-penalty
  :function-call
  :function-declarations
  :function-response
  :generation-config
  :id 
  :include-context
  :index
  :inline-data
  :input-schema
  :instructions
  :items
  :jsonrpc
  :level
  :link
  :location
  :logprobs
  :max-output-tokens
  :max-tokens
  :maximum
  :mcp-servers
  :media-resolution
  :message
  :messages
  :metadata
  :method
  :mime-type
  :minimum
  :model-version
  :name
  :next-cursor
  :nullable
  :output-schema
  :parameters
  :parameters-json-schema
  :params
  :parts
  :path
  :presence-penalty
  :progress
  :progress-token
  :prompt-token-count
  :prompts
  :properties
  :protocol-version
  :query
  :reason
  :relation
  :relations
  :request-id
  :requested-schema
  :required
  :resource-templates
  :resources
  :response
  :response-id
  :response-json-schema
  :response-logprobs
  :response-mime-type
  :response-modalities
  :response-schema
  :result
  :role
  :safety-settings
  :search-terms
  :seed
  :server-info
  :snippet
  :source
  :speech-config
  :stop-sequences
  :subscribe
  :system-instruction
  :system-prompt
  :target
  :temperature
  :text
  :thinking-config
  :thought
  :thought-signature
  :thoughts-token-count
  :title
  :tool-config
  :tools
  :top-k
  :top-p
  :total
  :total-token-count
  ;; :type
  :uri
  :uri-template
  :usage-metadata
  :version)

(define-field :type %get-type)

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

(defun encode-schema-type (thing)
  (cond ((consp thing)
         (let ((car (car thing))
               (cdr (cdr thing)))
           (cond ((and (consp car)
                       (or (eq (car car) :$schema)
                           (equal (car car) "$schema")
                           (eq (car car) :additional-properties)
                           (equal (car car) "additional-properties")))
                  (encode-schema-type cdr))
                 ((or (eq car :type)
                      (equal car "type"))
                  (cond ((or (eq cdr :unspecified) (equal cdr "unspecified")) (cons car 0))
                        ((or (eq cdr :string) (equal cdr "string")) (cons car 1))
                        ((or (eq cdr :number) (equal cdr "number")) (cons car 2))
                        ((or (eq cdr :integer) (equal cdr "integer")) (cons car 3))
                        ((or (eq cdr :boolean) (equal cdr "boolean")) (cons car 4))
                        ((or (eq cdr :array) (equal cdr "array")) (cons car 5))
                        ((or (eq cdr :object) (equal cdr "object")) (cons car 6))
                        (t (let ((newcdr (encode-schema-type cdr)))
                             (if (eq cdr newcdr)
                                 thing
                                 (cons car newcdr))))))
                 (t
                  (let ((newcar (encode-schema-type car))
                        (newcdr (encode-schema-type cdr)))
                    (if (and (eq car newcar) (eq cdr newcdr))
                        thing
                        (cons newcar newcdr)))))))
        ((or (null thing)
             (symbolp thing)
             (stringp thing)
             (numberp thing)
             (typep thing 'json-boolean))
         thing)
        ((hash-table-p thing)
         (let* ((alist (hash-table-alist thing))
                (new-alist (mapcar #'encode-schema-type
                                   (remove :$schema
                                           (remove "$schema"
                                                   (remove :additional-properties
                                                           (remove "additional-properties"
                                                                   alist
                                                                   :key #'car :test #'equal)
                                                           :key #'car :test #'eql)
                                                   :key #'car :test #'equal)
                                           :key #'car :test #'eql))))
           (if (every #'eq alist new-alist)
               thing
               (alist-hash-table new-alist))))
        ((vectorp thing)
         (let ((new (map 'vector #'encode-schema-type thing)))
           (if (every #'eql thing new)
               thing
               new)))
        (t 
         (error "Cannot encode schema type for ~s" thing))))

(defun decode-schema-type (thing)
  (cond ((consp thing)
         (let ((car (car thing))
               (cdr (cdr thing)))
           (cond ((or (eq car :type)
                      (equal car "type"))
                  (cond ((eql cdr 0) (cons car :unspecified))
                        ((eql cdr 1) (cons car :string))
                        ((eql cdr 2) (cons car :number))
                        ((eql cdr 3) (cons car :integer))
                        ((eql cdr 4) (cons car :boolean))
                        ((eql cdr 5) (cons car :array))
                        ((eql cdr 6) (cons car :object))
                        (t (let ((newcdr (decode-schema-type cdr)))
                             (if (eq cdr newcdr)
                                 thing
                                 (cons car newcdr))))))
                 (t
                  (let ((newcar (decode-schema-type car))
                        (newcdr (decode-schema-type cdr)))
                    (if (and (eq car newcar) (eq cdr newcdr))
                        thing
                        (cons newcar newcdr)))))))
        ((or (null thing)
             (symbolp thing)
             (stringp thing))
         thing)
        ((hash-table-p thing)
         (let* ((alist (hash-table-alist thing))
                (new-alist (mapcar #'decode-schema-type alist)))
           (if (every #'eq alist new-alist)
               thing
               (alist-hash-table new-alist))))
        ((vectorp thing)
         (map 'vector #'decode-schema-type thing))
        (t 
         (error "Cannot decode schema type for ~s" thing))))

(defun get-type-enum (schema)
  "Retrieves the type of a schema object."
  (decode-schema-type-enum (%get-type schema)))

(defun set-type-enum! (schema type)
  "Sets the type of a schema object to TYPE.
   Valid types are 'string', 'number', 'integer', 'boolean', 'object', 'array'."
  (setf (%get-type schema) (encode-schema-type-enum type)))

(defsetf get-type-enum set-type-enum!)

(defun get-type (object)
  (%get-type object))

(defun set-type! (object type)
  (setf (%get-type object) type))

(defsetf get-type set-type!)

(deff blob?
    (is-object-test '(:data :mime-type))
    "Predicate to check if a thing is a valid blob object (inline data).")

(deff candidate?
    (is-object-test '(:content) '(:citation-metadata :finish-reason :index))
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
       (get-thought thing)))

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
          () "Parts must be a list of parts or NIL.")
  (let ((content (object :parts parts)))
    (when role
      (setf (get-role content) role))
    content))

(defun function-call (&key name args)
  "Creates a function call object with the specified NAME and ARGS.
   Returns a hash table representing the function call structure."
  (assert (or (hash-table-p args) (alist? args))
          () "Args must be a hash table or alist.")
  (object :name name :args args))

(defun function-declaration (&key name description behavior
                               (parameters nil parameters-supplied-p) parametersJsonSchema response responseJsonSchema)
  "Creates a function declaration object with the specified NAME, DESCRIPTION,
   BEHAVIOR, PARAMETERS, PARAMETERS-JSON-SCHEMA, RESPONSE, and RESPONSE-JSON-SCHEMA."
  (let ((declaration (object :name name :description description)))
    (when behavior
      (unless (eq behavior :blocking)
        (error "Only :blocking behavior is supported, got ~s." behavior))
      (setf (get-behavior declaration) behavior))
    (when parameters-supplied-p
      (setf (get-parameters declaration) parameters))
    (when parametersJsonSchema
      (setf (get-parameters-json-schema declaration) parametersJsonSchema))
    (when response
      (setf (get-response declaration) response))
    (when responseJsonSchema
      (setf (get-response-json-schema declaration) responseJsonSchema))
    declaration))

(defun part (data &key metadata thought thought-signature)
  "Creates a part object for content. The DATA can be a string (text),
   a blob, a function call, a function response, file data, executable code,
   or a code execution result. Optional METADATA, THOUGHT, and THOUGHT-SIGNATURE
   can be included. Returns a hash table representing the part."
  (let ((part (object)))
    (cond ((stringp data)                (setf (get-text part)                  data))
          ((blob? data)                  (setf (get-inline-data part)           data))
          ((function-call? data)         (setf (get-function-call part)         data))
          ((function-response? data)     (setf (get-function-response part)     data))
          ((file-data? data)             (setf (get-file-data part)             data))
          ((executable-code? data)       (setf (get-executable-code part)       data))
          ((code-execution-result? data) (setf (get-code-execution-result part) data))
          (t (error "Unrecognized data: ~s" data)))
    (when metadata
      (setf (get-metadata part) metadata))
    (when thought
      (setf (get-thought part) thought))
    (when thought-signature
      (setf (get-thought-signature part) thought-signature))
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

  (let ((schema (object)))
    (setf (get-type-enum schema) type)
    (when format
      (setf (get-format schema) format))
    (when items
      (setf (get-items schema) items))
    (when title
      (setf (get-title schema) title))
    (when description
      (setf (get-description schema) description))
    (when nullable-supplied-p
      (setf (get-nullable schema) nullable))
    (when properties-supplied-p
      (setf (get-properties schema) properties))
    (when required-supplied-p
      (setf (get-required schema) required))
    schema))
