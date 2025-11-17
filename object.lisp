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
  :code
  :code-execution-result
  :command
  :completions
  :content
  :contents
  :data
  :description
  :enable-advanced-civic-answers
  :entities
  :enum
  :env
  :error
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
  :include-bash-history
  :include-context
  :include-timestamp
  :index
  :inline-data
  :input-schema
  :instructions
  :items
  :jsonrpc
  :keepalive-interval
  :level
  :link
  :list-changed
  :location
  :logging
  :logprobs
  :max-output-tokens
  :max-tokens
  :maximum
  :mcp-servers
  :media-resolution
  :memory-mcp-server
  :message
  :messages
  :metadata
  :method
  :mime-type
  :minimum
  :model
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
  :server-instructions
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
  :total-tokens
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
        ((or (symbolp thing)
             (stringp thing)
             (numberp thing)
             (typep thing 'jsonx:json-literal))
         thing)
        ((null thing)
         (object))
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

(defclass persona-config ()
  ((name :initarg :name :accessor get-name)
   (cached-content       :initarg :cached-content       :initform nil :accessor get-cached-content)
   (diary-directory      :initarg :diary-directory      :initform (make-pathname :directory (list :relative "Diary"))
                         :accessor get-diary-directory)
   (enable-evolution-tools :initarg :enable-evolution-tools :initform nil :accessor get-enable-evolution-tools)
   (enable-eval          :initarg :enable-eval          :initform nil :accessor get-enable-eval)
   (enable-filesystem-tools :initarg :enable-filesystem-tools :initform nil :accessor get-enable-filesystem-tools)
   (enable-git-tools     :initarg :enable-git-tools     :initform nil :accessor get-enable-git-tools)
   (enable-gnutils       :initarg :enable-gnutils-tools :initform nil :accessor get-enable-gnutils)
   (enable-interaction-tools :initarg :enable-interaction-tools :initform nil :accessor get-enable-interaction-tools)
   (enable-lisp-introspection-tools :initarg :enable-lisp-introspection-tools :initform nil :accessor get-enable-lisp-introspection-tools)
   (enable-misc-tools    :initarg :enable-misc-tools    :initform nil :accessor get-enable-misc-tools)
   (enable-shell-tools   :initarg :enable-shell-tools   :initform nil :accessor get-enable-shell-tools)
   (enable-web-tools     :initarg :enable-web-tools     :initform nil :accessor get-enable-web-tools)
   (generation-config    :initarg :generation-config :accessor %get-generation-config)
   (include-bash-history :initarg :include-bash-history :initform nil :accessor get-include-bash-history)
   (include-timestamp    :initarg :include-timestamp    :initform nil :accessor get-include-timestamp)
   (memory-filepath      :initarg :memory-filepath      :initform (make-pathname :name "memory" :type "json")
                         :accessor get-memory-filepath)
   (model                :initarg :model                :initform +default-model+ :accessor get-model)
   (narrative-memory     :initarg :narrative-memory     :initform nil :reader get-narrative-memory)
   (system-instruction-filepath :initarg :system-instruction-filepath
                                :initform (make-pathname :name "system-instruction" :type "md")
                                :accessor get-system-instruction-filepath)
   (system-instructions-filepath :initarg :system-instructions-filepath
                                 :initform (make-pathname :name "system-instructions" :type "md")
                                 :accessor get-system-instructions-filepath)
   (safety-settings      :initarg :safety-settings      :initform nil :accessor get-safety-settings)
   (temperature          :initarg :temperature          :accessor get-temperature)
   (tool-config          :initarg :tool-config          :initform nil :accessor get-tool-config))

  (:documentation "Class representing persona configuration."))

(defmethod get-generation-config ((object persona-config))
  (if (slot-boundp object 'generation-config)
      (slot-value object 'generation-config)
      (let ((generation-config (object)))

        (when (and (slot-boundp object 'safety-settings)
                   (slot-value object 'safety-settings))
          (setf (get-safety-settings generation-config) (get-safety-settings object)))

        (when (slot-boundp object 'temperature)
          (setf (get-temperature generation-config) (get-temperature object)))

        (unless (zerop (hash-table-count generation-config))
          generation-config))))

(defmethod get-system-instruction ((object persona-config))
  (let ((filepath (persona-system-instruction-filepath object)))
    (and (probe-file filepath)
         (list (uiop:read-file-string filepath)))))

(defmethod get-system-instructions ((object persona-config))
  (let ((filepath (persona-system-instructions-filepath object)))
    ;; (format *trace-output* "Fetching system instruction from file ~s for persona ~A.~%" filepath (get-name object))
    ;; (finish-output *trace-output*)
    (and (probe-file filepath)
         (file->paragraphs filepath))))

(defclass content-generator ()
  ((config :initarg :config :accessor get-config)
   (memory-mcp-server :reader get-memory-mcp-server))
  
  (:documentation "Class representing a content generator with a persona configuration.")
  (:metaclass sb-mop:funcallable-standard-class))

(defmethod initialize-instance :after ((instance content-generator) &key config &allow-other-keys)
  "Initializes the content generator instance."
  (sb-mop:set-funcallable-instance-function
   instance
   (lambda (prompt &key files system-instruction)
     (generate-content instance prompt files system-instruction))))

(defmethod shared-initialize :after ((instance content-generator) slot-names &key config &allow-other-keys)
  "Initializes the content generator instance."
  (setf (slot-value instance 'memory-mcp-server)
        (memory-mcp-server (persona-memory-file config))))

(defmethod get-cached-content ((object content-generator))
  (get-cached-content (get-config object)))

(defmethod get-diary-directory ((object content-generator))
  (get-diary-directory (get-config object)))

(defmethod get-include-bash-history ((object content-generator))
  (get-include-bash-history (get-config object)))

(defmethod get-include-timestamp ((object content-generator))
  (get-include-timestamp (get-config object)))

(defmethod get-generation-config ((object content-generator))
  (get-generation-config (get-config object)))

(defmethod get-model ((object content-generator))
  (get-model (get-config object)))

(defmethod get-name ((object content-generator))
  (get-name (get-config object)))

(defmethod get-safety-settings ((object content-generator))
  (get-safety-settings (get-config object)))

(defmethod get-system-instruction ((object content-generator))
  ;; (format *trace-output* "Fetching system instruction for content generator.~%")
  ;; (finish-output *trace-output*)
  (or (get-system-instruction (get-config object))
      (get-system-instructions (get-config object))))

(defmethod get-tools ((object content-generator))
  "Returns a vector containing a single Tool object, which holds all function declarations."
  (let ((declarations (map 'vector #'car (standard-functions-and-handlers object))))
    (when (and declarations (> (length declarations) 0))
      ;; The API expects a *vector* of Tool objects.
      ;; We are providing one Tool object that contains all declarations.
      (vector (object :function-declarations declarations)))))

(defmethod get-tool-config ((object content-generator))
  "Fetch the tool configuration for the content generator."
  (get-tool-config (get-config object)))
