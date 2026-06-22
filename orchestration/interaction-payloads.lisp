;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defmethod content->json ((content text-content))
  (let ((content-json (make-hash-table :test 'equal)))
    (setf (gethash "type" content-json) "text")
    (setf (gethash "text" content-json) (get-text content))
    (when (slot-boundp content 'annotation)
      (setf (gethash "annotation" content-json) (annotation->json (get-annotation content))))
    content-json))

(defun lmstudio-output-item->candidate-parts (item)
  (let ((type (adapter-field item "type" :type)))
    (cond
      ((string-equal type "reasoning")
       (let ((content (adapter-field item "content" :content)))
         (when (and (stringp content)
                    (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) content)) 0))
           (list (part content :thought t)))))
      ((string-equal type "message")
       (let ((content (adapter-field item "content" :content)))
         (when content
           (list (part content)))))
      ((string-equal type "tool_call")
       nil)
      (t nil))))

(defun lmstudio-stats->gemini-usage (stats)
  (when stats
    (let* ((usage (object))
           (input-tokens (adapter-field stats "input_tokens" :input_tokens :input-tokens :input--tokens))
           (total-output-tokens (adapter-field stats "total_output_tokens"
                                               :total_output_tokens
                                               :total-output-tokens
                                               :total--output--tokens))
           (reasoning-output-tokens (adapter-field stats "reasoning_output_tokens"
                                                   :reasoning_output_tokens
                                                   :reasoning-output-tokens
                                                   :reasoning--output--tokens)))
      (when input-tokens
        (setf (get-prompt-token-count usage) input-tokens))
      (when reasoning-output-tokens
        (setf (get-thoughts-token-count usage) reasoning-output-tokens))
      (when total-output-tokens
        (setf (get-candidates-token-count usage)
              (max 0 (- total-output-tokens (or reasoning-output-tokens 0)))))
      (when (> (hash-table-count usage) 0)
        usage))))

(defun lmstudio-error-message (event)
  (let ((error-payload (adapter-field event "error" :error)))
    (or (and error-payload
             (adapter-field error-payload "message" :message))
        (adapter-field event "message" :message)
        "LM Studio stream reported an unknown error.")))

(defun lmstudio-result->gemini-response (result)
  "Normalizes an LM Studio chat.end result into a Gemini-style response and usage."
  (let* ((output (adapter-as-list (adapter-field result "output" :output)))
         (parts (mappend #'lmstudio-output-item->candidate-parts output))
         (usage (lmstudio-stats->gemini-usage (adapter-field result "stats" :stats)))
         (response-id (adapter-field result
                                     "response_id"
                                     :response_id
                                     :response-id
                                     :response--id))
         (model-instance-id (adapter-field result
                                           "model_instance_id"
                                           :model_instance_id
                                           :model-instance-id
                                           :model--instance--id))
         (normalized (object :candidates (and parts
                                             (list (object :content (content :role "model" :parts parts)))))))
    (when (null parts)
      (error "LM Studio result contained no result parts. Response id: ~A"
             response-id))
    (when response-id
      (setf (get-response-id normalized) response-id))
    (when model-instance-id
      (setf (get-model-version normalized) model-instance-id))
    (when usage
      (setf (get-usage-metadata normalized) usage))
    (values normalized usage)))

(defun lmstudio-content->input-text (content)
  (let ((parts (adapter-as-list (adapter-field content "parts" :parts))))
    (unless (every (lambda (part)
                     (stringp (adapter-field part "text" :text)))
                   parts)
      (error "LM Studio input currently supports text-only content objects."))
    (format nil "~{~A~^~%~}"
            (mapcar (lambda (part)
                      (adapter-field part "text" :text))
                    parts))))

(defun lmstudio-content-list->input-text (contents)
  (let ((content-list (coerce contents 'list)))
    (cond ((null content-list) nil)
          ((= 1 (length content-list))
           (lmstudio-content->input-text (first content-list)))
          (t
           (format nil "~{~A~^~%~%~}"
                   (mapcar (lambda (content)
                             (let ((role (get-role content))
                                   (text (lmstudio-content->input-text content)))
                               (if role
                                   (format nil "~A: ~A" role text)
                                   text)))
                           content-list))))))

(defun normalize-lmstudio-input (input)
  (cond
    ((null input) nil)
    ((stringp input) input)
    ((typep input 'content)
     (lmstudio-content->input-text input))
    ((legacy-content-json-p input)
     (lmstudio-content->input-text input))
    ((vectorp input)
     (normalize-lmstudio-input (coerce input 'list)))
    ((listp input)
     (cond
       ((every (lambda (item) (typep item 'content)) input)
        (lmstudio-content-list->input-text input))
       ((every #'legacy-content-json-p input)
        (lmstudio-content-list->input-text input))
       ((every #'hash-table-p input)
        (coerce input 'vector))
       (t
        (error "Unsupported LM Studio input shape: ~S" input))))
    (t
     (error "Unsupported LM Studio input shape: ~S" input))))

(defun legacy-contents-latest-turn-text (contents)
  "Extracts the latest text turn from a legacy Gemini CONTENTS payload."
  (let* ((contents-list (adapter-as-list contents))
         (last-turn (car (last contents-list)))
         (parts (and last-turn (adapter-field last-turn "parts" :parts)))
         (text-list (and parts
                         (remove nil
                                 (map 'list
                                      (lambda (part)
                                        (adapter-field part "text" :text))
                                      (adapter-as-list parts))))))
    (when text-list
      (format nil "~{~A~^ ~}" text-list))))

(defun lmstudio-tool-bridge-url ()
  "Returns the configured URL for the LM Studio Gemini-tool MCP bridge, or NIL."
  (or (uiop:getenv "GEMINI_LMSTUDIO_TOOL_BRIDGE_URL")
      (active-lmstudio-tool-bridge-url)))

(defun ensure-lmstudio-tool-bridge-url (&key content-generator)
  "Returns a usable LM Studio MCP bridge URL, auto-starting the local bridge when possible."
  (or (lmstudio-tool-bridge-url)
      (when content-generator
        (start-lmstudio-tool-bridge)
        (active-lmstudio-tool-bridge-url))))

(defun lmstudio-tool-declarations (gemini-tools)
  "Returns a flat list of Gemini function declarations from GEMINI-TOOLS."
  (let ((declarations '()))
    (dolist (tool (adapter-as-list gemini-tools))
      (dolist (decl (adapter-as-list (get-function-declarations tool)))
        (push decl declarations)))
    (nreverse declarations)))

(defun lmstudio-schema-type-keyword (schema path)
  "Normalizes SCHEMA's type value to a keyword for validation."
  (let ((raw-type (adapter-field schema "type" :type)))
    (cond ((null raw-type)
           (error "LM Studio tool translation requires ~a.type to be present." path))
          ((integerp raw-type)
           (decode-schema-type-enum raw-type))
          ((keywordp raw-type)
           raw-type)
          ((stringp raw-type)
           (->keyword raw-type))
          (t
           (error "LM Studio tool translation requires ~a.type to be a schema type, got ~S."
                  path raw-type)))))

(defun validate-lmstudio-tool-schema (schema path &key rootp)
  "Validates that SCHEMA is within the currently supported LM Studio MCP-bridge subset."
  (when schema
    (let* ((allowed-keys '(:type :properties :required :items :description :title :format
                           :minimum :maximum))
           (schema-keys (keys schema)))
      (dolist (key schema-keys)
        (unless (member key allowed-keys :test #'eq)
          (error "LM Studio tool translation does not yet support schema key ~S at ~a."
                 key path)))
      (let ((type (lmstudio-schema-type-keyword schema path)))
        (when (and rootp (not (eq type :object)))
          (error "LM Studio tool translation currently requires ~a.type to be :object, got ~S."
                 path type))
        (case type
          (:object
           (let ((properties (adapter-field schema "properties" :properties)))
             (when properties
               (dolist (entry (hash-table-alist properties))
                 (validate-lmstudio-tool-schema (cdr entry)
                                               (format nil "~a.properties.~a" path (car entry))))))
           (let ((required (adapter-field schema "required" :required)))
             (when required
               (dolist (name (adapter-as-list required))
                 (unless (or (stringp name) (keywordp name))
                   (error "LM Studio tool translation requires ~a.required entries to be strings or keywords, got ~S."
                          path name))))))
          (:array
           (let ((items (adapter-field schema "items" :items)))
             (unless items
               (error "LM Studio tool translation requires ~a.items to be present for array schemas."
                      path))
             (validate-lmstudio-tool-schema items (format nil "~a.items" path))))
          ((:string :number :integer :boolean)
           nil)
          (t
           (error "LM Studio tool translation does not yet support schema type ~S at ~a."
                  type path)))))))

(defun validate-lmstudio-tool-declaration (declaration)
  "Validates that DECLARATION can be exposed through the first-pass LM Studio MCP bridge."
  (let* ((name (get-name declaration))
         (behavior (adapter-field declaration "behavior" :behavior))
         (parameters (or (get-parameters-json-schema declaration)
                         (get-parameters declaration)))
         (response (or (get-response-json-schema declaration)
                       (get-response declaration))))
    (unless (and (stringp name) (> (length name) 0))
      (error "LM Studio tool translation requires every function declaration to have a non-empty name, got ~S."
             name))
    (when (and behavior (not (eq behavior :blocking)))
      (error "LM Studio tool translation only supports :blocking behavior for tool ~S, got ~S."
             name behavior))
    (when parameters
      (validate-lmstudio-tool-schema parameters (format nil "tool ~S parameters" name) :rootp t))
    (when response
      (validate-lmstudio-tool-schema response (format nil "tool ~S response" name)))))

(defun build-lmstudio-tool-bridge-integration (gemini-tools &key content-generator)
  "Converts supported Gemini tool declarations into an LM Studio ephemeral_mcp integration descriptor."
  (let ((bridge-url (ensure-lmstudio-tool-bridge-url :content-generator content-generator))
        (declarations (lmstudio-tool-declarations gemini-tools)))
    (unless declarations
      (return-from build-lmstudio-tool-bridge-integration nil))
    (dolist (decl declarations)
      (validate-lmstudio-tool-declaration decl))
    (unless (and (stringp bridge-url) (> (length bridge-url) 0))
      (error (concatenate 'string
                          "LM Studio Gemini tool translation requires GEMINI_LMSTUDIO_TOOL_BRIDGE_URL "
                          "to point at an ephemeral MCP bridge server, or an active local LM Studio tool bridge.")))
    (let ((integration (object :type "ephemeral_mcp"
                                :server_label "gemini-tools"
                                :server_url bridge-url
                                :allowed_tools (coerce (mapcar #'get-name declarations) 'vector)))
          (headers (lmstudio-tool-bridge-session-headers declarations
                                                          :content-generator content-generator)))
      (when headers
        (setf (gethash :headers integration) headers))
      integration)))

(defun build-lmstudio-payload (model-id payload session &key content-generator (streamingp t))
  "Converts a standard Gemini payload into an LM Studio /api/v1/chat payload."
  (validate-gemini-payload-shape payload)
  (let* ((contents (get-contents payload))
         (input-field (adapter-field payload "input" :input))
         (system-instruction (get-system-instruction payload))
         (generation-config (get-generation-config payload))
         (translated-tools (and (get-tools payload)
                                (translate-legacy-tools (get-tools payload))))
         (manual-integrations (adapter-as-list (adapter-field payload "integrations" :integrations)))
         (previous-response-id
          (or (adapter-field payload "previous_response_id" :previous_response_id :previous-response-id)
              (runtime-session-interaction-id session)))
         (tool-bridge-integration (and (get-tools payload)
                                       (build-lmstudio-tool-bridge-integration (get-tools payload)
                                                                               :content-generator content-generator)))
         (lmstudio-payload (object :model model-id
                                   :stream (if streamingp
                                               jsonx:+json-true+
                                               jsonx:+json-false+)
                                   :store jsonx:+json-true+)))
    (let ((normalized-input
            (cond (input-field
                   (normalize-lmstudio-input input-field))
                  ((and contents previous-response-id)
                   (or (legacy-contents-latest-turn-text contents)
                       (normalize-lmstudio-input (adapter-as-list contents))))
                  (contents
                   (normalize-lmstudio-input (adapter-as-list contents)))
                  (t
                   (error "LM Studio backend requires either CONTENTS or INPUT.")))))
      (setf (gethash :input lmstudio-payload) normalized-input))
    (when system-instruction
      (setf (gethash :system_prompt lmstudio-payload)
            (content->text system-instruction)))
    (when generation-config
      (let ((temperature (get-temperature generation-config))
            (top-p (get-top-p generation-config))
            (top-k (get-top-k generation-config))
            (max-output-tokens (get-max-output-tokens generation-config)))
        (when temperature
          (setf (gethash :temperature lmstudio-payload) temperature))
        (when top-p
          (setf (gethash :top_p lmstudio-payload) top-p))
        (when top-k
          (setf (gethash :top_k lmstudio-payload) top-k))
        (when max-output-tokens
          (setf (gethash :max_output_tokens lmstudio-payload) max-output-tokens))))
    (when translated-tools
      (setf (gethash :tools lmstudio-payload)
            (coerce translated-tools 'vector))
      (setf (gethash :tool_choice lmstudio-payload) "auto"))
    (when previous-response-id
      (setf (gethash :previous_response_id lmstudio-payload) previous-response-id))
    (let ((integrations (append manual-integrations
                                (if tool-bridge-integration
                                    (list tool-bridge-integration)
                                    nil))))
      (when integrations
        (setf (gethash :integrations lmstudio-payload)
              (coerce integrations 'vector))))
    lmstudio-payload))

(defun content-block-json-p (item)
  (and (hash-table-p item)
       (stringp (adapter-field item "type" :type))))

(defun adapter-alist-object-p (item)
  "Returns true when ITEM looks like a single adapter object encoded as an alist."
  (and (consp item)
       (every (lambda (entry)
                (and (consp entry)
                     (or (keywordp (car entry))
                         (stringp (car entry)))))
              item)))

(defun legacy-content-json-p (item)
  (and (or (hash-table-p item)
           (adapter-alist-object-p item))
       (or (adapter-field item "role" :role)
           (adapter-field item "parts" :parts))))

(defun normalize-interactions-input (input)
  (cond
    ((or (null input) (stringp input))
     input)
    ((typep input 'user-input-step)
     (mapcar #'content->json (get-content input)))
    ((typep input 'content)
     (list (content->json input)))
    ((vectorp input)
     (normalize-interactions-input (coerce input 'list)))
    ((listp input)
     (cond
       ((every (lambda (item) (typep item 'user-input-step)) input)
        (if (= (length input) 1)
            (mapcar #'content->json (get-content (first input)))
            (error "Interactions input currently supports a single user_input step; use previous_interaction_id for multi-turn state.")))
       ((every (lambda (item) (typep item 'content)) input)
        (mapcar #'content->json input))
       ((every #'legacy-content-json-p input)
        (error "Interactions input does not accept legacy role/parts content objects."))
       ((every #'content-block-json-p input)
        input)
       (t input)))
    (t input)))

(defun request-body->interaction-payload (request)
  (let ((payload (object)))
    (assert (or (slot-boundp request 'model)
                (slot-boundp request 'agent)))
    (when (slot-boundp request 'model)
      (setf (get-model payload) (get-model request)))
    (when (slot-boundp request 'input)
      (setf (get-input payload) (normalize-interactions-input (get-input request))))
    payload))

(defun %%interaction (request &key verbose)
  (google:google-post "https://generativelanguage.googleapis.com/v1beta/interactions"
                      (google:gemini-api-key)
                      (request-body->interaction-payload request)
                      :api-revision "2026-05-20"
                      :verbose verbose))

(defun testit ()
  (%%interaction
   (make-instance 'request-body
                  :model "models/gemini-3.5-flash"
                  :input (list
                          (make-instance 'user-input-step
                                         :content (list (make-instance 'text-content
                                                                       :text "Hello, how are you?")))))
   :verbose t))

(defun map-type-code-to-string (code)
  (cond ((eql code 0) "unspecified")
        ((eql code 1) "string")
        ((eql code 2) "number")
        ((eql code 3) "integer")
        ((eql code 4) "boolean")
        ((eql code 5) "array")
        ((eql code 6) "object")
        (t code)))

(defun map-type-to-string (val)
  (cond ((numberp val) (map-type-code-to-string val))
        ((or (eq val :unspecified) (equal val "unspecified")) "unspecified")
        ((or (eq val :string) (equal val "string")) "string")
        ((or (eq val :number) (equal val "number")) "number")
        ((or (eq val :integer) (equal val "integer")) "integer")
        ((or (eq val :boolean) (equal val "boolean")) "boolean")
        ((or (eq val :array) (equal val "array")) "array")
        ((or (eq val :object) (equal val "object")) "object")
        ((symbolp val) (string-downcase (symbol-name val)))
        (t val)))

(defun normalize-type-codes (thing)
  (cond ((stringp thing) thing)
        ((hash-table-p thing)
         (let ((new-table (make-hash-table :test 'equal)))
           (maphash (lambda (k v)
                      (let ((clean-key (cond ((or (eq k :type) (equal k "type")) "type")
                                             ((or (eq k :required) (equal k "required")) "required")
                                             ((symbolp k) (cl-json:lisp-to-camel-case (symbol-name k)))
                                             (t k))))
                        (cond ((and (equal clean-key "type") (not (hash-table-p v)))
                               (setf (gethash "type" new-table) (map-type-to-string v)))
                              ((equal clean-key "required")
                               (let ((required-items (adapter-as-list v)))
                                 (when required-items
                                   (setf (gethash "required" new-table)
                                         (coerce (map 'list (lambda (item)
                                                              (if (symbolp item)
                                                                  (cl-json:lisp-to-camel-case (symbol-name item))
                                                                  (princ-to-string item)))
                                                     required-items)
                                                 'vector)))))
                              (t
                               (setf (gethash clean-key new-table) (normalize-type-codes v))))))
                    thing)
           new-table))
        ((listp thing)
         (mapcar #'normalize-type-codes thing))
        ((vectorp thing)
         (map 'vector #'normalize-type-codes thing))
        (t thing)))

(defun translate-legacy-tools (tools)
  (let ((new-tools '()))
    (dolist (tool (adapter-as-list tools))
      (let ((decls (adapter-field tool "functionDeclarations" :function-declarations :functionDeclarations)))
        (if decls
            (dolist (decl (adapter-as-list decls))
              (let ((interactions-tool (make-hash-table :test 'equal)))
                (setf (gethash "type" interactions-tool) "function")
                (setf (gethash "name" interactions-tool) (adapter-field decl "name" :name))
                (let ((desc (adapter-field decl "description" :description)))
                  (when desc (setf (gethash "description" interactions-tool) desc)))
                (let ((params (adapter-field decl "parameters" :parameters)))
                  (setf (gethash "parameters" interactions-tool)
                        (if params
                            (normalize-type-codes params)
                            (let ((default-params (make-hash-table :test 'equal))
                                  (properties (make-hash-table :test 'equal)))
                              (setf (gethash "type" default-params) "object")
                              (setf (gethash "properties" default-params) properties)
                              default-params))))
                (push interactions-tool new-tools)))
            (let ((type (adapter-field tool "type" :type)))
              (if type
                  (push tool new-tools)
                  (push tool new-tools))))))
    (nreverse new-tools)))

(defun set-safe-payload-key (table string-key keyword-key value)
  (if (eq (hash-table-test table) 'equal)
      (setf (gethash string-key table) value)
      (setf (gethash keyword-key table) value)))

(defun strip-unsupported-interactions-payload-fields (payload)
  (when (hash-table-p payload)
    (dolist (key '("cachedContent" :cached-content :cachedContent
                   "generationConfig" :generation-config :generationConfig
                   "safetySettings" :safety-settings :safetySettings
                   "systemInstruction" :system-instruction :systemInstruction
                   "toolConfig" :tool-config :toolConfig
                   "toolsConfig" :tools-config :toolsConfig
                   "tools_config" :tools_config))
      (remhash key payload)))
  payload)

(defun local-resolve-model-string (model)
  (let ((model (cond ((or (null model) (and (stringp model) (string-equal model "nil")))
                      "models/gemini-3.5-flash")
                     ((typep model 'model)
                      (get-model-id model))
                     (t model))))
    (typecase model
      (null "models/gemini-3.5-flash")
      (string
       (if (and (> (length model) 7) (string= (subseq model 0 7) "models/"))
           model
           (let ((m (find-model model)))
             (if m
                 (get-model-id m)
                 (let ((m2 (find-model (concatenate 'string "models/" model))))
                   (if m2
                       (get-model-id m2)
                       (concatenate 'string "models/" model)))))))
      (symbol
       (let* ((name (string-downcase (symbol-name model)))
              (clean-name (if (and (> (length name) 7) (string= (subseq name 0 7) "gemini-"))
                              name
                              name)))
         (let ((m (find-model clean-name)))
           (if m
               (get-model-id m)
               (let ((m2 (find-model (concatenate 'string "models/" clean-name))))
                 (if m2
                     (get-model-id m2)
                     (concatenate 'string "models/" clean-name))))))))))

(defun build-interactions-input (prompt)
  prompt)
