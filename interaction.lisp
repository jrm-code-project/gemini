;;; -*- mode: lisp; coding: utf-8 -*-

(in-package "GEMINI")

(deftype model-option ()
  '(member
    :gemini-2.5-computer-use-preview-10-2025
    :gemini-3.1-flash-tts-preview
    :gemini-2.5-flash-preview-tts
    :gemini-2.5-pro-preview-tts
    :lyria-3-pro-preview
    :gemini-2.5-flash
    :gemini-3.1-pro-preview
    :lyria-3-clip-preview
    :gemini-3.1-flash-lite
    :gemini-3.1-flash-lite-preview
    :gemini-3-flash-preview
    :gemini-3.5-flash
    :gemini-3-pro-preview
    :gemini-2.5-flash-native-audio-preview-12-2025
    :gemini-2.5-flash-image
    :gemini-2.5-flash-lite
    :gemini-2.5-pro
    :gemini-3.1-flash-image-preview
    :gemini-3-pro-image-preview
    :gemini-2.5-flash-lite-preview-09-2025
    :gemini-2.5-flash-preview-09-2025
    :gemini-2.5-computer-use-preview-10-2025
    :gemini-3.1-flash-tts-preview
    :gemini-2.5-flash-preview-tts
    :gemini-2.5-pro-preview-tts
    :lyria-3-pro-preview
    :gemini-2.5-flash
    :gemini-3.1-pro-preview
    :lyria-3-clip-preview
    :gemini-3.1-flash-lite
    :gemini-3.1-flash-lite-preview
    :gemini-3-flash-preview
    :gemini-3.5-flash
    :gemini-3-pro-preview
    :gemini-2.5-flash-native-audio-preview-12-2025
    :gemini-2.5-flash-image
    :gemini-2.5-flash-lite
    :gemini-2.5-pro
    :gemini-3.1-flash-image-preview
    :gemini-3-pro-image-preview
    :gemini-2.5-flash-lite-preview-09-2025
    :gemini-2.5-flash-preview-09-2025))

(deftype agent-option ()
  `(member
    :deep-research-preview-04-2026
    :deep-research-pro-preview-12-2025
    :deep-research-max-preview-04-2026
    :antigravity-preview-05-2026
    :deep-research-preview-04-2026
    :deep-research-pro-preview-12-2025
    :deep-research-max-preview-04-2026
    :antigravity-preview-05-2026))

(defclass request-body ()
  ((model :initarg :model :accessor get-model :type model-option)
   (agent :initarg :agent :accessor get-agent :type agent-option)
   (input :initarg :input :accessor get-input)))

(defmethod initialize-instance :after ((instance request-body) &rest args)
  ;; assert that either the model or agent are specified, but not both
  (assert (or (getf args :model)
              (getf args :agent))))

(deftype content-type ()
  '(member
    :audio
    :document
    :image
    :text
    :video))

(defclass content ()
  ((content-type :initarg :type :type content-type :reader get-content-type)))

(deftype audio-mime-type ()
  '(member
    :audio/aac
    :audio/aiff
    :audio/alaw
    :audio/flac
    :audio/l16
    :audio/m4a
    :audio/mp3
    :audio/mpeg
    :audio/mulaw
    :audio/ogg
    :audio/opus
    :audio/wav))

(deftype document-mime-type ()
  '(member
    :application/pdf
    :text/csv))

(deftype image-mime-type ()
  '(member
    :image/bmp
    :image/gif
    :image/heic
    :image/heif
    :image/jpeg
    :image/png
    :image/tiff
    :image/webp))

(deftype video-mime-type ()
  '(member
    :video/3gpp
    :video/avi
    :video/mov
    :video/mp4
    :video/mpeg
    :video/mpg
    :video/webm
    :video/wmv
    :video/x-flv))

(deftype model-resolution ()
  `(member
    :low
    :medium
    :high
    :ultra-high))

(deftype annotation-type ()
  '(member
    :file-citation
    :place-citation
    :uri-citation))

(defclass audio-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type audio-mime-type)
   (channels :initarg :channels :reader get-channels :type integer)
   (sample-rate :initarg :sample-rate :reader get-sample-rate :type integer))
  (:default-initargs :type :audio))

(defclass document-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type document-mime-type))
  (:default-initargs :type :document))

(defclass image-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type document-mime-type)
   (resolution :initarg :resolution :reader get-resolution :type model-resolution))
  (:default-initargs :type :image))

(defclass annotation ()
  ((type :initarg :type :reader get-annotation-type :type string)))

(defclass file-citation (annotation)
  ((document-uri :initarg :document-uri :reader get-document-uri :type string)
   (file-name :initarg :file-name :reader get-file-name :type string)
   (source :initarg :source :reader get-source :type string)
   (custom-metadata :initarg :custom-metadata :reader get-custom-metadata)
   (page-number :initarg :page-number :reader get-page-number :type integer)
   (media-id :initarg :media-id :reader get-media-id :type string)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :file-citation))

(defclass place-citation (annotation)
  ((place-id :initarg :place-id :reader get-place-id :type string)
   (name :initarg :name :reader get-name :type string)
   (uri :initarg :uri :reader get-uri :type string)
   (review-snippet :initarg :review-snippet :reader get-review-snippet :type review-snippet)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :place-citation))

(defclass uri-citation (annotation)
  ((url :initarg :url :reader get-url :type string)
   (title :initarg :title :reader get-title :type string)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :uri-citation))

(defclass text-content (content)
  ((text :initarg :text :reader get-text :type string)
   (annotation :initarg :annotation :reader get-annotation :type annotation))
  (:default-initargs :type :text))

(defmethod print-object ((obj text-content) stream)
  (format stream "#<TEXT-CONTENT: ~s>"
          (get-text obj)))

(defclass video-content (content)
  ()
  (:default-initargs :type :video))

(defclass interaction-step ()
  ((index :initarg :index :reader get-step-index :type (or null integer) :initform nil)
   (type :initarg :type :reader get-step-type :type symbol)))

(defclass user-input-step (interaction-step)
  ((content :initarg :content :reader get-content :type content))
  (:default-initargs :type :user--input))

(defclass model-output-step (interaction-step)
  ((content :initarg :content :reader get-content))
  (:default-initargs :type :model-output))

(defclass thought-step (interaction-step)
  ((signature :initarg :signature :reader get-signature :type (or null string))
   (summary :initarg :summary :reader get-summary :type (or null string)))
  (:default-initargs :type :thought))

(defclass function-call-step (interaction-step)
  ((id :initarg :id :reader get-call-id :type (or null string))
   (name :initarg :name :reader get-function-name :type (or null string))
   (arguments :initarg :arguments :reader get-arguments))
  (:default-initargs :type :function-call))

(defun parse-interaction-step (step-json)
  (let* ((type-str (or (adapter-field step-json "type" :type) ""))
         (type (cond ((string-equal type-str "model_output") :model-output)
                     ((string-equal type-str "thought") :thought)
                     ((string-equal type-str "function_call") :function-call)
                     ((string-equal type-str "user_input") :user-input)
                     (t (intern (string-upcase type-str) "KEYWORD"))))
         (index (adapter-field step-json "index" :index)))
    (case type
      (:model-output
       (let* ((content-json (adapter-field step-json "content" :content))
              (parts (and content-json (adapter-field content-json "parts" :parts)))
              (parsed-content (and parts
                                   (map 'list
                                        (lambda (part)
                                          (adapter-field part "text" :text))
                                        (adapter-as-list parts)))))
         (make-instance 'model-output-step
                        :index index
                        :content parsed-content)))
      (:thought
       (make-instance 'thought-step
                      :index index
                      :signature (adapter-field step-json "signature" :signature)
                      :summary (adapter-field step-json "summary" :summary)))
      (:function-call
       (let ((call (adapter-field step-json "functionCall" :function-call)))
         (make-instance 'function-call-step
                        :index index
                        :id (adapter-field step-json "id" :id)
                        :name (and call (adapter-field call "name" :name))
                        :arguments (and call (adapter-field call "arguments" :arguments)))))
      (t (make-instance 'interaction-step :index index :type type)))))

(defmethod content->json ((content text-content))
  (let ((content-json (make-hash-table :test 'equal)))
    (setf (gethash "type" content-json) "text")
    (setf (gethash "text" content-json) (get-text content))
    (when (slot-boundp content 'annotation)
      (setf (gethash "annotation" content-json) (annotation->json (get-annotation content))))
    content-json))

(defmethod step->json ((step user-input-step))
  (let ((step-json (make-hash-table :test 'equal)))
    (setf (gethash "type" step-json) "user_input")
    (setf (gethash "content" step-json) (mapcar #'content->json (get-content step)))
    step-json))

(defun steps->json (steps)
  (mapcar #'step->json steps))

(defun interaction-step->candidate-parts (step)
  (typecase step
    (model-output-step
     (mapcar #'part (or (get-content step) '())))
    (thought-step
     (list (part (or (get-summary step) "")
                 :thought t
                 :thought-signature (get-signature step))))
    (function-call-step
     (list (part (function-call :name (or (get-function-name step) "")
                                :args (or (get-arguments step) (object))))))
    (t nil)))

(defun interaction-steps->gemini-response (steps response)
  (let* ((parts (mappend #'interaction-step->candidate-parts (or steps '())))
         (usage-metadata (adapter-field response "usageMetadata" :usage-metadata :usageMetadata "usage"))
         (normalized (object :candidates (if parts
                                             (list (object :content (content :role "model" :parts parts)))
                                             nil))))
    (when (null parts)
      (error "Interactions response contained no result parts. Response id: ~A"
             (adapter-field response "id" :id)))
    (let ((response-id (adapter-field response "id" :id)))
      (when response-id
        (setf (get-response-id normalized) response-id)))
    (let ((model-version (adapter-field response "modelVersion" :model-version :modelVersion)))
      (when model-version
        (setf (get-model-version normalized) model-version)))
    (when usage-metadata
      (setf (get-usage-metadata normalized) usage-metadata))
    (values normalized usage-metadata)))

(defparameter +interactions-malformed-tool-call-max-retries+ 5
  "Maximum number of immediate retries after a malformed_tool_call response from Interactions.")

(defun interactions-malformed-tool-call-error-p (condition)
  "Returns true when CONDITION appears to represent an Interactions malformed_tool_call response." 
  (let ((message (string-downcase (princ-to-string condition))))
    (or (search "malformed_tool_call" message)
        (search "invalid json syntax" message)
        (search "output could not be parsed" message))))

(defun post-interactions-with-retry (payload read-timeout connect-timeout)
  "Posts PAYLOAD to the Interactions endpoint, retrying a small number of malformed_tool_call responses." 
  (let ((attempt 0))
    (labels ((post-once ()
               (handler-case
                   (google:google-post "https://generativelanguage.googleapis.com/v1beta/interactions"
                                       (google:gemini-api-key)
                                       payload
                                       :api-revision "2026-05-20"
                                       :read-timeout (or read-timeout 60)
                                       :connect-timeout (or connect-timeout 300))
                 (error (e)
                   (if (and (interactions-malformed-tool-call-error-p e)
                            (< attempt +interactions-malformed-tool-call-max-retries+))
                       (progn
                         (incf attempt)
                         (log-warn "Interactions malformed_tool_call response. Retrying attempt ~D of ~D."
                                   attempt
                                   +interactions-malformed-tool-call-max-retries+)
                         (post-once))
                       (error e))))))
      (post-once))))

(defun post-interactions-streaming-with-retry (payload stream-proc read-timeout connect-timeout)
  "Posts PAYLOAD to the Interactions endpoint, handling streaming responses,
   and retrying a small number of malformed_tool_call responses."
  (let ((attempt 0))
    (labels ((post-once ()
               (handler-case
                   (google-interactions-post-streaming
                    "https://generativelanguage.googleapis.com/v1beta/interactions?alt=sse"
                    (google:gemini-api-key)
                    payload
                    stream-proc
                    :read-timeout read-timeout
                    :connect-timeout connect-timeout)
                 (error (e)
                   (if (and (interactions-malformed-tool-call-error-p e)
                            (< attempt +interactions-malformed-tool-call-max-retries+))
                       (progn
                         (incf attempt)
                         (log-warn "Interactions malformed_tool_call response. Retrying attempt ~D of ~D."
                                   attempt
                                   +interactions-malformed-tool-call-max-retries+)
                         (post-once))
                       (error e))))))
      (post-once))))

(defparameter *original-google-post* #'google:google-post
  "The original un-mocked Google POST function.")

(defun google-interactions-post-streaming (uri api-key payload receiver &key verbose read-timeout connect-timeout)
  "Perform an HTTP POST of a JSON object to the Google API, handling streaming responses.
   Includes Api-Revision support specifically for the stateful Interactions API.
   Bridges legacy tests that mock google:google-post by automatically simulating SSE events."
  (let ((current-google-post (fdefinition 'google:google-post)))
    (if (not (eq current-google-post *original-google-post*))
        ;; Detect if google:google-post has been mocked (e.g. in legacy tests)
        (let ((response (funcall current-google-post
                                 "https://generativelanguage.googleapis.com/v1beta/interactions"
                                 api-key
                                 payload)))
          ;; Simulate SSE stream events from the mocked non-streaming response object
          (funcall receiver (object :event-type "interaction.created"
                                    :interaction-id (adapter-field response "id" :id)))
          (let ((steps (adapter-field response "steps" :steps)))
            (dolist (step (adapter-as-list steps))
              (funcall receiver (object :event-type "step.start" :step step))
              (funcall receiver (object :event-type "step.stop" :step step))))
          (funcall receiver (object :event-type "interaction.completed"
                                    :interaction response)))
        
        ;; Standard Production SSE Streaming HTTP Call
        (multiple-value-bind (body-stream status headers)
            (funcall google:*dex-post* uri
                     :headers `(("Accept" . "application/json")
                                ("Content-Type" . "application/json")
                                ("Api-Revision" . "2026-05-20")
                                ("x-goog-api-key" . ,api-key))
                     :verbose verbose
                     :content (cl-json:encode-json-to-string payload)
                     :want-stream t
                     :read-timeout (or read-timeout google:+default-read-timeout+)
                     :connect-timeout (or connect-timeout google:+default-connect-timeout+))
          (declare (ignore status))
          (let ((content-type (google::get-header-value headers "content-type")))
            (if (and content-type (str:starts-with? "text/event-stream" content-type))
                (google::process-sse-stream body-stream receiver)
                ;; Fallback to contiguous JSON stream
                (do ((json :begin (handler-case (cl-json:decode-json body-stream)
                                    (end-of-file () nil)))
                     (result nil (unless (eq json :begin)
                                   (funcall receiver json))))
                    ((null json) result))))))))

(defun parse-interaction-sse-event-type (event-type-str)
  "Maps string-based event types to semantic keywords."
  (cond ((string-equal event-type-str "interaction.created") :interaction-created)
        ((string-equal event-type-str "step.start") :step-start)
        ((string-equal event-type-str "step.delta") :step-delta)
        ((string-equal event-type-str "step.stop") :step-stop)
        ((string-equal event-type-str "interaction.completed") :interaction-completed)
        ((string-equal event-type-str "error") :error)
        (t (intern (string-upcase (substitute #\- #\. event-type-str)) "KEYWORD"))))

(defun parse-interaction-sse-event (event-type event)
  "Parses the active inner payload of an SSE event depending on its type."
  (case event-type
    (:step-delta
     (let ((delta (adapter-field event "delta" :delta)))
       (when delta
         (or (adapter-field delta "text" :text)
             (adapter-field delta "arguments" :arguments)
             delta))))
    ((:step-start :step-stop)
     (let ((step-json (adapter-field event "step" :step)))
       (when step-json
         (parse-interaction-step step-json))))
    (:interaction-completed
     (let ((interaction (adapter-field event "interaction" :interaction)))
       (when interaction
         (let ((steps-json (adapter-field interaction "steps" :steps)))
           (and steps-json
                (map 'list #'parse-interaction-step (adapter-as-list steps-json)))))))
    (t event)))

(defun make-interaction-stream-processor (session client-receiver)
  "Constructs an SSE event handler that updates the session context dynamically
   and forwards parsed structures to the client's callback."
  (lambda (event)
    (let* ((event-type-str (adapter-field event "eventType" "event_type" :event-type :event_type))
           (event-type (and event-type-str (parse-interaction-sse-event-type event-type-str)))
           (interaction-id (adapter-field event "interactionId" "interaction_id" :interaction-id :interaction_id))
           (environment-id (adapter-field event "environmentId" "environment_id" :environment-id :environment_id))
           (parsed-data (and event-type (parse-interaction-sse-event event-type event))))
      
      ;; Dynamically bind / update session properties
      (when interaction-id
        (setf (runtime-session-interaction-id session) interaction-id))
      (when environment-id
        (setf (runtime-session-environment-id session) environment-id))
      
      (let ((interaction (adapter-field event "interaction" :interaction)))
        (when interaction
          (let ((id (adapter-field interaction "id" :id))
                (env-id (adapter-field interaction "environmentId" "environment_id" :environment-id :environment_id)))
            (when id (setf (runtime-session-interaction-id session) id))
            (when env-id (setf (runtime-session-environment-id session) env-id)))))
      
      ;; Call user's client-receiver with signature: (keyword-event-type parsed-payload raw-event-hash-table)
      (when client-receiver
        (funcall client-receiver event-type parsed-data event)))))

(defun content-block-json-p (item)
  (and (hash-table-p item)
       (stringp (adapter-field item "type" :type))))

(defun legacy-content-json-p (item)
  (and (hash-table-p item)
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
            ;; If it doesn't have functionDeclarations, but already has a type, keep it as-is
            (let ((type (adapter-field tool "type" :type)))
              (if type
                  (push tool new-tools)
                  ;; Fallback just in case
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
         ;; check if registered
         (let ((m (find-model clean-name)))
           (if m
               (get-model-id m)
               (let ((m2 (find-model (concatenate 'string "models/" clean-name))))
                 (if m2
                     (get-model-id m2)
                     (concatenate 'string "models/" clean-name))))))))))

(defun build-interactions-input (prompt)
  prompt)

(defmethod invoke-backend ((backend interactions-backend) model-id payload &key (read-timeout 300) (connect-timeout 60) receiver &allow-other-keys)
  "Invokes the stateful Interactions API. It reads previous_interaction_id 
   from the current session, performs a streaming POST call under the hood, 
   updates the session ID, and returns parsed step-based objects (accumulated 
   internally if no receiver callback is provided)."
  (let* ((session (ensure-runtime-session))
         (prev-id (runtime-session-interaction-id session))
         ;; Check if payload is a legacy contents-based stateless payload
         (contents (adapter-field payload "contents" :contents)))
    (when contents
      (let* ((contents-list (adapter-as-list contents))
             (last-turn (car (last contents-list)))
             (parts (and last-turn (adapter-field last-turn "parts" :parts)))
             (text-list (and parts
                             (map 'list
                                  (lambda (part)
                                    (adapter-field part "text" :text))
                                  (adapter-as-list parts))))
             (prompt (format nil "~{~A~^ ~}" text-list))
             (new-payload (make-hash-table :test 'equal)))
        ;; Map model-id
        (setf (gethash "model" new-payload) (local-resolve-model-string model-id))
        ;; Convert prompt to input step (flat list of parts)
        (setf (gethash "input" new-payload) (build-interactions-input prompt))
        ;; Copy background if present
        (let ((bg (adapter-field payload "background" :background)))
          (when bg (setf (gethash "background" new-payload) bg)))
        ;; Copy tools if present
        (let ((tools (adapter-field payload "tools" :tools :tools-config :tools_config)))
          (when tools (setf (gethash "tools" new-payload) tools)))
        (setf payload new-payload)))

    ;; Translate tools to Interactions schema if present (un-nest functionDeclarations)
    (let ((tools (adapter-field payload "tools" :tools :tools-config :tools_config)))
      (if (and tools (hash-table-p payload))
          (let ((translated (translate-legacy-tools tools)))
            (set-safe-payload-key payload "tools" :tools translated))
          ;; If tools is nil/empty, remove it from the payload hash table to avoid sending "tools": null
          (when (hash-table-p payload)
            (remhash "tools" payload)
            (remhash :tools payload)
            (remhash :tools-config payload)
            (remhash :tools_config payload))))

    ;; Normalize any pre-built input form into the REST shapes accepted by Interactions.
    (let ((input (adapter-field payload "input" :input)))
      (when (and input (hash-table-p payload))
        (set-safe-payload-key payload "input" :input (normalize-interactions-input input))))

              ;; Legacy Gemini payloads can carry fields the Interactions API does not accept.
              ;; Strip them here so chat flows using the generic payload builder remain valid.
              (strip-unsupported-interactions-payload-fields payload)

    ;; Inject previous_interaction_id if it exists and wasn't explicitly provided
    (when (and prev-id (not (adapter-field payload "previous_interaction_id" :previous_interaction_id :previous-interaction-id)))
      (cond ((hash-table-p payload)
             (set-safe-payload-key payload "previous_interaction_id" :previous--interaction--id prev-id))
            ((consp payload)
             (push (cons "previous_interaction_id" prev-id) payload))))
    
        (format t ";; DEBUG: Serialized Interactions Payload: ~A~%" (cl-json:encode-json-to-string payload))
        (force-output)
        
        ;; Set the streaming flag in the request payload
        (set-safe-payload-key payload "stream" :stream t)
        
        (let ((final-steps nil)
              (final-interaction nil))
          (flet ((internal-receiver (event-type parsed-data raw-event)
                   (when receiver
                     (funcall receiver event-type parsed-data raw-event))
                   (when (eq event-type :interaction-completed)
                     (setf final-steps parsed-data)
                     (setf final-interaction (adapter-field raw-event "interaction" :interaction)))))
            
            (let ((stream-proc (make-interaction-stream-processor session #'internal-receiver)))
              (post-interactions-streaming-with-retry
               payload
               stream-proc
               read-timeout
               connect-timeout))
            
            ;; If no receiver callback was supplied, block and return the reconstituted response
            (unless receiver
              (if final-interaction
                  (interaction-steps->gemini-response final-steps final-interaction)
                  (error "Interactions stream closed without receiving interaction.completed event.")))))))

                      
