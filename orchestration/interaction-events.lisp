;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

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
     (let ((summary (get-summary step))
           (signature (get-signature step)))
       (when (or signature
                 (and (stringp summary)
                      (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) summary)) 0)))
         (list (part (or summary "")
                     :thought t
                     :thought-signature signature)))))
    (function-call-step
     (list (part (function-call :name (or (get-function-name step) "")
                                :args (or (get-arguments step) (object))))))
    (t nil)))

(defun interaction-steps-have-result-parts-p (steps)
  (some (lambda (step)
          (interaction-step->candidate-parts step))
        (or steps '())))

(defun interaction-step-has-meaningful-result-parts-p (step)
  (typecase step
    (model-output-step
     (some (lambda (text)
             (and (stringp text)
                  (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) text)) 0)))
           (or (get-content step) '())))
    (thought-step
     (let ((summary (get-summary step)))
       (and (stringp summary)
            (> (length (string-trim '(#\Space #\Tab #\Newline #\Return) summary)) 0))))
    (function-call-step
     (or (get-function-name step)
         (get-arguments step)))
    (t nil)))

(defun interaction-steps-have-meaningful-result-parts-p (steps)
  (some #'interaction-step-has-meaningful-result-parts-p
        (or steps '())))

(defun merge-streamed-string-values (existing incoming)
  (cond ((null existing) incoming)
        ((null incoming) existing)
        ((string= existing incoming) incoming)
        ((str:starts-with? existing incoming) incoming)
        ((str:starts-with? incoming existing) existing)
        (t (concatenate 'string existing incoming))))

(defun merge-streamed-interaction-step (existing incoming)
  (cond
    ((null existing) incoming)
    ((null incoming) existing)
    ((and (typep existing 'model-output-step)
          (typep incoming 'model-output-step))
     (let* ((existing-text (let ((content (remove nil (or (get-content existing) '()))))
                             (and content (format nil "~{~A~}" content))))
            (incoming-text (let ((content (remove nil (or (get-content incoming) '()))))
                             (and content (format nil "~{~A~}" content))))
            (merged-text (merge-streamed-string-values existing-text incoming-text)))
       (make-instance 'model-output-step
                      :index (or (get-step-index incoming) (get-step-index existing))
                      :content (and merged-text (> (length merged-text) 0)
                                    (list merged-text)))))
    ((and (typep existing 'thought-step)
          (typep incoming 'thought-step))
     (let ((summary (merge-streamed-string-values (get-summary existing)
                                                  (get-summary incoming))))
       (make-instance 'thought-step
                      :index (or (get-step-index incoming) (get-step-index existing))
                      :signature (or (get-signature incoming) (get-signature existing))
                      :summary (and summary (> (length summary) 0) summary))))
    ((and (typep existing 'function-call-step)
          (typep incoming 'function-call-step))
     (let* ((existing-args (get-arguments existing))
            (incoming-args (get-arguments incoming))
            (merged-args (cond ((and (stringp existing-args) (stringp incoming-args))
                                (merge-streamed-string-values existing-args incoming-args))
                               (incoming-args incoming-args)
                               (t existing-args))))
       (make-instance 'function-call-step
                      :index (or (get-step-index incoming) (get-step-index existing))
                      :id (or (get-call-id incoming) (get-call-id existing))
                      :name (or (get-function-name incoming) (get-function-name existing))
                      :arguments merged-args)))
    (t incoming)))

(defun make-delta-derived-interaction-step (raw-event parsed-data fallback-step)
  (let* ((delta (adapter-field raw-event "delta" :delta))
         (index (or (adapter-field raw-event "index" :index)
                    (and fallback-step (get-step-index fallback-step))))
         (step-type (or (and fallback-step (get-step-type fallback-step))
                        (let ((delta-type (and delta (adapter-field delta "type" :type))))
                          (cond ((stringp delta-type)
                                 (cond ((string-equal delta-type "arguments") :function-call)
                                       ((string-equal delta-type "thought") :thought)
                                       (t :model-output)))
                                ((hash-table-p parsed-data) :function-call)
                                ((stringp parsed-data) :model-output)
                                (t :model-output))))))
    (case step-type
      (:thought
       (make-instance 'thought-step
                      :index index
                      :signature (and (typep fallback-step 'thought-step)
                                      (get-signature fallback-step))
                      :summary (and parsed-data (princ-to-string parsed-data))))
      (:function-call
       (make-instance 'function-call-step
                      :index index
                      :id (and (typep fallback-step 'function-call-step)
                               (get-call-id fallback-step))
                      :name (and (typep fallback-step 'function-call-step)
                                 (get-function-name fallback-step))
                      :arguments parsed-data))
      (t
       (make-instance 'model-output-step
                      :index index
                      :content (and parsed-data
                                    (list (princ-to-string parsed-data))))))))

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

(defun make-provider-stream-processor (session client-receiver
                                      &key event-type-reader
                                        event-type-parser
                                        event-payload-parser
                                        session-updater)
  "Constructs a provider-neutral SSE event processor.

Provider-specific adapters supply functions to read the raw event type,
parse it into an internal keyword, parse the event payload, and update any
session metadata carried by raw events."
  (lambda (event)
    (when session-updater
      (funcall session-updater session event))
    (let* ((event-type-str (and event-type-reader
                                (funcall event-type-reader event)))
           (event-type (and event-type-str
                            event-type-parser
                            (funcall event-type-parser event-type-str)))
           (parsed-data (if (and event-type event-payload-parser)
                            (funcall event-payload-parser event-type event)
                            event)))
      (when client-receiver
        (funcall client-receiver event-type parsed-data event)))))

(defun interaction-stream-event-type-string (event)
  (adapter-field event "eventType" "event_type" :event-type :event_type :event--type))

(defun parse-lmstudio-sse-event-type (event-type-str)
  "Maps LM Studio named SSE event strings to semantic keywords."
  (cond ((string-equal event-type-str "chat.start") :chat-start)
        ((string-equal event-type-str "model_load.start") :model-load-start)
        ((string-equal event-type-str "model_load.progress") :model-load-progress)
        ((string-equal event-type-str "model_load.end") :model-load-end)
        ((string-equal event-type-str "prompt_processing.start") :prompt-processing-start)
        ((string-equal event-type-str "prompt_processing.progress") :prompt-processing-progress)
        ((string-equal event-type-str "prompt_processing.end") :prompt-processing-end)
        ((string-equal event-type-str "reasoning.start") :reasoning-start)
        ((string-equal event-type-str "reasoning.delta") :reasoning-delta)
        ((string-equal event-type-str "reasoning.end") :reasoning-end)
        ((string-equal event-type-str "tool_call.start") :tool-call-start)
        ((string-equal event-type-str "tool_call.arguments") :tool-call-arguments)
        ((string-equal event-type-str "tool_call.success") :tool-call-success)
        ((string-equal event-type-str "tool_call.failure") :tool-call-failure)
        ((string-equal event-type-str "message.start") :message-start)
        ((string-equal event-type-str "message.delta") :message-delta)
        ((string-equal event-type-str "message.end") :message-end)
        ((string-equal event-type-str "chat.end") :chat-end)
        ((string-equal event-type-str "error") :error)
        (t (intern (string-upcase (substitute #\- #\. event-type-str)) "KEYWORD"))))

(defun parse-lmstudio-sse-event (event-type event)
  "Parses LM Studio SSE payloads into callback-friendly values."
  (case event-type
    ((:reasoning-delta :message-delta)
     (adapter-field event "content" :content))
    ((:tool-call-start :tool-call-arguments :tool-call-success :tool-call-failure :chat-end :error)
     event)
    (t event)))

(defun lmstudio-stream-event-type-string (event)
  (adapter-field event "type" :type))

(defun make-interaction-stream-processor (session client-receiver)
  "Constructs an SSE event handler that updates the session context dynamically
   and forwards parsed structures to the client's callback."
  (make-provider-stream-processor
   session
   client-receiver
   :event-type-reader #'interaction-stream-event-type-string
   :event-type-parser #'parse-interaction-sse-event-type
   :event-payload-parser #'parse-interaction-sse-event
   :session-updater #'update-interaction-stream-session))

(defun make-lmstudio-stream-processor (session client-receiver)
  "Constructs an LM Studio named-event SSE processor."
  (make-provider-stream-processor
   session
   client-receiver
   :event-type-reader #'lmstudio-stream-event-type-string
   :event-type-parser #'parse-lmstudio-sse-event-type
   :event-payload-parser #'parse-lmstudio-sse-event
   :session-updater #'update-lmstudio-stream-session))
