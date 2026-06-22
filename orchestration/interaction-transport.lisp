;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defvar *current-sse-socket* nil
  "Thread-local dynamically bound active stateful SSE socket.")

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
                   (if (and (or (null *current-sse-socket*)
                                (not (sse-socket-abort-requested-p *current-sse-socket*)))
                            (interactions-malformed-tool-call-error-p e)
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
                   (if (and (or (null *current-sse-socket*)
                                (not (sse-socket-abort-requested-p *current-sse-socket*)))
                            (interactions-malformed-tool-call-error-p e)
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

(defun safely-cleanup-sse-resource (description thunk)
  "Runs THUNK during SSE cleanup and logs any teardown failure without masking it silently."
  (handler-case
      (funcall thunk)
    (error (e)
      (log-warn "SSE cleanup failed while ~A: ~A" description e))))

(defclass stateful-sse-socket ()
  ((state :initform :unconnected
          :accessor sse-socket-state
          :type (member :unconnected :connecting :streaming :draining :closed :aborted))
   (state-lock :initform (sb-thread:make-mutex :name "sse-state-lock")
               :reader sse-socket-state-lock)
   (waitqueue :initform (sb-thread:make-waitqueue)
              :reader sse-socket-waitqueue)
   (stream :initform nil
           :accessor sse-socket-stream
           :documentation "The raw HTTP/SSE body-stream returned by Dexador.")
   (network-thread :initform nil
                   :accessor sse-socket-network-thread
                   :documentation "The thread performing the active block reads.")
   (monitor-thread :initform nil
                   :accessor sse-socket-monitor-thread
                   :documentation "The thread watching for hangs, timeouts, and state updates.")
   (last-activity-time :initform (get-universal-time)
                       :accessor sse-socket-last-activity-time
                       :documentation "Timestamp of the last successfully parsed byte or SSE chunk.")
   (read-timeout :initarg :read-timeout
                 :initform 300
                 :reader sse-socket-read-timeout)
   (receiver :initarg :receiver
             :reader sse-socket-receiver
             :documentation "Callback function for parsed SSE events.")
   (abort-requested-p :initform nil
                      :accessor sse-socket-abort-requested-p
                      :documentation "Flag indicating that the client thread requested an abort.")
   (cleanup-hook :initform nil
                 :accessor sse-socket-cleanup-hook
                 :documentation "Closure to run on final termination to release resources.")))

(defun close-sse-socket-resources-safely (socket)
  "Executes physical resource teardown. Idempotent and thread-safe."
  (let ((stream (sse-socket-stream socket)))
    (when (and stream (open-stream-p stream))
      (safely-cleanup-sse-resource
       "closing the SSE body stream"
       (lambda () (close stream)))
      (setf (sse-socket-stream socket) nil)))
  (let ((hook (sse-socket-cleanup-hook socket)))
    (when hook
      (setf (sse-socket-cleanup-hook socket) nil)
      (safely-cleanup-sse-resource
       "running the SSE cleanup hook"
       hook)))
  (let ((net-thread (sse-socket-network-thread socket)))
    (when (and net-thread
               (sb-thread:thread-alive-p net-thread)
               (not (eq sb-thread:*current-thread* net-thread)))
      (loop repeat 5
            while (sb-thread:thread-alive-p net-thread)
            do (sleep 0.01))
      (when (sb-thread:thread-alive-p net-thread)
        (safely-cleanup-sse-resource
         "terminating the SSE network thread"
         (lambda () (sb-thread:terminate-thread net-thread))))
      (setf (sse-socket-network-thread socket) nil))))

(defun transition-sse-state (socket new-state)
  "Thread-safely transitions the socket state, checking valid lifecycles."
  (let ((trigger-cleanup nil))
    (sb-thread:with-mutex ((sse-socket-state-lock socket))
      (let ((old-state (sse-socket-state socket)))
        (unless (or (eq old-state new-state)
                    (and (member old-state '(:draining :closed :aborted))
                         (member new-state '(:unconnected :connecting :streaming)))
                    (and (member old-state '(:aborted :draining))
                         (eq new-state :closed)))
          (log-debug "SSE Socket State Transition: ~A -> ~A" old-state new-state)
          (setf (sse-socket-state socket) new-state)
          (when (member new-state '(:draining :closed :aborted))
            (setf trigger-cleanup t)))))
    (when trigger-cleanup
      (close-sse-socket-resources-safely socket))))

(defun start-sse-monitor-thread (socket)
  "Launches the guardian thread to oversee connection safety."
  (setf (sse-socket-monitor-thread socket)
        (sb-thread:make-thread
         (lambda ()
           (unwind-protect
                (loop
                  (let ((state nil)
                        (now (get-universal-time)))
                    (sb-thread:with-mutex ((sse-socket-state-lock socket))
                      (setf state (sse-socket-state socket)))
                    (when (and (eq state :streaming)
                               (> (- now (sse-socket-last-activity-time socket))
                                  (sse-socket-read-timeout socket)))
                      (log-error "SSE Socket read-timeout exceeded. Socket is frozen.")
                      (transition-sse-state socket :aborted)
                      (return))
                    (when (sse-socket-abort-requested-p socket)
                      (transition-sse-state socket :draining)
                      (return))
                    (when (member state '(:closed :aborted))
                      (return))
                    (sb-thread:with-mutex ((sse-socket-state-lock socket))
                      (sb-thread:condition-wait (sse-socket-waitqueue socket)
                                                (sse-socket-state-lock socket)
                                                :timeout 0.5))))
             (let ((current-state nil))
               (sb-thread:with-mutex ((sse-socket-state-lock socket))
                 (setf current-state (sse-socket-state socket)))
               (unless (member current-state '(:closed :aborted))
                 (transition-sse-state socket :closed)))))
         :name "SSE Socket Monitor")))

(defun signal-sse-abort (socket)
  "Instantly signals an abort request to the monitor thread."
  (sb-thread:with-mutex ((sse-socket-state-lock socket))
    (setf (sse-socket-abort-requested-p socket) t))
  (sb-thread:condition-notify (sse-socket-waitqueue socket)))

(defun google-interactions-post-streaming (uri api-key payload receiver &key verbose read-timeout connect-timeout)
  "Perform an HTTP POST of a JSON object to the Google API, handling streaming responses.
   Includes Api-Revision support specifically for the stateful Interactions API.
   Bridges legacy tests that mock google:google-post by automatically simulating SSE events."
  (let ((current-google-post (fdefinition 'google:google-post)))
    (if (not (eq current-google-post *original-google-post*))
        (let ((response (funcall current-google-post
                                 "https://generativelanguage.googleapis.com/v1beta/interactions"
                                 api-key
                                 payload)))
          (funcall receiver (object :event-type "interaction.created"
                                    :interaction-id (adapter-field response "id" :id)))
          (let ((steps (adapter-field response "steps" :steps)))
            (dolist (step (adapter-as-list steps))
              (funcall receiver (object :event-type "step.start" :step step))
              (funcall receiver (object :event-type "step.stop" :step step))))
          (funcall receiver (object :event-type "interaction.completed"
                                    :interaction response)))
        (let ((socket (make-instance 'stateful-sse-socket
                                     :read-timeout (or read-timeout 300)
                                     :receiver receiver))
              (net-thread-error nil))
          (setf *current-sse-socket* socket)
          (transition-sse-state socket :connecting)
          (setf (sse-socket-network-thread socket)
                (sb-thread:make-thread
                 (lambda ()
                   (let ((*current-sse-socket* socket))
                     (handler-case
                         (multiple-value-bind (body-stream status headers)
                             (funcall google:*dex-post* uri
                                      :headers `(("Accept" . "text/event-stream")
                                                 ("Content-Type" . "application/json")
                                                 ("Api-Revision" . "2026-05-20")
                                                 ("x-goog-api-key" . ,api-key))
                                      :verbose verbose
                                      :content (cl-json:encode-json-to-string payload)
                                      :want-stream t
                                      :read-timeout (or read-timeout google:+default-read-timeout+)
                                      :connect-timeout (or connect-timeout google:+default-connect-timeout+))
                           (declare (ignore status))
                           (setf (sse-socket-stream socket) body-stream)
                           (transition-sse-state socket :streaming)
                           (setf (sse-socket-last-activity-time socket) (get-universal-time))
                           (let ((content-type (google::get-header-value headers "content-type")))
                             (if (and content-type (str:starts-with? "text/event-stream" content-type))
                                 (google::process-sse-stream
                                  body-stream
                                  (lambda (event)
                                    (setf (sse-socket-last-activity-time socket) (get-universal-time))
                                    (unless (member (sse-socket-state socket) '(:draining :closed :aborted))
                                      (funcall receiver event))))
                                 (do ((json :begin (handler-case (cl-json:decode-json body-stream)
                                                     (end-of-file () nil)))
                                      (result nil (unless (eq json :begin)
                                                    (setf (sse-socket-last-activity-time socket) (get-universal-time))
                                                    (funcall receiver json))))
                                     ((null json) result)))))
                       (error (e)
                         (unless (sse-socket-abort-requested-p socket)
                           (setf net-thread-error e)
                           (error e))))))
                 :name "SSE Network Thread"))
          (start-sse-monitor-thread socket)
          (unwind-protect
               (progn
                 (sb-thread:join-thread (sse-socket-network-thread socket))
                 (when net-thread-error
                   (error net-thread-error)))
            (transition-sse-state socket :closed))))))

(defun lmstudio-post-streaming (uri payload receiver &key verbose read-timeout connect-timeout)
  "Perform an LM Studio /api/v1/chat request in SSE streaming mode."
  (declare (ignore verbose))
  (let ((socket (make-instance 'stateful-sse-socket
                               :read-timeout (or read-timeout 300)
                               :receiver receiver))
        (net-thread-error nil))
    (setf *current-sse-socket* socket)
    (transition-sse-state socket :connecting)
    (setf (sse-socket-network-thread socket)
          (sb-thread:make-thread
           (lambda ()
             (let ((*current-sse-socket* socket))
               (handler-case
                   (multiple-value-bind (body-stream status headers uri* stream)
                       (dex:post uri
                                 :headers (openai-request-headers)
                                 :content (cl-json:encode-json-to-string payload)
                                 :want-stream t
                                 :read-timeout (or read-timeout 300)
                                 :connect-timeout (or connect-timeout 60))
                     (declare (ignore status uri* stream))
                     (setf (sse-socket-stream socket) body-stream)
                     (transition-sse-state socket :streaming)
                     (setf (sse-socket-last-activity-time socket) (get-universal-time))
                     (let ((content-type (google::get-header-value headers "content-type")))
                       (if (and content-type (str:starts-with? "text/event-stream" content-type))
                           (google::process-sse-stream
                            body-stream
                            (lambda (event)
                              (setf (sse-socket-last-activity-time socket) (get-universal-time))
                              (unless (member (sse-socket-state socket) '(:draining :closed :aborted))
                                (funcall receiver event))))
                           (error "LM Studio streaming endpoint did not return text/event-stream. Content-Type: ~A"
                                  content-type))))
                 (error (e)
                   (unless (sse-socket-abort-requested-p socket)
                     (setf net-thread-error e)
                     (error e))))))
           :name "LM Studio SSE Network Thread"))
    (start-sse-monitor-thread socket)
    (unwind-protect
         (progn
           (sb-thread:join-thread (sse-socket-network-thread socket))
           (when net-thread-error
             (error net-thread-error)))
      (transition-sse-state socket :closed))))

(defun lmstudio-post (uri payload &key verbose read-timeout connect-timeout)
  "Perform a non-streaming LM Studio /api/v1/chat request."
  (declare (ignore verbose))
  (let ((response
          (dex:post uri
                    :headers (openai-request-headers)
                    :content (cl-json:encode-json-to-string payload)
                    :read-timeout (or read-timeout 300)
                    :connect-timeout (or connect-timeout 60))))
    (if (stringp response)
        (cl-json:decode-json-from-string response)
        response)))

(defmethod invoke-backend ((backend lmstudio-backend) model-id payload &key (read-timeout 300) (connect-timeout 60) receiver content-generator &allow-other-keys)
  "Invokes the LM Studio /api/v1/chat streaming backend."
  (let* ((session (ensure-runtime-session))
         (actual-model-id (if (typep model-id 'model)
                              (get-model-id model-id)
                              model-id))
         (endpoint (or (get-backend-url backend)
                       "http://localhost:1234/api/v1/chat")))
    (if receiver
        (let ((*current-sse-socket* nil))
          (handler-bind ((sb-sys:interactive-interrupt
                          (lambda (c)
                            (declare (ignore c))
                            (when *current-sse-socket*
                              (signal-sse-abort *current-sse-socket*)
                              (transition-sse-state *current-sse-socket* :draining))
                            (return-from invoke-backend (values nil nil)))))
            (let* ((lmstudio-payload (build-lmstudio-payload actual-model-id payload session
                                                             :content-generator content-generator
                                                             :streamingp t))
                   (final-result nil)
                   (stream-error nil))
              (flet ((internal-receiver (event-type parsed-data raw-event)
                       (when receiver
                         (funcall receiver event-type parsed-data raw-event))
                       (when (eq event-type :error)
                         (setf stream-error raw-event))
                       (when (eq event-type :chat-end)
                         (setf final-result (adapter-field raw-event "result" :result))
                         (when *current-sse-socket*
                           (signal-sse-abort *current-sse-socket*)))))
                (let ((stream-proc (make-lmstudio-stream-processor session #'internal-receiver)))
                  (report-elapsed-time (format nil "LM Studio request for `~a`" actual-model-id)
                    (lmstudio-post-streaming endpoint
                                             lmstudio-payload
                                             stream-proc
                                             :read-timeout read-timeout
                                             :connect-timeout connect-timeout)))
                (if (and stream-error
                         (or (null final-result)
                             (null (adapter-as-list (adapter-field final-result "output" :output)))))
                    (error "LM Studio stream reported an error: ~A"
                           (lmstudio-error-message stream-error))
                    (values nil nil))))))
        (let ((lmstudio-payload (build-lmstudio-payload actual-model-id payload session
                                                        :content-generator content-generator
                                                        :streamingp nil)))
          (report-elapsed-time (format nil "LM Studio request for `~a`" actual-model-id)
            (multiple-value-bind (response usage-metadata)
                (lmstudio-result->gemini-response
                 (lmstudio-post endpoint
                                lmstudio-payload
                                :read-timeout read-timeout
                                :connect-timeout connect-timeout))
              (let ((response-id (and response (get-response-id response))))
                (when response-id
                  (setf (runtime-session-interaction-id session) response-id)))
              (when usage-metadata
                (process-usage-metadata usage-metadata))
              (values response usage-metadata)))))))

(defmethod invoke-backend ((backend interactions-backend) model-id payload &key (read-timeout 500) (connect-timeout 60) receiver &allow-other-keys)
  "Invokes the stateful Interactions API. It reads previous_interaction_id
   from the current session, performs a streaming POST call under the hood,
   updates the session ID, and returns parsed step-based objects (accumulated
   internally if no receiver callback is provided)."
  (let ((*current-sse-socket* nil))
    (handler-bind ((sb-sys:interactive-interrupt
                    (lambda (c)
                      (declare (ignore c))
                      (when *current-sse-socket*
                        (signal-sse-abort *current-sse-socket*)
                        (transition-sse-state *current-sse-socket* :draining))
                      (return-from invoke-backend (values nil nil)))))
      (let* ((session (ensure-runtime-session))
             (prev-id (runtime-session-interaction-id session))
             (contents (adapter-field payload "contents" :contents)))
        (when contents
          (let* ((prompt (legacy-contents-latest-turn-text contents))
                 (new-payload (make-hash-table :test 'equal)))
            (setf (gethash "model" new-payload) (local-resolve-model-string model-id))
            (setf (gethash "input" new-payload) (build-interactions-input prompt))
            (let ((bg (adapter-field payload "background" :background)))
              (when bg (setf (gethash "background" new-payload) bg)))
            (let ((tools (adapter-field payload "tools" :tools :tools-config :tools_config)))
              (when tools (setf (gethash "tools" new-payload) tools)))
            (setf payload new-payload)))
        (let ((tools (adapter-field payload "tools" :tools :tools-config :tools_config)))
          (if (and tools (hash-table-p payload))
              (let ((translated (translate-legacy-tools tools)))
                (set-safe-payload-key payload "tools" :tools translated))
              (when (hash-table-p payload)
                (remhash "tools" payload)
                (remhash :tools payload)
                (remhash :tools-config payload)
                (remhash :tools_config payload))))
        (let ((input (adapter-field payload "input" :input)))
          (when (and input (hash-table-p payload))
            (set-safe-payload-key payload "input" :input (normalize-interactions-input input))))
        (strip-unsupported-interactions-payload-fields payload)
        (when (and prev-id (not (adapter-field payload "previous_interaction_id" :previous_interaction_id :previous-interaction-id)))
          (cond ((hash-table-p payload)
                 (set-safe-payload-key payload "previous_interaction_id" :previous--interaction--id prev-id))
                ((consp payload)
                 (push (cons "previous_interaction_id" prev-id) payload))))
        (log-debug "Serialized Interactions Payload: ~A" (cl-json:encode-json-to-string payload))
        (set-safe-payload-key payload "stream" :stream t)
        (let ((final-steps nil)
              (final-interaction nil)
              (streamed-step-table (make-hash-table :test 'equal))
              (streamed-step-order '())
              (current-stream-step-key nil))
          (labels ((step-storage-key (step)
                     (or (get-step-index step)
                         current-stream-step-key
                         (gensym "STREAM-STEP-")))
                   (store-stream-step (step)
                     (when step
                       (let* ((key (step-storage-key step))
                              (existing (gethash key streamed-step-table)))
                         (unless (member key streamed-step-order :test #'equal)
                           (setf streamed-step-order (append streamed-step-order (list key))))
                         (setf (gethash key streamed-step-table)
                               (merge-streamed-interaction-step existing step))
                         (setf current-stream-step-key key))))
                   (streamed-steps ()
                     (mapcar (lambda (key)
                               (gethash key streamed-step-table))
                             streamed-step-order))
                   (effective-final-steps ()
                     (let ((captured-streamed-steps (streamed-steps)))
                       (if (interaction-steps-have-meaningful-result-parts-p final-steps)
                           final-steps
                           captured-streamed-steps))))
            (flet ((internal-receiver (event-type parsed-data raw-event)
                     (when receiver
                       (funcall receiver event-type parsed-data raw-event))
                     (case event-type
                       (:step-start
                        (store-stream-step parsed-data))
                       (:step-delta
                        (store-stream-step
                         (make-delta-derived-interaction-step
                          raw-event
                          parsed-data
                          (and current-stream-step-key
                               (gethash current-stream-step-key streamed-step-table)))))
                       (:step-stop
                        (store-stream-step parsed-data))
                       (otherwise nil))
                     (when (eq event-type :interaction-completed)
                       (setf final-steps (effective-final-steps))
                       (setf final-interaction (adapter-field raw-event "interaction" :interaction)))))
              (let ((stream-proc (make-interaction-stream-processor session #'internal-receiver)))
                (report-elapsed-time (format nil "Interactions API request for `~a`" model-id)
                  (post-interactions-streaming-with-retry
                   payload
                   stream-proc
                   read-timeout
                   connect-timeout)))
              (unless receiver
                (if final-interaction
                    (interaction-steps->gemini-response final-steps final-interaction)
                    (let ((error-message
                            (cond ((and *current-sse-socket* (eq (sse-socket-state *current-sse-socket*) :aborted))
                                   (format nil "Interactions stream aborted due to read timeout (no activity for ~A seconds)."
                                           (sse-socket-read-timeout *current-sse-socket*)))
                                  ((and *current-sse-socket* (sse-socket-abort-requested-p *current-sse-socket*))
                                   "Interactions stream was aborted by the user.")
                                  (t
                                   "Interactions stream closed without receiving interaction.completed event."))))
                      (cerror "Fake an interaction.completed event and return steps accumulated so far."
                              error-message)
                      (let ((fake-interaction (object :id (or (and *current-sse-socket*
                                                                  (runtime-session-interaction-id session))
                                                             "fake_interaction_id")
                                                     :steps (coerce (effective-final-steps) 'vector))))
                        (interaction-steps->gemini-response (effective-final-steps) fake-interaction))))))))))))
