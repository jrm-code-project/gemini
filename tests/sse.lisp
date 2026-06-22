;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite sse-tests)

(test test-sse-socket-lifecycle-normal
  "Test that the stateful-sse-socket transitions states cleanly and cleans up resources normally."
  (let* ((mock-stream (make-string-input-stream "mock data"))
         (socket (make-instance 'gemini:stateful-sse-socket :read-timeout 10)))
    (setf (gemini::sse-socket-stream socket) mock-stream)
    (is (eq :unconnected (gemini:sse-socket-state socket)))
    
    (gemini:transition-sse-state socket :connecting)
    (is (eq :connecting (gemini:sse-socket-state socket)))
    
    (gemini:transition-sse-state socket :streaming)
    (is (eq :streaming (gemini:sse-socket-state socket)))
    
    (gemini:transition-sse-state socket :closed)
    (is (eq :closed (gemini:sse-socket-state socket)))
    
    ;; Stream should be closed
    (is (not (open-stream-p mock-stream)))))

(test test-sse-socket-abort-and-drain
  "Test that signaling abort transitions to :draining instantly and wakes the monitor thread."
  (let* ((mock-stream (make-string-input-stream "mock data"))
         (socket (make-instance 'gemini:stateful-sse-socket :read-timeout 10))
         (cleanup-called nil))
    (setf (gemini::sse-socket-stream socket) mock-stream)
    (setf (gemini::sse-socket-cleanup-hook socket) (lambda () (setf cleanup-called t)))
    (gemini:transition-sse-state socket :streaming)
    
    ;; Start the monitor thread
    (gemini:start-sse-monitor-thread socket)
    (is (and (gemini::sse-socket-monitor-thread socket)
             (sb-thread:thread-alive-p (gemini::sse-socket-monitor-thread socket))))
    
    ;; Signal abort
    (gemini:signal-sse-abort socket)
    
    ;; Wait a tiny moment for the monitor thread to notice and exit
    (loop repeat 100
          while (sb-thread:thread-alive-p (gemini::sse-socket-monitor-thread socket))
          do (sleep 0.01))
    
    ;; Monitor thread should have exited
    (is (not (sb-thread:thread-alive-p (gemini::sse-socket-monitor-thread socket))))
    ;; State should have transitioned to :draining (which is preserved by guards)
    (is (eq :draining (gemini:sse-socket-state socket)))
    ;; Resources should be cleaned up
    (is (not (open-stream-p mock-stream)))
    (is (eq t cleanup-called))))

(test test-sse-socket-timeout-handling
  "Test that the monitor thread automatically aborts a frozen connection after read-timeout."
  (let* ((mock-stream (make-string-input-stream "mock data"))
         ;; Very short timeout of 1 second for fast testing
         (socket (make-instance 'gemini:stateful-sse-socket :read-timeout 1)))
    (setf (gemini::sse-socket-stream socket) mock-stream)
    (gemini:transition-sse-state socket :streaming)
    ;; Set last activity to 2 seconds ago to simulate timeout instantly
    (setf (gemini::sse-socket-last-activity-time socket) (- (get-universal-time) 2))
    
    (gemini:start-sse-monitor-thread socket)
    
    ;; Monitor thread should see the timeout and transition to aborted
    (loop repeat 100
          while (sb-thread:thread-alive-p (gemini::sse-socket-monitor-thread socket))
          do (sleep 0.01))
    
    (is (eq :aborted (gemini:sse-socket-state socket)))
    (is (not (open-stream-p mock-stream)))))

(test test-sse-socket-done-marker-handling
  "Test that our cl-json:decode-json-from-string wrapper intercepts [DONE] and returns nil cleanly instead of throwing a syntax error."
  (is (null (cl-json:decode-json-from-string "[DONE]")))
  (is (null (cl-json:decode-json-from-string "  [DONE]  ")))
  ;; Standard JSON should still decode perfectly
  (is (equal "bar" (cdr (assoc :foo (cl-json:decode-json-from-string "{\"foo\": \"bar\"}"))))))

(test test-google-interactions-post-streaming-requests-sse
  "Verify the transport requests an SSE response and opens the Dexador stream in streaming mode."
  (let ((orig-dex-post google:*dex-post*)
        (captured-uri nil)
        (captured-headers nil)
        (captured-content nil)
        (captured-want-stream nil)
        (events '()))
    (unwind-protect
         (progn
           (setf google:*dex-post*
                 (lambda (uri &key headers content want-stream verbose read-timeout connect-timeout &allow-other-keys)
                   (declare (ignore verbose read-timeout connect-timeout))
                   (setf captured-uri uri
                         captured-headers headers
                         captured-content content
                         captured-want-stream want-stream)
                   (values
                    (make-string-input-stream
                     (format nil
                             "data: {\"eventType\":\"interaction.completed\",\"interaction\":{\"id\":\"int_hdr\",\"steps\":[{\"type\":\"model_output\",\"index\":0,\"content\":{\"parts\":[{\"text\":\"ok\"}]}}]}}~%~%"))
                    200
                    (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))
           (gemini::google-interactions-post-streaming
            "https://generativelanguage.googleapis.com/v1beta/interactions?alt=sse"
            "test-key"
            (alexandria:plist-hash-table '("stream" t "model" "models/gemini-3.5-flash") :test 'equal)
            (lambda (event)
              (push event events)))
           (is (equal "https://generativelanguage.googleapis.com/v1beta/interactions?alt=sse" captured-uri))
           (is (eq t captured-want-stream))
           (is (equal "text/event-stream" (cdr (assoc "Accept" captured-headers :test #'equal))))
           (is (equal "application/json" (cdr (assoc "Content-Type" captured-headers :test #'equal))))
           (is (equal "2026-05-20" (cdr (assoc "Api-Revision" captured-headers :test #'equal))))
           (is (equal "test-key" (cdr (assoc "x-goog-api-key" captured-headers :test #'equal))))
           (is (search "\"stream\":" captured-content))
           (is (= 1 (length events))))
      (setf google:*dex-post* orig-dex-post))))

(test test-interactions-backend-openai-style-sse-is-not-google-event-stream
  "Characterize the current failure mode when OpenAI/LM Studio-style SSE is fed through the Interactions event processor."
  (let ((session (gemini:make-runtime-session))
        (orig-dex-post google:*dex-post*)
        (backend (make-instance 'gemini:interactions-backend))
        (dummy-payload (make-hash-table :test 'equal)))
    (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf google:*dex-post*
                   (lambda (uri &rest args)
                     (declare (ignore uri args))
                     (values
                      (make-string-input-stream
                       (format nil
                               "data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"Hello\"},\"finish_reason\":null}]}~%~%
data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\" world\"},\"finish_reason\":null}]}~%~%
data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}~%~%
data: [DONE]~%~%"))
                      200
                      (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))

             (handler-case
                 (progn
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                   (fail "Expected interactions backend to reject non-Google SSE event envelopes."))
               (error (e)
                 (is (search "without receiving interaction.completed event"
                             (princ-to-string e)))))
             (is (null (gemini:runtime-session-interaction-id session)))
             (is (null (gemini:runtime-session-environment-id session))))
        (setf google:*dex-post* orig-dex-post)))))

(test test-sse-socket-descriptive-error-handling
  "Test that invoke-backend raises highly descriptive errors for aborted or timed-out sockets."
  (let ((session (gemini:make-runtime-session))
        (orig-dex-post google:*dex-post*)
        (backend (make-instance 'gemini:interactions-backend))
        (dummy-payload (make-hash-table :test 'equal)))
    (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             ;; 1. Normal closure without completed event (should raise standard error)
             (setf google:*dex-post*
                   (lambda (uri &rest args)
                     (declare (ignore uri args))
                     ;; Return mock body-stream, status, and headers
                     (values (make-string-input-stream "")
                             200
                             (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))
             
             (signals error
               (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))
             
             ;; 2. Simulated Aborted socket state (should raise read timeout error)
             (setf google:*dex-post*
                   (lambda (uri &rest args)
                     (declare (ignore uri args))
                     (when gemini::*current-sse-socket*
                       (gemini:transition-sse-state gemini::*current-sse-socket* :aborted))
                     (values (make-string-input-stream "")
                             200
                             (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))
             
             (handler-case
                 (progn
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                   (fail "Expected error not raised."))
               (error (e)
                 (is (search "read timeout" (princ-to-string e)))))
             
             ;; 3. Simulated Continuable error (should return fabricated response on continue)
             (setf google:*dex-post*
                   (lambda (uri &rest args)
                     (declare (ignore uri args))
                     (values (make-string-input-stream
                              (format nil
                                      "data: {\"eventType\":\"step.start\",\"step\":{\"type\":\"model_output\",\"index\":0}}~%~%
data: {\"eventType\":\"step.delta\",\"index\":0,\"delta\":{\"text\":\"STREAM_OK\"}}~%~%
data: {\"eventType\":\"step.stop\",\"step\":{\"type\":\"model_output\",\"index\":0}}~%~%"))
                             200
                             (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))
             
             (let ((result nil))
               (let ((*error-output* (make-string-output-stream)))
                 (handler-bind ((error (lambda (e)
                                         (declare (ignore e))
                                         (let ((restart (find-restart 'continue)))
                                           (when restart
                                             (invoke-restart restart))))))
                   (setf result (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))))
               (is (not (null result)))
               (is (equal "fake_interaction_id" (gemini::get-response-id result)))))
        (setf google:*dex-post* orig-dex-post)))))
