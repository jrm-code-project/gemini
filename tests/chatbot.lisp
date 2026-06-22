;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite gemini-chatbot-tests)

(test explicit-session-chat-helpers-are-exported
  "Verify explicit-session chat helpers are part of the public GEMINI package API."
  (multiple-value-bind (new-chat-symbol new-chat-status)
      (find-symbol "NEW-CHAT-WITH-SESSION" "GEMINI")
    (is (eq :external new-chat-status))
    (is (fboundp new-chat-symbol)))
  (multiple-value-bind (chat-symbol chat-status)
      (find-symbol "CHAT-WITH-SESSION" "GEMINI")
    (is (eq :external chat-status))
    (is (fboundp chat-symbol)))
  (multiple-value-bind (continue-symbol continue-status)
      (find-symbol "CONTINUE-GEMINI-WITH-SESSION" "GEMINI")
    (is (eq :external continue-status))
    (is (fboundp continue-symbol))))

(test chatbot-conversation-isolation
  "Test that separate chatbot instances maintain independent conversation state."
  (let ((orig-generate-content #'gemini::generate-content))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                   (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                   (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
           (let* ((bot-a (gemini::chatbot gemini::*gemini-flash*))
                  (bot-b (gemini::chatbot gemini::*gemini-flash*))
                  (agent-a (funcall bot-a :agent!))
                  (agent-b (funcall bot-b :agent!)))
             (funcall bot-a "alpha")
             (funcall bot-b "beta")
             (is (not (eq agent-a agent-b)))
             (is (search "alpha"
                         (gemini::content->text
                          (car (last (gemini::conversational-agent-conversation agent-a) 2)))))
             (is (search "beta"
                         (gemini::content->text
                          (car (last (gemini::conversational-agent-conversation agent-b) 2)))))
             (is (not (search "beta"
                              (gemini::content->text
                               (car (last (gemini::conversational-agent-conversation agent-a) 2))))))
             (is (not (search "alpha"
                              (gemini::content->text
                               (car (last (gemini::conversational-agent-conversation agent-b) 2))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test continue-gemini-with-session-isolation
  "Test that explicit runtime sessions isolate continue-gemini context state."
  (let ((orig-generate-content #'gemini::generate-content))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                   (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                   (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
           (let ((session-a (gemini:make-runtime-session))
                 (session-b (gemini:make-runtime-session)))
             (let ((res-a (gemini::continue-gemini-with-session session-a "alpha"))
                   (res-b (gemini::continue-gemini-with-session session-b "beta")))
               (declare (ignore res-a res-b))
               (let ((context-a (gemini::runtime-session-context session-a))
                 (context-b (gemini::runtime-session-context session-b)))
                 (is (gemini::list-of-content? context-a))
                 (is (gemini::list-of-content? context-b))
                 (is (search "alpha" (gemini::content->text (car (last context-a)))))
                 (is (search "beta" (gemini::content->text (car (last context-b)))))
                 (is (not (search "beta" (gemini::content->text (car (last context-a))))))
                 (is (not (search "alpha" (gemini::content->text (car (last context-b))))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test continue-gemini-with-session-works-isolated
  "Test that continue-gemini-with-session updates the supplied session."
  (let ((orig-generate-content #'gemini::generate-content))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::generate-content)
                (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                  (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                  (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
          (let* ((session (gemini:make-runtime-session))
                 (result (gemini::continue-gemini-with-session session "hello")))
            (declare (ignore result))
            (is (gemini::list-of-content? (gemini::runtime-session-context session)))
            (is (search "reply: hello"
                        (gemini::content->text
                         (car (last (gemini::runtime-session-context session))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test continue-gemini-wrapper-updates-default-repl-session
  "Test that the continue-gemini wrapper updates the *default-repl-session*."
  (let ((orig-generate-content #'gemini::generate-content)
        (old-default-session gemini::*default-repl-session*))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::generate-content)
                (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                  (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                  (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
          (setf gemini::*default-repl-session* (gemini:make-runtime-session))
          (is (null (gemini::continue-gemini "hello")))
          (is (gemini::list-of-content? (gemini::runtime-session-context gemini::*default-repl-session*)))
          (is (search "reply: hello"
                      (gemini::content->text
                       (car (last (gemini::runtime-session-context gemini::*default-repl-session*)))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content
            gemini::*default-repl-session* old-default-session))))

(test runtime-session-topic-isolation
  "Test that current-topic reads/writes the active runtime session state."
  (let ((session-a (gemini:make-runtime-session :conversation-topic "topic-a"))
        (session-b (gemini:make-runtime-session :conversation-topic "topic-b")))
    (gemini:with-runtime-session (session-a)
      (is (equal "topic-a" (gemini:current-topic)))
      (setf (gemini:current-topic) "topic-a-updated")
      (is (equal "topic-a-updated" (gemini:current-topic))))
    (gemini:with-runtime-session (session-b)
      (is (equal "topic-b" (gemini:current-topic))))
    (is (equal "topic-a-updated" (gemini::runtime-session-conversation-topic session-a)))
    (is (equal "topic-b" (gemini::runtime-session-conversation-topic session-b)))))

(test repl-eval-print-form-keeps-session-context-complete
  "Test that REPL eval history is recorded entirely on the runtime session rather than split across legacy globals."
  (let ((session (gemini:make-runtime-session)))
    (let ((*standard-output* (make-string-output-stream)))
      (gemini:with-runtime-session (session)
        (gemini::repl-eval-print-form '(+ 1 2)))
      (let ((context (gemini::runtime-session-context session)))
        (is (= 4 (length context)))
        (is (equal context (gemini::runtime-session-prior-context session)))
        (is (equal "model" (gemini:get-role (first context))))
        (is (equal "function" (gemini:get-role (second context))))
        (is (equal "model" (gemini:get-role (third context))))
        (is (equal "user" (gemini:get-role (fourth context))))))))

(test chat-with-session-default-persona-fallback
  "Test that chat-with-session works without explicit persona/session initialization."
  (let ((orig-generate-content #'gemini::generate-content))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                   (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                   (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
           (let* ((session (gemini:make-runtime-session))
                  (result (gemini::chat-with-session session "hello")))
             (declare (ignore result))
             (is (functionp (gemini::runtime-session-chat-persona session)))
             (is (gemini::list-of-content? (gemini::runtime-session-context session)))
             (is (search "reply: hello"
                         (gemini::content->text
                          (car (last (gemini::runtime-session-context session))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test chat-with-session-binds-default-repl-session-during-invocation
  "Test that explicit-session chat installs the supplied session while the persona invokes generation."
  (let ((orig-generate-content #'gemini::generate-content)
       (seen-session nil))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::generate-content)
                (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                  (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                  (setf seen-session gemini::*default-repl-session*)
                  (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
          (let ((session (gemini:make-runtime-session)))
            (gemini::chat-with-session session "hello")
            (is (eq session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test chat-with-session-updates-supplied-session-completely
  "Test that explicit-session chat updates the supplied session completely."
  (let ((orig-generate-content #'gemini::generate-content))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::generate-content)
                (lambda (content-generator context mood prompt parts files system-instruction &key read-timeout connect-timeout)
                  (declare (ignore content-generator context mood parts files system-instruction read-timeout connect-timeout))
                  (gemini::content :role "model" :parts (list (part (format nil "reply: ~a" prompt))))))
          (let* ((session (gemini:make-runtime-session))
                 (result (gemini::chat-with-session session "hello")))
            (declare (ignore result))
            (is (gemini::list-of-content? (gemini::runtime-session-context session)))
            (is (search "reply: hello"
                        (gemini::content->text
                         (car (last (gemini::runtime-session-context session))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test new-chat-with-session-registers-persona-on-session
  "Test that explicit-session chat initialization keeps persona state on the session."
  (let ((orig-reload-persona #'gemini::reload-persona)
       (session (gemini:make-runtime-session)))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::reload-persona)
                (lambda (persona-name prompt)
                  (declare (ignore persona-name prompt))
                  (lambda (input &rest keys)
                    (declare (ignore input keys))
                    nil)))
          (gemini::new-chat-with-session session "Mock" "hello")
          (is (functionp (gemini::runtime-session-chat-persona session))))
      (setf (fdefinition 'gemini::reload-persona) orig-reload-persona))))

(test new-chat-works-flawlessly
  "Test that new-chat works flawlessly."
  (let ((orig-reload-persona #'gemini::reload-persona)
        (old-default-session gemini::*default-repl-session*))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::reload-persona)
                 (lambda (persona-name prompt)
                   (declare (ignore persona-name prompt))
                   (lambda (input &rest keys)
                     (declare (ignore input keys))
                     nil)))
           (setf gemini::*default-repl-session* (gemini:make-runtime-session))
           (is (null (gemini:new-chat "Mock" "hello")))
           (is (typep gemini::*default-repl-session* 'gemini:runtime-session))
           (is (null (gemini::runtime-session-model gemini::*default-repl-session*)))
           (is (functionp (gemini::runtime-session-chat-persona gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::reload-persona) orig-reload-persona
            gemini::*default-repl-session* old-default-session))))

(test new-chat-with-session-clears-prior-interaction-id
  "Test that starting a new chat discards any previous provider response linkage."
  (let ((orig-reload-persona #'gemini::reload-persona))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::reload-persona)
                 (lambda (persona-name prompt)
                   (declare (ignore persona-name prompt))
                   (lambda (input &rest keys)
                     (declare (ignore input keys))
                     nil)))
           (let ((session (gemini:make-runtime-session :interaction-id "resp_prev")))
             (gemini::new-chat-with-session session "Mock" "hello")
             (is (null (gemini::runtime-session-interaction-id session)))))
      (setf (fdefinition 'gemini::reload-persona) orig-reload-persona))))

(test persona-goals-files-read-and-write
  "Test that both reading and writing persona goal-tracking files work as expected on a mock persona without side effects on production goals."
  (let ((config (gemini::load-persona-config "MockGoalsBot")))
    (unwind-protect
         (progn
           ;; 1. Write custom contents
           (gemini:write-persona-long-term-goals config "Long-Term goals content")
           (gemini:write-persona-short-term-goals config "Short-Term goals content")
           (gemini:write-persona-immediate-goals config "Immediate goals content")

           ;; 2. Read and verify
           (is (equal "Long-Term goals content" (gemini:read-persona-long-term-goals config)))
           (is (equal "Short-Term goals content" (gemini:read-persona-short-term-goals config)))
           (is (equal "Immediate goals content" (gemini:read-persona-immediate-goals config))))
      ;; 3. Cleanup the mock persona directory
      (let ((dir (gemini::persona-directory "MockGoalsBot")))
        (when (probe-file dir)
          (uiop:delete-directory-tree dir :validate t))))))

(test persona-goals-llm-tools-registration
  "Test that the persona goals tools are correctly declared and compiled as function declarations for the LLM."
  (let* ((config (gemini::load-persona-config "Default"))
         (tools (gemini:persona-goals-tools config))
         (write-lt (cdr (assoc "writeLongTermGoals" tools :key #'gemini:get-name :test #'equal)))
         (read-lt (cdr (assoc "readLongTermGoals" tools :key #'gemini:get-name :test #'equal))))
    (is (not (null write-lt)))
    (is (not (null read-lt)))
    ;; Test execution of the handlers
    (let ((res (funcall read-lt)))
      (is (stringp res))
      (is (search "Long-Term Goals" res)))))
