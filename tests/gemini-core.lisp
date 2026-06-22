;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite gemini-core-tests)

(test invoke-gemini-with-session-binds-explicit-session
  "Verify that the explicit-session entry point installs the supplied runtime session during generation."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "ok")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (gemini:get-parts (gemini:invoke-gemini-with-session explicit-session "hello")))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test invoke-gemini-wrapper-uses-ambient-session
  "Verify that the legacy wrapper still routes through the active ambient runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "ok")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (gemini:get-parts (gemini:invoke-gemini "hello")))
             (is (eq ambient-session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test prompt-predicate-with-session-binds-explicit-session
  "Verify that prompt-predicate's explicit-session variant binds the supplied runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "T")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (eq t (gemini::prompt-predicate-with-session explicit-session "hello")))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test prompt-predicate-wrapper-uses-ambient-session
  "Verify that prompt-predicate still uses the ambient runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "T")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (eq t (gemini::prompt-predicate "hello")))
             (is (eq ambient-session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test flash-compress-with-session-binds-explicit-session
  "Verify that flash-compress's explicit-session variant binds the supplied runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "compressed")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "compressed" (gemini::flash-compress-with-session explicit-session "hello")))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test flash-compress-wrapper-uses-ambient-session
  "Verify that flash-compress still uses the ambient runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "compressed")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "compressed" (gemini::flash-compress "hello")))
             (is (eq ambient-session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test improve-system-instruction-with-session-binds-explicit-session
  "Verify that improve-system-instruction's explicit-session variant binds the supplied runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "improved")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "improved"
                        (gemini:content->text
                         (gemini:improve-system-instruction-with-session explicit-session "hello"))))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test improve-system-instruction-wrapper-uses-ambient-session
  "Verify that improve-system-instruction still uses the ambient runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "improved")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "improved" (gemini:content->text (gemini:improve-system-instruction "hello"))))
             (is (eq ambient-session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test analyze-lisp-with-session-binds-explicit-session
  "Verify that analyze-lisp's explicit-session variant binds the supplied runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "analysis")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "analysis"
                        (gemini:content->text
                         (gemini:analyze-lisp-with-session explicit-session "(defun x () 1)"))))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test analyze-lisp-wrapper-uses-ambient-session
  "Verify that analyze-lisp still uses the ambient runtime session."
  (let ((orig-generate-content #'gemini::generate-content)
        (ambient-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-session gemini::*default-repl-session*)
                   (gemini:content :role "model" :parts (list (part "analysis")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "analysis" (gemini:content->text (gemini:analyze-lisp "(defun x () 1)"))))
             (is (eq ambient-session seen-session))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test analyze-file-with-session-uses-explicit-session
  "Verify that analyze-file's explicit-session variant routes each turn through the supplied runtime session."
  (let ((orig-file-forms #'gemini::file-forms)
        (orig-continue #'gemini::continue-gemini-with-session)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-sessions '()))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::file-forms)
                 (lambda (filename)
                   (declare (ignore filename))
                   '("(defun a () 1)" "(defun b () 2)")))
           (setf (fdefinition 'gemini::continue-gemini-with-session)
                 (lambda (session prompt)
                   (declare (ignore prompt))
                   (push session seen-sessions)
                   "analysis"))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal '(("(defun a () 1)" . "analysis")
                          ("(defun b () 2)" . "analysis"))
                        (gemini::analyze-file-with-session explicit-session "dummy.lisp" :verbose nil)))
             (is (= 2 (length seen-sessions)))
             (is (every (lambda (session) (eq session explicit-session)) seen-sessions))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::file-forms) orig-file-forms
            (fdefinition 'gemini::continue-gemini-with-session) orig-continue))))

(test analyze-system-definition-with-session-binds-explicit-session
  "Verify that analyze-system-definition's explicit-session variant passes the supplied runtime session through to generation."
  (let ((orig-find-system #'asdf:find-system)
        (orig-system-source-file #'asdf:system-source-file)
        (orig-read-file-string #'uiop:read-file-string)
        (orig-invoke #'gemini:invoke-gemini-with-session)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'asdf:find-system)
                 (lambda (system-name &rest args)
                   (declare (ignore system-name args))
                   :fake-system))
           (setf (fdefinition 'asdf:system-source-file)
                 (lambda (system)
                   (declare (ignore system))
                   #P"D:\\repositories\\gemini\\README.md"))
           (setf (fdefinition 'uiop:read-file-string)
                 (lambda (pathname &rest args)
                   (declare (ignore pathname args))
                   "system contents"))
           (setf (fdefinition 'gemini:invoke-gemini-with-session)
                 (lambda (session prompt &key context mood parts files system-instruction model tools tool-config read-timeout connect-timeout)
                   (declare (ignore prompt context mood parts files system-instruction model tools tool-config read-timeout connect-timeout))
                   (setf seen-session session)
                   (gemini:content :role "model" :parts (list (part "analysis")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "analysis"
                        (gemini:content->text
                         (gemini::analyze-system-definition-with-session explicit-session "gemini"))))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'asdf:find-system) orig-find-system
            (fdefinition 'asdf:system-source-file) orig-system-source-file
            (fdefinition 'uiop:read-file-string) orig-read-file-string
            (fdefinition 'gemini:invoke-gemini-with-session) orig-invoke))))

(test analyze-component-with-session-binds-explicit-session
  "Verify that analyze-component's explicit-session variant passes the supplied runtime session through to generation."
  (let ((orig-get-contents #'gemini::get-system-and-component-contents)
        (orig-invoke #'gemini:invoke-gemini-with-session)
        (ambient-session (gemini:make-runtime-session))
        (explicit-session (gemini:make-runtime-session))
        (seen-session nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::get-system-and-component-contents)
                 (lambda (system-name component-name)
                   (declare (ignore system-name component-name))
                   (values :fake-system "system contents" :fake-component "component contents")))
           (setf (fdefinition 'gemini:invoke-gemini-with-session)
                 (lambda (session prompt &key context mood parts files system-instruction model tools tool-config read-timeout connect-timeout)
                   (declare (ignore prompt context mood parts files system-instruction model tools tool-config read-timeout connect-timeout))
                   (setf seen-session session)
                   (gemini:content :role "model" :parts (list (part "component analysis")))))
           (let ((gemini::*default-repl-session* ambient-session))
             (is (equal "component analysis"
                        (gemini:content->text
                         (gemini::analyze-component-with-session explicit-session "gemini" "gemini.lisp"))))
             (is (eq explicit-session seen-session))
             (is (eq ambient-session gemini::*default-repl-session*))))
      (setf (fdefinition 'gemini::get-system-and-component-contents) orig-get-contents
            (fdefinition 'gemini:invoke-gemini-with-session) orig-invoke))))

(test entropy-and-redaction
  "Test Shannon entropy calculation and token/string redaction."
  ;; 1. Entropy calculation
  (is (= 0.0 (gemini::calculate-string-entropy "")))
  (is (= 0.0 (gemini::calculate-string-entropy "aaaa")))
  ;; A string with high entropy (fully unique characters) should be higher
  (is (> (gemini::calculate-string-entropy "abcdefghijklmnopqrstuvwxyz") 3.5))
  ;; Verify that string entropy yields identical values under optimized vector-loop
  (is (< (abs (- 2.0 (gemini::calculate-string-entropy "abcd"))) 0.001))
  
  ;; 2. Redact token
  (is (equal "[REDACTED]" (gemini::redact-token "xYz9pQrSlKjGwV")))
  (is (equal "short" (gemini::redact-token "short")))
  
  ;; 3. Redact full string
  (is (equal "hello [REDACTED] world" (gemini::redact "hello xYz9pQrSlKjGwV world"))))

(test generation-recursion-hard-limit
  "Ensure %generate-content aborts deterministically at the configured hard recursion limit."
  (is (= 32 gemini::+max-generation-recursion-depth+))
  (signals gemini::generation-recursion-limit-exceeded
    (gemini::%generate-content nil nil nil "noop" nil nil nil nil 32)))

(test generation-recursion-hard-limit-continuable
  "Ensure continuing the hard-limit condition extends the active recursion limit by 32."
  (let ((gemini::*generation-recursion-hard-limit* gemini::+max-generation-recursion-depth+))
    (handler-bind ((gemini::generation-recursion-limit-exceeded
                     (lambda (c)
                       (declare (ignore c))
                       (let ((restart (find-restart 'continue)))
                         (when restart
                           (invoke-restart restart))))))
      (gemini::ensure-generation-recursion-budget! 32))
    (is (= (+ gemini::+max-generation-recursion-depth+
              gemini::+generation-recursion-depth-extension+)
           gemini::*generation-recursion-hard-limit*))))

(test openai-timeout-propagation
  "Verify that %invoke-gemini successfully propagates read and connect timeouts to %%invoke-openai."
  (let* ((called-read-timeout nil)
         (called-connect-timeout nil)
         ;; Mock %%invoke-openai
         (mock-invoke-openai (lambda (model-id payload &key read-timeout connect-timeout &allow-other-keys)
                               (declare (ignore model-id payload))
                               (setq called-read-timeout read-timeout)
                               (setq called-connect-timeout connect-timeout)
                               (values "mock-response" nil)))
         (generator gemini::*gemini-uncensored*))
    (let ((orig-invoke-openai #'gemini::%%invoke-openai)
          (orig-openai-response->gemini-response #'gemini::openai-response->gemini-response))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-openai) mock-invoke-openai)
             (setf (fdefinition 'gemini::openai-response->gemini-response) (lambda (x) (declare (ignore x)) (values "mock-response" nil)))
             ;; Invoke with custom timeouts
             (gemini::%invoke-gemini generator "test-model" (gemini::object) :read-timeout 123 :connect-timeout 45)
             ;; Check that they matched!
             (is (= 123 called-read-timeout))
             (is (= 45 called-connect-timeout)))
        ;; Restore
        (setf (fdefinition 'gemini::%%invoke-openai) orig-invoke-openai)
        (setf (fdefinition 'gemini::openai-response->gemini-response) orig-openai-response->gemini-response)))))

(test gemini-rate-limit-backoff-grows-exponentially
  "Test that 429 backoff penalty grows exponentially and updates backoff-until."
  (let ((gemini::+gemini-backoff-base-seconds+ 2)
        (gemini::+gemini-backoff-max-seconds+ 8)
        (gemini::+gemini-backoff-jitter-max-seconds+ 0)
        (gemini::*gemini-backoff-jitter-function* (lambda (max-jitter-seconds)
                                                    (declare (ignore max-jitter-seconds))
                                                    0)))
    (gemini::reset-gemini-rate-limit-backoff!)
    (gemini::register-gemini-rate-limit-hit!)
    (is (= 2 gemini::*gemini-rate-limit-penalty-seconds*))
    (let ((first-until gemini::*gemini-rate-limit-backoff-until*))
      (gemini::register-gemini-rate-limit-hit!)
      (is (= 4 gemini::*gemini-rate-limit-penalty-seconds*))
      (is (>= gemini::*gemini-rate-limit-backoff-until* first-until)))))

(test invoke-gemini-retries-on-429
  "Test that %%invoke-gemini retries when a 429 error is encountered."
  (let ((orig-google-post #'google:google-post)
        (orig-rate-limit #'gemini::gemini-rate-limit)
        (calls 0)
        (gemini::+gemini-rate-limit-max-retries+ 3)
        (gemini::+gemini-backoff-base-seconds+ 0)
        (gemini::+gemini-backoff-max-seconds+ 0)
        (gemini::+gemini-backoff-jitter-max-seconds+ 0)
        (gemini::*gemini-backoff-jitter-function* (lambda (max-jitter-seconds)
                                                    (declare (ignore max-jitter-seconds))
                                                    0)))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::gemini-rate-limit)
                 (lambda (&key timeout-ms model-id)
                   (declare (ignore timeout-ms model-id))
                   nil))
           (setf (fdefinition 'google:google-post)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (incf calls)
                   (if (= calls 1)
                       (error "HTTP 429 Too Many Requests")
                       (gemini::object :candidates
                                       (list (gemini::object :content (gemini::content :role "model" :parts (list (part "ok")))))))))
           (let ((response (gemini::%%invoke-gemini "models/gemini-flash-latest" (gemini::object))))
             (is (= 2 calls))
             (is (gemini::get-candidates response))))
      (setf (fdefinition 'google:google-post) orig-google-post)
      (setf (fdefinition 'gemini::gemini-rate-limit) orig-rate-limit))))

(test invoke-gemini-does-not-retry-non-429
  "Test that %%invoke-gemini does not retry non-429 transport errors."
  (let ((orig-google-post #'google:google-post)
        (orig-rate-limit #'gemini::gemini-rate-limit)
        (calls 0))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::gemini-rate-limit)
                 (lambda (&key timeout-ms model-id)
                   (declare (ignore timeout-ms model-id))
                   nil))
           (setf (fdefinition 'google:google-post)
                 (lambda (&rest args)
                   (declare (ignore args))
                   (incf calls)
                   (error "HTTP 500 Server Error")))
           (signals error
             (gemini::%%invoke-gemini "models/gemini-flash-latest" (gemini::object)))
           (is (= 1 calls)))
      (setf (fdefinition 'google:google-post) orig-google-post)
      (setf (fdefinition 'gemini::gemini-rate-limit) orig-rate-limit))))

(test openai-request-headers-explicit-override
  "Ensure explicit header override controls Authorization emission deterministically."
  (let ((headers-without-auth (gemini::openai-request-headers :authorization-header nil))
        (headers-with-auth (gemini::openai-request-headers :authorization-header "Bearer test-token")))
    (is (equal "application/json" (cdr (assoc "Content-Type" headers-without-auth :test #'equal))))
    (is (null (assoc "Authorization" headers-without-auth :test #'equal)))
    (is (equal "Bearer test-token" (cdr (assoc "Authorization" headers-with-auth :test #'equal))))))

(test openai-request-headers-runtime-config-precedence
  "Ensure runtime config variable is used for OpenAI Authorization header resolution."
  (let ((gemini::*openai-authorization* "Bearer runtime-config-token")
        (gemini::*openai-use-lm-studio-default-authorization* nil))
    (let ((headers (gemini::openai-request-headers)))
      (is (equal "Bearer runtime-config-token" (cdr (assoc "Authorization" headers :test #'equal)))))))

(test openai-usage-translation-and-normalization
  "Test that openai-usage->gemini-usage correctly extracts token stats and adjusts candidates token count when thoughts are present."
  (let* ((mock-usage-with-reasoning
           (gemini::object :prompt_tokens 15
                           :completion_tokens 120
                           :completion_tokens_details (gemini::object :reasoning_tokens 80)))
         (gemini-usage (gemini::openai-usage->gemini-usage mock-usage-with-reasoning)))
    (is (= 15 (gemini::get-prompt-token-count gemini-usage)))
    (is (= 80 (gemini::get-thoughts-token-count gemini-usage)))
    ;; completion_tokens (120) - reasoning_tokens (80) = candidates (40)
    (is (= 40 (gemini::get-candidates-token-count gemini-usage))))

  (let* ((mock-usage-no-reasoning
           (gemini::object :prompt_tokens 10
                           :completion_tokens 50))
         (gemini-usage (gemini::openai-usage->gemini-usage mock-usage-no-reasoning)))
    (is (= 10 (gemini::get-prompt-token-count gemini-usage)))
    (is (null (gemini::get-thoughts-token-count gemini-usage)))
    (is (= 50 (gemini::get-candidates-token-count gemini-usage)))))

(test generate-content-thin-loop-hits-hard-limit
  "Test that repeated thin responses trigger recursive reinvocation until the hard limit is reached."
  (let ((orig-invoke #'gemini::%invoke-gemini)
        (gemini::*generation-recursion-hard-limit* 2)
        (gemini::*echo-result* nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::%invoke-gemini)
                 (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                   (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                   (values
                    (gemini::object :candidates
                                    (list (gemini::object :content (gemini::content :role "model" :parts (list (part ""))))))
                    (gemini::object :prompt-token-count 1 :candidates-token-count 0))))
           (signals gemini::generation-recursion-limit-exceeded
                        (gemini::%generate-content gemini::*gemini-flash* nil nil "thin loop" nil nil nil nil 0)))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test generate-content-large-thoughts-thin-response-retry
  "Test that a response with very large thoughts count (> 10000) and < 2 candidate tokens triggers a retry/continuation."
  (let ((orig-invoke #'gemini::%invoke-gemini)
        (gemini::*generation-recursion-hard-limit* 2)
        (gemini::*echo-result* nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::%invoke-gemini)
                 (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                   (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                   (values
                    (gemini::object :candidates
                                    (list (gemini::object :content (gemini::content :role "model" :parts (list (part ""))))))
                    (gemini::object :prompt-token-count 1 :thoughts-token-count 15000 :candidates-token-count 0))))
           ;; This should signal generation-recursion-limit-exceeded because it retries recursively
           (signals gemini::generation-recursion-limit-exceeded
             (gemini::%generate-content gemini::*gemini-flash* nil nil "large thoughts thin loop" nil nil nil nil 0)))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test lmstudio-thin-responses-do-not-requery
  "Ensure LM Studio backends return promptly instead of recursively retrying zero-candidate responses."
  (let ((orig-invoke #'gemini::%invoke-gemini)
        (calls 0)
        (gemini::*echo-result* nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::%invoke-gemini)
                (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                  (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                  (incf calls)
                  (values
                   (gemini::object :candidates
                                   (list (gemini::object
                                          :content (gemini::content :role "model"
                                                                    :parts (list (part ""))))))
                   (gemini::object :prompt-token-count 1
                                   :thoughts-token-count 15000
                                   :candidates-token-count 0))))
           (let* ((config (make-instance 'gemini::persona-config
                                        :name "lmstudio-thin-response-test"
                                        :googleapi :lmstudio-api
                                        :model "qwen/qwen3.6-27b"
                                        :url "http://127.0.0.1:1234/api/v1/chat"))
                 (generator (make-instance 'gemini::content-generator :config config))
                 (response (gemini::%generate-content generator nil nil "thin lmstudio" nil nil nil nil 0)))
            (is (= 1 calls))
            (is (equal "" (content->text response)))))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test report-elapsed-time-preserves-multiple-values
  "Verify report-elapsed-time does not discard secondary values from wrapped calls."
  (is (equal '(:alpha :beta)
           (multiple-value-list
            (gemini::report-elapsed-time "multiple-values-test"
              (values :alpha :beta))))))

(test lmstudio-valid-short-response-does-not-requery
  "Ensure LM Studio responses with real content and usage metadata are returned on the first attempt."
  (let ((orig-invoke #'gemini::%invoke-gemini)
       (calls 0)
       (gemini::*echo-result* nil))
    (unwind-protect
        (progn
          (setf (fdefinition 'gemini::%invoke-gemini)
                (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                  (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                  (incf calls)
                  (values
                   (gemini::object
                    :candidates
                    (list (gemini::object
                           :content (gemini::content
                                     :role "model"
                                     :parts (list (part "OK"))))))
                   (gemini::object
                    :prompt-token-count 15
                    :thoughts-token-count 31
                    :candidates-token-count 4))))
          (let* ((config (make-instance 'gemini::persona-config
                                       :name "lmstudio-valid-short-response-test"
                                       :googleapi :lmstudio-api
                                       :model "qwen/qwen3.6-27b"
                                       :url "http://127.0.0.1:1234/api/v1/chat"))
                 (generator (make-instance 'gemini::content-generator :config config))
                 (response (gemini::%generate-content generator nil nil "Reply with exactly: OK" nil nil nil nil 0)))
            (is (= 1 calls))
            (is (equal "OK" (content->text response)))))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test generate-content-function-call-loop-hits-hard-limit
  "Test that repeated function-call recursion paths share the same hard recursion budget."
  (let ((orig-invoke #'gemini::%invoke-gemini)
        (gemini::*generation-recursion-hard-limit* 2)
        (gemini::*echo-result* nil)
        (gemini::*trace-function-calls* nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::%invoke-gemini)
                 (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                   (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                   (values
                    (gemini::object
                     :candidates
                     (list (gemini::object
                            :content (gemini::content :role "model"
                                                      :parts (list (part (gemini::function-call :name "missing_fn"
                                                                                                :args (gemini::object))))))))
                    (gemini::object :prompt-token-count 1 :candidates-token-count 1))))
           (signals gemini::generation-recursion-limit-exceeded
             (gemini::%generate-content gemini::*gemini-flash* nil nil "function loop" nil nil nil nil 0)))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test generate-content-mixed-loop-hits-hard-limit
  "Test that mixed thin-response and function-call recursion paths share one hard recursion budget."
  (let ((orig-invoke #'gemini::%invoke-gemini)
        (calls 0)
        (gemini::*generation-recursion-hard-limit* 2)
        (gemini::*echo-result* nil)
        (gemini::*trace-function-calls* nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::%invoke-gemini)
                 (lambda (content-generator model-id payload &key read-timeout connect-timeout)
                   (declare (ignore content-generator model-id payload read-timeout connect-timeout))
                   (incf calls)
                   (if (= calls 1)
                       ;; First response is thin (no candidate tokens) to trigger thin-response recursion.
                       (values
                        (gemini::object :candidates
                                        (list (gemini::object :content (gemini::content :role "model"
                                                                                       :parts (list (part ""))))))
                        (gemini::object :prompt-token-count 1 :candidates-token-count 0))
                       ;; Second response is function-call output to trigger tool recursion on same budget.
                       (values
                        (gemini::object
                         :candidates
                         (list (gemini::object
                                :content (gemini::content :role "model"
                                                          :parts (list (part (gemini::function-call :name "missing_fn"
                                                                                                    :args (gemini::object))))))))
                        (gemini::object :prompt-token-count 1 :candidates-token-count 1)))))
           (signals gemini::generation-recursion-limit-exceeded
             (gemini::%generate-content gemini::*gemini-flash* nil nil "mixed loop" nil nil nil nil 0))
           (is (= 2 calls)))
      (setf (fdefinition 'gemini::%invoke-gemini) orig-invoke))))

(test build-openai-payload-translation-invariants
  "Test that system instruction and tool declarations are translated into OpenAI payload fields."
  (let* ((payload (gemini::object
                   :contents (list (gemini::content :role "user" :parts (list (part "hello"))))
                   :system-instruction (gemini::content :role "system" :parts (list (part "sys")))
                   :tools (vector
                           (gemini::object
                            :function-declarations
                            (vector (gemini::function-declaration
                                     :name "machineType"
                                     :description "returns machine type"
                                     :parameters (gemini::object :type "object")))))))
         (openai (gemini::build-openai-payload "mock-model" payload))
         (messages (gemini::get-messages openai))
         (tools (gemini::get-tools openai)))
    (is (equal "mock-model" (gemini::get-model openai)))
    (is (>= (length messages) 2))
    (is (equal "system" (gemini::openai-field (elt messages 0) :role "role")))
    (is (equal "sys" (gemini::openai-field (elt messages 0) :content "content")))
    (is (not (null tools)))
    (is (> (length tools) 0))
    (is (equal "function" (gemini::get-type (elt tools 0))))))

(test openai-tool-schema-normalization
  "Test that internal schema type encodings are normalized before OpenAI tool serialization."
  (let* ((payload (gemini::object
                   :tools (vector
                           (gemini::object
                            :function-declarations
                            (vector (gemini::function-declaration
                                     :name "machineType"
                                     :description "returns machine type"
                                     :parameters (gemini::schema :type :object
                                                                 :properties (gemini::object
                                                                              :detail (gemini::schema :type :string)))))))))
         (openai (gemini::build-openai-payload "mock-model" payload))
         (tool (elt (gemini::get-tools openai) 0))
         (function (gemini::openai-field tool :function "function"))
         (parameters (gemini::openai-field function :parameters "parameters"))
         (properties (gemini::openai-field parameters :properties "properties"))
         (detail-schema (gemini::openai-field properties :detail "detail")))
    (is (equal "object" (gemini::openai-field parameters :type "type")))
    (is (equal "string" (gemini::openai-field detail-schema :type "type")))))

(test openai-required-field-normalization
  "Test that OpenAI required fields serialize as strings, not character vectors."
  (let* ((schema (gemini::schema :type :object
                                 :properties (gemini::object
                                              :directory (gemini::schema :type :string)
                                              :mime-type (gemini::schema :type :string))
                                 :required (vector :directory :mime-type)))
         (payload (gemini::object
                   :tools (vector
                           (gemini::object
                            :function-declarations
                            (vector (gemini::function-declaration
                                     :name "writeFileBlob"
                                     :description "Write a blob to a file."
                                     :parameters schema))))))
         (openai (gemini::build-openai-payload "mock-model" payload))
         (tool (elt (gemini::get-tools openai) 0))
         (function (gemini::openai-field tool :function "function"))
         (parameters (gemini::openai-field function :parameters "parameters"))
         (required (gemini::openai-field parameters :required "required")))
    (is (equalp #("directory" "mimeType") required))))

(test schema-required-field-preservation
  "Test that schema constructors preserve the required field shape for Gemini serialization."
  (let* ((schema (gemini::schema :type :object
                                 :properties (gemini::object
                                              :directory (gemini::schema :type :string)
                                              :mime-type (gemini::schema :type :string))
                                 :required (vector :directory :pathname :mime-type))))
    (is (equalp #(:directory :pathname :mime-type) (gemini::get-required schema)))))

(test adapter-openai-response-normalization
  "Test shared adapter normalization for OpenAI responses and usage aliases."
  (let* ((decoded (jsonx:with-decoder-jrm-semantics
                    (cl-json:decode-json-from-string
                     "{\"model_name\":\"adapter-model\",\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"role\":\"assistant\",\"content\":\"adapter reply\"}}],\"usage\":{\"promptTokens\":7,\"completion_tokens\":31,\"completion_tokens_details\":{\"reasoning_tokens\":11}}}")))
         (response nil)
         (usage nil))
    (multiple-value-setq (response usage)
      (gemini::openai-response-hash->gemini-response decoded))
    (is (equal "adapter-model" (gemini::get-model-version response)))
    (is (equal "adapter reply"
               (gemini::content->text (gemini::get-content (elt (gemini::get-candidates response) 0)))))
    (is (= 7 (gemini::get-prompt-token-count usage)))
    (is (= 11 (gemini::get-thoughts-token-count usage)))
    (is (= 20 (gemini::get-candidates-token-count usage)))))

(test adapter-payload-preflight-validation
  "Test shared adapter payload validation rejects non-content entries and invalid field values."
  ;; 1. Valid payload should pass
  (finishes
    (gemini::validate-gemini-payload-shape
     (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello")))))))

  ;; 2. Non-content entries (not object)
  (is (not (null (search "contents[0]"
                         (handler-case (progn (gemini::validate-gemini-payload-shape (gemini::object :contents (list "not-content"))) "")
                           (error (e) (format nil "~a" e)))))))

  ;; 3. Missing parts inside content entry
  (is (not (null (search "missing required field 'parts'"
                         (handler-case (progn (gemini::validate-gemini-payload-shape (gemini::object :contents (list (gemini::object :role "user")))) "")
                           (error (e) (format nil "~a" e)))))))

  ;; 4. Invalid parts inside content entry
  (is (not (null (search "part must contain one of"
                         (handler-case (progn (gemini::validate-gemini-payload-shape 
                                               (gemini::object :contents (list (gemini::object :role "user" :parts (list (gemini::object :invalid-part-field 123))))))
                                              "")
                           (error (e) (format nil "~a" e)))))))

  ;; 5. Invalid text type inside text part
  (is (not (null (search "must be a string"
                         (handler-case (progn (gemini::validate-gemini-payload-shape 
                                               (gemini::object :contents (list (gemini::object :role "user" :parts (list (gemini::object :text 12345))))))
                                              "")
                           (error (e) (format nil "~a" e)))))))

  ;; 6. Invalid generation-config temperature
  (is (not (null (search "must be <= 2.0"
                         (handler-case (progn (gemini::validate-gemini-payload-shape 
                                               (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))
                                                               :generation-config (gemini::object :temperature 2.5)))
                                              "")
                           (error (e) (format nil "~a" e)))))))

  ;; 7. Invalid generation-config candidateCount
  (is (not (null (search "must be >= 1"
                         (handler-case (progn (gemini::validate-gemini-payload-shape 
                                               (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))
                                                               :generation-config (gemini::object :candidate-count 0)))
                                              "")
                           (error (e) (format nil "~a" e)))))))

  ;; 8. Invalid blob mimeType inside inlineData
  (is (not (null (search "missing required field 'mimeType'"
                         (handler-case (progn (gemini::validate-gemini-payload-shape 
                                               (gemini::object :contents (list (gemini::object :role "user" :parts (list (gemini::object :inline-data (gemini::object :data "abc")))))))
                                              "")
                           (error (e) (format nil "~a" e))))))))

(test gemini-backend-mock-test
  "Test the gemini-backend CLOS implementation with mocked %%invoke-gemini responses."
  (let* ((backend (make-instance 'gemini::gemini-backend))
         (mock-payload (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))))
         (mock-response
           (gemini::object :candidates
                           (list (gemini::object :content (gemini::content :role "model" :parts (list (part "gemini reply")))))
                           :usage-metadata
                           (gemini::object :prompt-token-count 10 :candidates-token-count 20)))
         (called-model-id nil)
         (called-payload nil)
         ;; Mock %%invoke-gemini
         (mock-invoke-gemini (lambda (model-id payload &key read-timeout connect-timeout)
                               (declare (ignore read-timeout connect-timeout))
                               (setf called-model-id model-id
                                     called-payload payload)
                               mock-response)))
    (let ((orig-invoke-gemini #'gemini::%%invoke-gemini))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-gemini) mock-invoke-gemini)
             
             ;; Call invoke-backend
             (multiple-value-bind (response usage)
                 (gemini::invoke-backend backend "mock-gemini-model" mock-payload)
               (is (equal "mock-gemini-model" called-model-id))
               (is (eq mock-payload called-payload))
               (is (equal "gemini reply" (content->text (get-content (elt (get-candidates response) 0)))))
               (is (= 10 (gemini::get-prompt-token-count usage)))
               (is (= 20 (gemini::get-candidates-token-count usage)))))
        ;; Restore
        (setf (fdefinition 'gemini::%%invoke-gemini) orig-invoke-gemini)))))

(test openai-backend-mock-test
  "Test the openai-backend CLOS implementation with mocked %%invoke-openai responses."
  (let* ((backend (make-instance 'gemini::openai-backend :url "http://mock-url/v1/chat/completions"))
         (mock-payload (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))))
         ;; Mock OpenAI JSON response containing choices and usage details
         (mock-response-json
           "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"openai reply\"}}],
             \"usage\": {\"prompt_tokens\": 5, \"completion_tokens\": 50, \"completion_tokens_details\": {\"reasoning_tokens\": 30}}}")
         (called-model-id nil)
         (called-payload nil)
         (called-url nil)
         ;; Mock %%invoke-openai
         (mock-invoke-openai (lambda (model-id payload &key url read-timeout connect-timeout)
                               (declare (ignore read-timeout connect-timeout))
                               (setf called-model-id model-id
                                     called-payload payload
                                     called-url url)
                               mock-response-json)))
    (let ((orig-invoke-openai #'gemini::%%invoke-openai))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-openai) mock-invoke-openai)
             
             ;; Call invoke-backend
             (multiple-value-bind (response usage)
                 (gemini::invoke-backend backend "mock-openai-model" mock-payload)
               (is (equal "mock-openai-model" called-model-id))
               (is (equal "http://mock-url/v1/chat/completions" called-url))
               (is (not (null called-payload)))
               (is (equal "openai reply" (content->text (get-content (elt (get-candidates response) 0)))))
               (is (= 5 (gemini::get-prompt-token-count usage)))
               (is (= 30 (gemini::get-thoughts-token-count usage)))
               ;; completion_tokens (50) - reasoning_tokens (30) = 20
               (is (= 20 (gemini::get-candidates-token-count usage)))))
        ;; Restore
        (setf (fdefinition 'gemini::%%invoke-openai) orig-invoke-openai)))))

(test gemini-backend-safety-block-test
  "Verify that invoke-backend signals an error when Gemini blocks output for safety/guideline reasons."
  (let* ((backend (make-instance 'gemini::gemini-backend))
         (mock-payload (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))))
         (mock-response (gemini::object :prompt-feedback (gemini::object :block-reason "SAFETY")
                                        :candidates (list (gemini::object :content (gemini::content :role "model" :parts (list (part "")))))))
         (mock-invoke-gemini (lambda (model-id payload &key read-timeout connect-timeout)
                               (declare (ignore model-id payload read-timeout connect-timeout))
                               mock-response)))
    (let ((orig-invoke-gemini #'gemini::%%invoke-gemini))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-gemini) mock-invoke-gemini)
             (signals gemini::gemini-api-error
               (gemini::invoke-backend backend "mock-gemini-model" mock-payload)))
        (setf (fdefinition 'gemini::%%invoke-gemini) orig-invoke-gemini)))))

(test openai-backend-content-filter-stop-test
  "Verify that invoke-backend signals an error when OpenAI-compatible responses stop due to content filtering/guidelines."
  (let* ((backend (make-instance 'gemini::openai-backend :url "http://mock-url/v1/chat/completions"))
         (mock-payload (gemini::object :contents (list (gemini::content :role "user" :parts (list (part "hello"))))))
         (mock-response-json
           "{\"choices\": [{\"finish_reason\": \"content_filter\", \"message\": {\"role\": \"assistant\", \"content\": \"\"}}], \"usage\": {\"prompt_tokens\": 2, \"completion_tokens\": 0}}")
         (mock-invoke-openai (lambda (model-id payload &key url read-timeout connect-timeout)
                               (declare (ignore model-id payload url read-timeout connect-timeout))
                               mock-response-json)))
    (let ((orig-invoke-openai #'gemini::%%invoke-openai))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-openai) mock-invoke-openai)
             (signals gemini::gemini-api-error
               (gemini::invoke-backend backend "mock-openai-model" mock-payload)))
        (setf (fdefinition 'gemini::%%invoke-openai) orig-invoke-openai)))))

(test openai-backend-function-calling-test
  "Verify that the OpenAI API backend correctly supports function calling by translating tools in the payload and parsing tool_calls in the response."
  (let* ((config (make-instance 'gemini::persona-config
                               :name "mock-persona"
                               :googleapi nil
                               :enable-misc-tools t
                               :model "gemma-4-e4b-uncensored"
                               :url "http://mock-openai-url/v1/chat/completions"))
         (generator (make-instance 'gemini::content-generator :config config))
         ;; Mock %%invoke-openai to return a tool call
         (mock-invoke-openai
           (lambda (model-id payload &key url read-timeout connect-timeout)
             (declare (ignore model-id url read-timeout connect-timeout))
             (declare (ignore payload))
             ;; Return a mock tool call for "machineType"
             "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": null, \"tool_calls\": [{\"id\": \"call_001\", \"type\": \"function\", \"function\": {\"name\": \"machineType\", \"arguments\": \"{}\"}}]}}], \"usage\": {\"prompt_tokens\": 10, \"completion_tokens\": 20}}")))
    (let ((orig-invoke-openai #'gemini::%%invoke-openai))
      (unwind-protect
           (progn
             (setf (fdefinition 'gemini::%%invoke-openai) mock-invoke-openai)
             
             ;; Execute content generation on the generator
             ;; We expect the loop to call %%invoke-openai, receive the tool call,
             ;; execute the "machineType" handler, and then make a second call to %%invoke-openai with the function response!
             ;; To stop the second call from causing an error, we can make the mock return a normal message on the second call.
             (let ((call-count 0)
                   (first-payload nil))
               (setf (fdefinition 'gemini::%%invoke-openai)
                     (lambda (model-id payload &key url read-timeout connect-timeout)
                       (declare (ignore model-id url read-timeout connect-timeout))
                       (incf call-count)
                       (if (= call-count 1)
                           (setf first-payload payload))
                       (if (= call-count 1)
                           "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": null, \"tool_calls\": [{\"id\": \"call_001\", \"type\": \"function\", \"function\": {\"name\": \"machineType\", \"arguments\": \"{}\"}}]}}], \"usage\": {\"prompt_tokens\": 10, \"completion_tokens\": 20}}"
                           "{\"choices\": [{\"message\": {\"role\": \"assistant\", \"content\": \"The machine type is x86_64.\"}}], \"usage\": {\"prompt_tokens\": 25, \"completion_tokens\": 15}}")))
               
               (multiple-value-bind (response usage)
                   (gemini::generate-content generator nil nil "What is the machine type?" nil nil nil)
                 (declare (ignore usage))
                 (is (= 2 call-count))
                 ;; Verify tools was correctly populated in the first payload
                 (is (not (null (gemini::get-tools first-payload))))
                 (let ((tools-array (gemini::get-tools first-payload)))
                   (is (> (length tools-array) 0))
                   (is (equal "function" (gemini::get-type (elt tools-array 0)))))
                 ;; Verify the final response text
                 (is (equal "The machine type is x86_64." (content->text response))))))
        ;; Restore
        (setf (fdefinition 'gemini::%%invoke-openai) orig-invoke-openai)))))

(test model-objects-and-registry
  "Verify that model objects can be created, registered, found, and are automatically enforced in persona-config and agent slots."
  (let* ((mock-model-id "models/mock-model-123")
         (model-obj (gemini::ensure-model mock-model-id)))
    ;; 1. Check basic model object properties
    (is (typep model-obj 'gemini::model))
    (is (equal mock-model-id (gemini::get-model-id model-obj)))
    (is (equal mock-model-id (gemini::get-model-name model-obj)))
    
    ;; 2. Register with a custom name
    (let ((custom-model (make-instance 'gemini::model :id "id-xyz" :name "custom-name")))
      (gemini::register-model custom-model)
      (is (eq custom-model (gemini::find-model "id-xyz")))
      (is (eq custom-model (gemini::find-model "custom-name")))
      (is (eq custom-model (gemini::ensure-model "id-xyz")))
      (is (eq custom-model (gemini::ensure-model "custom-name")))
      (is (eq custom-model (gemini::ensure-model custom-model))))

    ;; 3. Check persona-config auto-conversion
    (let ((config (make-instance 'gemini::persona-config
                                 :name "test-persona"
                                 :model "models/gemini-flash-latest")))
      (is (typep (gemini::get-model config) 'gemini::model))
      (is (equal "models/gemini-flash-latest" (gemini::get-model-id (gemini::get-model config)))))

    ;; 4. Check agent auto-conversion
    (let ((agent (make-instance 'gemini::agent
                                :name "test-agent"
                                :instruction "test"
                                :model "models/gemini-pro-latest")))
      (is (typep (gemini::agent-model agent) 'gemini::model))
      (is (equal "models/gemini-pro-latest" (gemini::get-model-id (gemini::agent-model agent)))))

    ;; 5. Check resolve-agent-model behavior
    (let* ((agent (make-instance 'gemini::agent
                                 :name "test-agent"
                                 :instruction "test"
                                 :model "models/gemini-pro-latest"))
           (resolved (gemini::resolve-agent-model agent nil)))
      (is (typep resolved 'gemini::model))
      (is (equal "models/gemini-pro-latest" (gemini::get-model-id resolved))))))

(test qwen-persona-uses-lmstudio-chat-backend
  "Verify the checked-in Qwen persona targets the LM Studio /api/v1/chat backend with the live model identifier."
  (let* ((config (gemini::load-persona-config "Qwen3.6-27b"))
        (backend (gemini::resolve-backend-instance config)))
    (is (eq :lmstudio-api (gemini::get-googleapi config)))
    (is (null (gemini::get-enable-lmstudio-tools config)))
    (is (null (gemini::get-enable-filesystem-tools config)))
    (is (null (gemini::get-enable-git-tools config)))
    (is (null (gemini::get-enable-interaction-tools config)))
    (is (null (gemini::get-enable-mcp-tools config)))
    (is (null (gemini::get-enable-web-tools config)))
    (is (equal "qwen/qwen3.6-27b"
              (gemini::get-model-id (gemini::get-model config))))
    (is (equal "http://127.0.0.1:1234/api/v1/chat"
              (gemini::get-url config)))
    (is (typep backend 'gemini::lmstudio-backend))
    (is (equal "http://127.0.0.1:1234/api/v1/chat"
              (gemini::get-backend-url backend)))))

(test content-generator-lazily-creates-memory-mcp-server
  "Verify content generators do not construct their memory MCP server until MCP-backed behavior is used."
  (let ((orig-memory-mcp-server (fdefinition 'gemini::memory-mcp-server))
        (calls 0))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::memory-mcp-server)
                 (lambda (memory-file)
                   (declare (ignore memory-file))
                   (incf calls)
                   :mock-memory-server))
           (let* ((config (make-instance 'gemini::persona-config
                                        :name "lazy-memory-mcp-test"
                                        :enable-mcp-tools t))
                  (generator (make-instance 'gemini::content-generator :config config)))
             (is (null (slot-value generator 'gemini::memory-mcp-server)))
             (is (eql :mock-memory-server (gemini::get-memory-mcp-server generator)))
             (is (= 1 calls))
             (is (eql :mock-memory-server (gemini::get-memory-mcp-server generator)))
             (is (= 1 calls))))
      (setf (fdefinition 'gemini::memory-mcp-server) orig-memory-mcp-server))))

(test build-gemini-payload-skips-mcp-when-disabled
  "Verify disabling MCP tools prevents hidden MCP startup during payload construction."
  (let ((orig-start-mcp-servers (fdefinition 'gemini::start-mcp-servers))
        (orig-memory-mcp-server (fdefinition 'gemini::memory-mcp-server))
        (start-calls 0)
        (memory-calls 0))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::start-mcp-servers)
                 (lambda ()
                   (incf start-calls)
                   (error "MCP startup should not happen when MCP tools are disabled.")))
           (setf (fdefinition 'gemini::memory-mcp-server)
                 (lambda (memory-file)
                   (declare (ignore memory-file))
                   (incf memory-calls)
                   (error "Memory MCP server should not be created when MCP tools are disabled.")))
           (let* ((config (make-instance 'gemini::persona-config
                                        :name "mcp-disabled-payload-test"
                                        :enable-mcp-tools nil))
                  (generator (make-instance 'gemini::content-generator :config config)))
             (multiple-value-bind (payload prompt-context)
                 (gemini::build-gemini-payload generator nil nil "Hello" nil nil nil nil)
               (declare (ignore prompt-context))
               (is (null (gemini::get-tools payload)))
               (is (= 0 start-calls))
               (is (= 0 memory-calls)))))
      (setf (fdefinition 'gemini::start-mcp-servers) orig-start-mcp-servers)
      (setf (fdefinition 'gemini::memory-mcp-server) orig-memory-mcp-server))))

(test find-mcp-server-starts-servers-on-demand
  "Verify MCP server lookup now initializes configured servers on first use instead of at load time."
  (let ((orig-start-mcp-servers (fdefinition 'gemini::start-mcp-servers))
        (saved-servers gemini::*mcp-servers*)
        (calls 0))
    (unwind-protect
         (progn
           (setf gemini::*mcp-servers* nil)
           (setf (fdefinition 'gemini::start-mcp-servers)
                 (lambda ()
                   (incf calls)
                   (setf gemini::*mcp-servers*
                         (list (make-instance 'gemini::mcp-server
                                              :name "memory"
                                              :config nil
                                              :delayed-prompts nil
                                              :delayed-resources nil
                                              :delayed-resource-templates nil
                                              :delayed-tools nil)))))
           (let ((server (gemini::find-mcp-server "memory")))
             (is (typep server 'gemini::mcp-server))
             (is (equal "memory" (gemini::get-name server)))
             (is (= 1 calls))))
      (setf (fdefinition 'gemini::start-mcp-servers) orig-start-mcp-servers)
      (setf gemini::*mcp-servers* saved-servers))))

(test mcp-lifecycle-helpers-are-exported
  "Verify explicit MCP lifecycle helpers are part of the public GEMINI package API."
  (multiple-value-bind (start-symbol start-status)
      (find-symbol "START-MCP-SERVERS" "GEMINI")
    (is (eq :external start-status))
    (is (fboundp start-symbol)))
  (multiple-value-bind (stop-symbol stop-status)
      (find-symbol "STOP-MCP-SERVERS" "GEMINI")
    (is (eq :external stop-status))
    (is (fboundp stop-symbol)))
  (multiple-value-bind (restart-symbol restart-status)
      (find-symbol "RESTART-MCP-SERVERS" "GEMINI")
    (is (eq :external restart-status))
    (is (fboundp restart-symbol))))

(test explicit-session-helper-api-is-exported
  "Verify explicit-session helper entry points are exposed through the public GEMINI package API."
  (dolist (name '("ANALYZE-COMPONENT-WITH-SESSION"
                  "ANALYZE-FILE-WITH-SESSION"
                  "ANALYZE-LISP-WITH-SESSION"
                  "ANALYZE-SYSTEM-DEFINITION-WITH-SESSION"
                  "CHAT-WITH-SESSION"
                  "CONDENSE-PROMPT-WITH-SESSION"
                  "CONTINUE-GEMINI-WITH-SESSION"
                  "FLASH-COMPRESS-WITH-SESSION"
                  "GEMINI-FLASH-LITE-WITH-SESSION"
                  "GEMINI-FLASH-WITH-SESSION"
                  "GEMINI-PRO-WITH-SESSION"
                  "GEMINI-UNCENSORED-WITH-SESSION"
                  "IMPROVE-PROMPT-WITH-SESSION"
                  "IMPROVE-SYSTEM-INSTRUCTION-WITH-SESSION"
                  "INVOKE-GEMINI-WITH-SESSION"
                  "INVOKE-INTERACTION-WITH-SESSION"
                  "NEW-CHAT-WITH-SESSION"
                  "PROMPT-PREDICATE-WITH-SESSION"
                  "QWEN-WITH-SESSION"
                  "STRENGTHEN-SYSTEM-INSTRUCTION-WITH-SESSION"
                  "WEAKEN-SYSTEM-INSTRUCTION-WITH-SESSION"))
    (multiple-value-bind (symbol status)
        (find-symbol name "GEMINI")
      (is (eq :external status))
      (is (fboundp symbol)))))

(test lmstudio-auto-tools-require-explicit-opt-in
  "Verify LM Studio personas do not auto-attach generator tools unless explicitly opted in."
  (flet ((build-payload-tools (enable-lmstudio-tools)
           (let* ((config (make-instance 'gemini::persona-config
                                        :name "lmstudio-policy-test"
                                        :googleapi :lmstudio-api
                                        :enable-misc-tools t
                                        :enable-lmstudio-tools enable-lmstudio-tools
                                        :enable-mcp-tools nil
                                        :model "qwen/qwen3.6-27b"
                                        :url "http://127.0.0.1:1234/api/v1/chat"))
                 (generator (make-instance 'gemini::content-generator :config config)))
             (multiple-value-bind (payload prompt-context)
                (gemini::build-gemini-payload generator nil nil "Hello LM Studio" nil nil nil nil)
              (declare (ignore prompt-context))
              (gemini::get-tools payload)))))
    (is (null (build-payload-tools nil)))
    (is (not (null (build-payload-tools t))))))

(test lmstudio-build-payload-skips-auto-personality
  "Verify LM Studio payloads do not inherit the rotating personality prompt by default."
  (let ((*enable-personality* t))
    (let* ((config (make-instance 'gemini::persona-config
                                :name "lmstudio-personality-test"
                                :googleapi :lmstudio-api
                                :model "qwen/qwen3.6-27b"
                                :url "http://127.0.0.1:1234/api/v1/chat"))
           (generator (make-instance 'gemini::content-generator :config config)))
      (multiple-value-bind (payload prompt-context)
          (gemini::build-gemini-payload generator nil nil "Hello LM Studio" nil nil nil nil)
        (declare (ignore prompt-context))
        (let ((lmstudio-payload (gemini::build-lmstudio-payload
                                "qwen/qwen3.6-27b"
                                payload
                                (gemini:make-runtime-session)
                                :content-generator generator)))
          (is (null (gemini::adapter-field lmstudio-payload "system_prompt" :system_prompt))))))))

(test lmstudio-request-tools-override-persona-defaults
  "Verify request-level tools can disable or replace persona-level LM Studio auto-tools."
  (let* ((config (make-instance 'gemini::persona-config
                               :name "lmstudio-request-override-test"
                               :googleapi :lmstudio-api
                               :enable-misc-tools t
                               :enable-lmstudio-tools t
                               :enable-mcp-tools nil
                               :model "qwen/qwen3.6-27b"
                               :url "http://127.0.0.1:1234/api/v1/chat"))
         (generator (make-instance 'gemini::content-generator :config config))
         (custom-tools
           (vector
            (gemini::object
             :function-declarations
             (vector
              (gemini::function-declaration
              :name "customEcho"
              :description "Echoes custom input"
              :parameters (gemini::schema
                           :type :object
                           :properties (gemini::object :value (gemini::schema :type :string))
                           :required (vector :value))))))))
    (flet ((payload-tools (&rest args)
             (multiple-value-bind (payload prompt-context)
                (apply #'gemini::build-gemini-payload
                       generator nil nil "Hello LM Studio" nil nil nil nil
                       args)
              (declare (ignore prompt-context))
              (gemini::get-tools payload))))
      (is (not (null (payload-tools))))
      (is (null (payload-tools :tools nil)))
      (let* ((tools (payload-tools :tools custom-tools))
             (tool (elt tools 0))
             (declarations (coerce (gemini::get-function-declarations tool) 'list)))
        (is (= 1 (length declarations)))
        (is (equal "customEcho"
                  (gemini::get-name (first declarations))))))))

(test qwen-entry-point-allows-explicit-request-tools
  "Verify the QWen entry point can still opt into explicit request-level tools after plain-chat defaults are restored."
  (let ((orig-generate-content #'gemini::generate-content)
        (captured-tools :unset)
        (captured-tool-config :unset))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                         &key tools tool-config read-timeout connect-timeout)
                  (declare (ignore content-generator context mood prompt parts files system-instruction
                                   read-timeout connect-timeout))
                  (setf captured-tools tools
                        captured-tool-config tool-config)
                  (gemini::content :role "model" :parts (list (part "ok")))))
           (let* ((tools (vector (gemini::object :function-declarations (vector))))
                  (tool-config (gemini::object :function-calling-config
                                              (gemini::object :mode "AUTO"))))
             (gemini::qwen "Hello" :tools tools :tool-config tool-config)
             (is (eq tools captured-tools))
             (is (eq tool-config captured-tool-config))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test diary-tools-require-explicit-opt-in
  "Verify diary-backed personas only expose writeDiaryEntry when explicitly enabled in config."
  (flet ((tool-names (persona-name &key enable-diary-tools)
           (let* ((config (make-instance 'gemini::persona-config
                                        :name persona-name
                                        :enable-diary-tools enable-diary-tools))
                 (generator (make-instance 'gemini::content-generator :config config))
                 (entries (gemini::standard-functions-and-handlers generator)))
             (mapcar (lambda (entry)
                      (gemini::get-name (car entry)))
                    entries))))
    (is (not (member "writeDiaryEntry"
                    (tool-names "Default" :enable-diary-tools nil)
                    :test #'equal)))
    (is (member "writeDiaryEntry"
               (tool-names "Default" :enable-diary-tools t)
               :test #'equal))
    (is (eq t (gemini::get-enable-diary-tools
              (gemini::load-persona-config "Default"))))
    (is (eq t (gemini::get-enable-diary-tools
              (gemini::load-persona-config "Janus"))))))

(test token-accounting-thread-safety
  "Test that global token logging is robust and thread-safe under high concurrent contention."
  (let* ((num-threads 10)
        (increments-per-thread 100)
         (mock-metadata (gemini::object :prompt-token-count 1
                                        :thoughts-token-count 1
                                        :candidates-token-count 1))
         (threads nil)
         (start-latch nil))
    
    ;; Reset global counters under lock first
    (sb-thread:with-mutex (gemini::*gemini-token-lock*)
      (setf gemini::*accumulated-prompt-tokens* 0
            gemini::*accumulated-response-tokens* 0))
    
    ;; Spawn threads
    (dotimes (i num-threads)
      (push (sb-thread:make-thread
             (lambda ()
               ;; Busy spin until the starting pistol is fired
               (loop while (not start-latch))
               ;; Fire the increments
               (dotimes (j increments-per-thread)
                 ;; Silence *trace-output* to prevent terminal spamming during test
                 (let ((*trace-output* (make-broadcast-stream)))
                   (gemini::process-usage-metadata mock-metadata)))))
            threads))
    
    ;; Fire starting pistol!
    (setf start-latch t)
    
    ;; Join all threads
    (dolist (thread threads)
      (sb-thread:join-thread thread))
    
    ;; Verify correct atomic aggregation
    (is (= (* num-threads increments-per-thread) gemini::*accumulated-prompt-tokens*))
    (is (= (* num-threads increments-per-thread 2) gemini::*accumulated-response-tokens*))))

(test merge-prompts-and-parts
  "Test merging user prompts with files and other parts."
  (let ((normal-part (part "user input"))
        (file-part (part "file content"))
        (model-content (content :parts (list (part "model output")) :role "model")))
    ;; 1. merge-user-prompt-and-files when first content is a "user" role
    (let* ((initial-prompt (list (content :parts (list normal-part) :role "user")))
           (merged (gemini::merge-user-prompt-and-files initial-prompt (list file-part))))
      (is (= 1 (length merged)))
      (is (equal "user" (get-role (car merged))))
      (is (= 2 (length (coerce (get-parts (car merged)) 'list)))))
      
    ;; 2. merge-user-prompt-and-files when first content is empty
    (let ((merged (gemini::merge-user-prompt-and-files nil (list file-part))))
      (is (= 1 (length merged)))
      (is (equal "user" (get-role (car merged))))
      ;; should append "Please analyze the attached files."
      (is (= 2 (length (coerce (get-parts (car merged)) 'list)))))

    ;; 3. merge-user-prompt-and-files when first content has a "model" role
    (let* ((initial-prompt (list model-content))
           (merged (gemini::merge-user-prompt-and-files initial-prompt (list file-part))))
      ;; Should prepend the new "user" content and keep the existing "model" content
      (is (= 2 (length merged)))
      (is (equal "user" (get-role (car merged))))
      (is (equal "model" (get-role (cadr merged))))
      (is (= 2 (length (coerce (get-parts (car merged)) 'list)))))

    ;; 4. merge-user-prompt-and-parts when first content has a "model" role
    (let* ((initial-prompt (list model-content))
           (merged (gemini::merge-user-prompt-and-parts initial-prompt (list file-part))))
      ;; Should prepend the new "user" content and keep the existing "model" content
      (is (= 2 (length merged)))
      (is (equal "user" (get-role (car merged))))
      (is (equal "model" (get-role (cadr merged))))
      (is (= 1 (length (coerce (get-parts (car merged)) 'list)))))))

(test prompt-conversion
  "Test ->prompt conversion of various types into content structures."
  ;; 1. Single string -> list of content
  (let ((res (gemini::->prompt "hello")))
    (is (= 1 (length res)))
    (is (gemini::content? (car res)))
    (is (equal "user" (get-role (car res)))))
    
  ;; 2. List of strings -> list of content
  (let ((res (gemini::->prompt '("hello" "world"))))
    (is (= 1 (length res)))
    (is (gemini::content? (car res)))
    (is (= 2 (length (coerce (get-parts (car res)) 'list)))))
    
  ;; 3. Single part -> list of content
  (let ((res (gemini::->prompt (part "hello part"))))
    (is (= 1 (length res)))
    (is (gemini::content? (car res)))
    (is (equal "user" (get-role (car res)))))
    
  ;; 4. List of parts -> list of content
  (let ((res (gemini::->prompt (list (part "p1") (part "p2")))))
    (is (= 1 (length res)))
    (is (= 2 (length (coerce (get-parts (car res)) 'list)))))
    
  ;; 5. Invalid type should raise error
  (signals error (gemini::->prompt 12345)))

(test turbo-detection
  "Test detection of turbo characters in prompt prefix."
  (is (not (null (gemini::turbo-prompt? "$hello"))))
  (is (not (null (gemini::turbo-prompt? "+hello"))))
  (is (null (gemini::turbo-prompt? "hello"))))

(test conversational-agent-clos-mapping
  "Test that chatbot returns a closure wrapping a real conversational-agent CLOS instance subclassing agent."
  (let* ((generator gemini::*gemini-flash*)
         (bot (gemini::chatbot generator)))
    ;; Verify it is a funcallable closure/function
    (is (functionp bot))
    ;; Verify we can retrieve the CLOS agent instance using the :agent! message
    (let ((agent (funcall bot :agent!)))
      (is (typep agent 'gemini::conversational-agent))
      (is (typep agent 'gemini::agent))
      ;; Check slots
      (is (eq generator (gemini::get-content-generator agent)))
      (is (listp (gemini::conversational-agent-conversation agent)))
      (is (equal "neutral" (gemini::conversational-agent-mood agent)))
      ;; Verify it contains standard agent slots
      (is (equal (gemini::get-name generator) (gemini::agent-name agent))))))

(test safe-float-parsing
  "Test that parse-float-safely accurately and securely parses floats without Lisp reader macros."
  (is (= 0.0 (gemini::parse-float-safely "")))
  (is (= 0.0 (gemini::parse-float-safely "abc")))
  (is (= 1.0 (gemini::parse-float-safely "1")))
  (is (< (abs (- 0.85 (gemini::parse-float-safely "0.85"))) 0.001))
  (is (< (abs (- 0.85 (gemini::parse-float-safely "abc0.85xyz"))) 0.001))
  (is (< (abs (- 123.456 (gemini::parse-float-safely "  123.456  "))) 0.001)))

(test content-to-text-null-handling
  "Test that content->text safely returns an empty string when the input content is NIL."
  (is (equal "" (gemini::content->text nil))))

(test similarity-commutativity
  "Test that the similarity function is commutative (symmetrical)."
  (uiop:with-temporary-file (:stream s1 :pathname p1 :direction :output)
    (write-string "hello" s1)
    (close s1)
    (uiop:with-temporary-file (:stream s2 :pathname p2 :direction :output)
      (write-string "world" s2)
      (close s2)
      (let* ((called 0)
             ;; Mock gemini-flash-lite
             (mock-flash-lite (lambda (prompt &key &allow-other-keys)
                                (declare (ignore prompt))
                                (incf called)
                                (gemini::content :parts (list (part "0.75")) :role "model"))))
        (let ((orig-flash-lite (fdefinition 'gemini::gemini-flash-lite)))
          (unwind-protect
               (progn
                 (setf (fdefinition 'gemini::gemini-flash-lite) mock-flash-lite)
                 ;; Compare p1 and p2
                 (let ((score1 (gemini::similarity p1 p2))
                       (score2 (gemini::similarity p2 p1)))
                   (is (= 0.75 score1))
                   (is (= 0.75 score2))
                   (is (= score1 score2))
                   (is (= 2 called))))
            ;; Restore
            (setf (fdefinition 'gemini::gemini-flash-lite) orig-flash-lite)))))))

(test file-keyword-argument-translation
  "Verify that providing the :file keyword argument to entry points translates correctly to a singleton list for files in generate-content."
  (let ((orig-generate-content #'gemini::generate-content)
        (seen-files nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore content-generator context mood prompt parts system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (setf seen-files files)
                   (gemini:content :role "model" :parts (list (part "ok")))))
           ;; Test invoke-gemini with :file
           (gemini:invoke-gemini "hello" :file "test-file.txt")
           (is (equal '("test-file.txt") seen-files))
           
           ;; Test prompt-predicate with :file
           (gemini::prompt-predicate "is this true?" :file "test-file-2.txt")
           (is (equal '("test-file-2.txt") seen-files))

           ;; Test chatbot / chat interface with :file
           (let ((agent (make-instance 'gemini::conversational-agent
                                      :name "TestBot"
                                      :instruction ""
                                      :model "model"
                                      :content-generator gemini::*default-content-generator*)))
             (gemini::invoke agent "hi" :file "test-file-3.txt")
             (is (equal '("test-file-3.txt") seen-files))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test dynamic-content-generator-registry
  "Verify that the dynamic model/content-generator registry supports registering, finding, listing, and dynamically invoking model configurations at runtime."
  (let ((mock-generator (make-instance 'gemini::content-generator :config (gemini::load-persona-config "Default")))
        (orig-generate-content #'gemini::generate-content)
        (called nil))
    (unwind-protect
         (progn
           ;; 1. Register model
           (gemini:register-content-generator "mock-test-model" mock-generator)
           (is (eq mock-generator (gemini:find-content-generator "mock-test-model")))
           (is (member "MOCK-TEST-MODEL" (gemini:list-content-generators) :test #'string=))

           ;; 2. Set up mock for generate-content to assert dispatch
           (setf (fdefinition 'gemini::generate-content)
                 (lambda (content-generator context mood prompt parts files system-instruction
                          &key tools tool-config read-timeout connect-timeout)
                   (declare (ignore context mood prompt parts files system-instruction
                                    tools tool-config read-timeout connect-timeout))
                   (is (eq content-generator mock-generator))
                   (setf called t)
                   (gemini:content :role "model" :parts (list (part "registry ok")))))

           ;; 3. Invoke model
           (gemini:invoke-model "mock-test-model" "hi")
           (is (not (null called)))

           ;; 4. Unregister model
           (gemini:unregister-content-generator "mock-test-model")
           (is (null (gemini:find-content-generator "mock-test-model"))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

(test jsonrpc-client-self-healing-and-restart
  "Verify that the jsonrpc-client self-healing watchdog restarts/reconnects the process if it goes down."
  (let ((client (make-instance 'gemini::jsonrpc-client
                               :name "mock-mcp-server"
                               :command '("sbcl" "--version")
                               :args '()
                               :unsolicited-handler (lambda (msg) (declare (ignore msg)) nil)))
        (orig-launch-program #'uiop:launch-program)
        (launch-called 0)
        (orig-start-threads #'gemini::start-jsonrpc-client-threads)
        (orig-stop-threads #'gemini::stop-jsonrpc-client-threads))
    (unwind-protect
         (progn
           ;; 1. Mock launch-program
           (setf (fdefinition 'uiop:launch-program)
                 (lambda (cmd-args &key error-output input output)
                   (declare (ignore error-output input output))
                   (is (equal '("sbcl" "--version") cmd-args))
                   (incf launch-called)
                   (make-instance 'uiop/launch-program::process-info)))
           
           ;; 2. Mock uiop:process-alive-p to return NIL (dead)
           (setf (fdefinition 'uiop:process-alive-p)
                 (lambda (proc)
                   (declare (ignore proc))
                   nil))

           ;; 3. Mock start/stop threads
           (setf (fdefinition 'gemini::start-jsonrpc-client-threads)
                 (lambda (c eof)
                   (declare (ignore c eof))
                   nil))
           (setf (fdefinition 'gemini::stop-jsonrpc-client-threads)
                 (lambda (c &optional eof)
                   (declare (ignore c eof))
                   nil))

           ;; 4. Set process-info initially to a dummy process-info
           (setf (gemini::process-info client) (make-instance 'uiop/launch-program::process-info))

           ;; 5. Call ensure-jsonrpc-client-alive (which should trigger a restart/reconnection)
           (gemini::ensure-jsonrpc-client-alive client)
           
           ;; 6. Assert that uiop:launch-program was called exactly once to restart
           (is (= 1 launch-called)))
      ;; Restore all original functions
      (setf (fdefinition 'uiop:launch-program) orig-launch-program)
      (setf (fdefinition 'uiop:process-alive-p) #'uiop:process-alive-p)
      (setf (fdefinition 'gemini::start-jsonrpc-client-threads) orig-start-threads)
      (setf (fdefinition 'gemini::stop-jsonrpc-client-threads) orig-stop-threads))))
