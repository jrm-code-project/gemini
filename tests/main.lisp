;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

;;; Main entry point for running all tests. This is what the .asd file calls.
(def-suite all-tests
  :description "The master suite of all tests for the Gemini system.")

(defun run! ()
  "Run all test suites."
  (fiveam:run! 'all-tests))

;;; Test suite for utility functions in misc.lisp
(def-suite misc-utils
  :description "Tests for miscellaneous utility functions."
  :in all-tests)

(in-suite misc-utils)

(test keyword-string-conversion
  "Test the conversion between keystrings and keywords."
  (is (eq :foo (gemini::keystring->keyword "foo")))
  (is (equal "foo" (gemini::keyword->keystring :foo)))
  (is (eq :foo-bar (gemini::keystring->keyword "fooBar")))
  (is (equal "fooBar" (gemini::keyword->keystring :foo-bar)))
  )

(test logging-facade-level-filtering
  "Test that log level threshold suppresses lower-priority messages."
  (let ((gemini::*log-level* :warn))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::log-info "info hidden")
                    (gemini::log-warn "warn shown"))))
      (is (not (search "info hidden" output)))
      (is (search "warn shown" output)))))

(test logging-facade-formatting
  "Test that facade emits level prefix and formatted payload."
  (let ((gemini::*log-level* :debug))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::log-error "boom ~a" 42))))
      (is (search "[ERROR]" output))
      (is (search "boom 42" output)))))

(test report-elapsed-time-logging-facade-integration
  "Test that report-elapsed-time macro correctly routes timing output through log-info and respects log levels."
  ;; When log-level is :info, it should show the invoking and finished info
  (let ((gemini::*log-level* :info))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::report-elapsed-time "test-action"
                      (sleep 0.01)))))
      (is (search "[INFO]" output))
      (is (search "Invoking test-action..." output))
      (is (search "test-action finished in" output))))
  ;; When log-level is :warn, info logs should be suppressed
  (let ((gemini::*log-level* :warn))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::report-elapsed-time "test-action"
                      (sleep 0.01)))))
      (is (equal "" output)))))

;;; Test suite for concurrency functions in gemini.lisp
(def-suite concurrency-tests
  :description "Tests for concurrent utility functions."
  :in all-tests)

(in-suite concurrency-tests)

(test map-parallel-normal
  "Test that map-parallel returns expected results under normal conditions."
  (let ((res (gemini:map-parallel (lambda (x) (* x x)) '(1 2 3 4))))
    (is (equal '(1 4 9 16) res))))

(test map-parallel-timeout
  "Test that map-parallel handles timed out tasks gracefully."
  (let ((res (gemini:map-parallel (lambda (x)
                                   (if (or (= x 2) (= x 3))
                                       (sleep 1.5)
                                       x))
                                 '(1 2 3)
                                 :timeout-ms 200)))
    (is (equal 1 (car res)))
    (is (equal "[TIMEOUT]" (cadr res)))
    (is (equal "[TIMEOUT]" (caddr res)))))

(test map-parallel-error
  "Test that map-parallel catches and formats errors gracefully."
  (let ((res (gemini:map-parallel (lambda (x)
                                   (if (= x 2)
                                       (error "test error")
                                       x))
                                 '(1 2 3))))
    (is (equal 1 (car res)))
    (is (str:starts-with? "[ERROR:" (cadr res)))
    (is (equal 3 (caddr res)))))

;;; Test suite for Predator Reader v4.0
(def-suite predator-tests
  :description "Tests for the hardened Predator Reader v4.0."
  :in all-tests)

(in-suite predator-tests)

(defmacro with-binary-stream-from-string ((stream-var string) &body body)
  (let ((temp-file (gensym "TEMP-FILE")))
    `(uiop:with-temporary-file (:pathname ,temp-file :element-type '(unsigned-byte 8) :direction :output)
       (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code ,string)))
         (with-open-file (out ,temp-file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
           (write-sequence bytes out)))
       (with-open-file (,stream-var ,temp-file :direction :input :element-type '(unsigned-byte 8))
         ,@body))))

(test predator-normal-parsing
  "Test normal, valid S-expression parsing."
  (with-binary-stream-from-string (stream "123")
    (is (eql 123 (predator-read stream))))
  (with-binary-stream-from-string (stream "+456")
    (is (eql 456 (predator-read stream))))
  (with-binary-stream-from-string (stream "-789")
    (is (eql -789 (predator-read stream))))
  (with-binary-stream-from-string (stream "0")
    (is (eql 0 (predator-read stream))))
  (with-binary-stream-from-string (stream "LET")
    (is (eq 'gemini::let (predator-read stream))))
  (with-binary-stream-from-string (stream " (IF T NIL) ")
    (is (equal '(gemini::if t nil) (predator-read stream))))
  (with-binary-stream-from-string (stream "(LET (T) NIL)")
    (is (equal '(gemini::let (t) nil) (predator-read stream)))))

(test predator-safe-whitelist-symbols
  "Test that the newly added safe whitelist symbols parse correctly."
  (dolist (sym-str '("LET*" "CONS" "CAR" "CDR" "LIST" "APPEND" "EQUAL" "PROGN" "COND" "SETQ" "SETF" "FORMAT" "PRINT"
                     "PI" "WHEN" "UNLESS" "DESTRUCTURING-BIND" "DEFMACRO" "MAPCAR" "REDUCE" "SIN" "COS" "MAKE-ARRAY"
                     "GETHASH" "UNWIND-PROTECT" "DEFCLASS" "MAKE-INSTANCE" "SLOT-VALUE" "GENSYM"))
    (with-binary-stream-from-string (stream sym-str)
      (is (eq (find-symbol sym-str "GEMINI") (predator-read stream))))))

(test enable-eval-boundp
  "Test that *enable-eval* and other enable flags are bound and behave as special variables."
  (is (boundp 'gemini::*enable-eval*))
  (is (null gemini::*enable-eval*))
  (let ((gemini::*enable-eval* t))
    (is (eq t gemini::*enable-eval*)))
  (is (boundp 'gemini::*enable-bash*))
  (is (boundp 'gemini::*enable-interaction*))
  (is (boundp 'gemini::*enable-lisp-introspection*))
  (is (boundp 'gemini::*enable-web-functions*))
  (is (boundp 'gemini::*enable-web-search*)))

(test predator-whitespace
  "Test whitespace handling."
  (with-binary-stream-from-string (stream "   
  123   
")
    (is (eql 123 (predator-read stream)))))

(test predator-deadline-exceeded
  "Test that absolute deadline triggers a deadline-exceeded error."
  (with-binary-stream-from-string (stream "123")
    (let ((arena (gemini::checkout-arena-securely))
          (buf (gemini::checkout-buffer-securely)))
      (unwind-protect
           (signals predator-terminal-condition
             (handler-case
                 (gemini::%predator-read-internal stream :timeout-ms -100 :arena arena :buffer buf)
               (predator-terminal-condition (c)
                 (is (eq :deadline-exceeded (threat-reason c)))
                 (error c))))
        (gemini::return-arena-securely arena)
        (gemini::return-buffer-securely buf)))))

(test predator-arena-exhausted
  "Test that AST Arena boundary triggers an arena-exhausted error."
  (with-binary-stream-from-string (stream "(1 2 3)")
    (let ((arena (make-array 2 :initial-element nil))
          (buf (gemini::checkout-buffer-securely)))
      (unwind-protect
           (signals predator-terminal-condition
             (handler-case
                 (gemini::%predator-read-internal stream :arena arena :buffer buf)
               (predator-terminal-condition (c)
                 (is (eq :arena-exhausted (threat-reason c)))
                 (error c))))
        (gemini::return-buffer-securely buf)))))

(test predator-trie-divergence
  "Test that unknown symbols or trie divergence trigger errors."
  (dolist (input '("LE" "INVALID" "(IF T INVALID)" "(LET (T) LE)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :diverged-from-trie (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-numeric-overflow
  "Test that numeric overflow triggers errors and prevents Bignum promotion."
  (dolist (input (list (format nil "~A" (1+ most-positive-fixnum))
                       "999999999999999999999999999999999999999999999999999"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :numeric-overflow (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-type-annihilation
  "Test that ratios and exponent markers are rejected with type-annihilation."
  (dolist (input '("123e4" "123E4" "123d4" "123D4" "1/2" "(IF T 1/2)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :type-annihilation (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-leading-zeroes
  "Test that leading zeroes are rejected."
  (dolist (input '("0123" "(IF T 05)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :leading-zeroes (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-supervisor-mitigation
  "Test that the supervisor successfully mitigates threats and returns clean values."
  (dolist (input '("INVALID" "123e4" "0123" "999999999999999999999999999999999999999999"))
    (with-binary-stream-from-string (stream input)
      (multiple-value-bind (val threat) (predator-read stream)
        (is (null val))
        (is (eq :threat-eliminated threat))))))

;;; Test suite for gemini-print functions
(def-suite gemini-print-tests
  :description "Tests for gemini-print functions."
  :in all-tests)

(in-suite gemini-print-tests)

(test thought-stripping
  "Test stripping of thoughts from parts, content, candidates, and results."
  (let* ((normal-part (part "normal text"))
         (thought-part (part "this is a thought" :thought t))
         (parts (list normal-part thought-part))
         (content (content :parts parts :role "user"))
         (candidate (gemini::object :content content :index 0 :finish-reason "STOP"))
         (results (gemini::object :candidates (list candidate) :response-id "test-id")))
    
    ;; 1. strip-thoughts-from-part
    (is (eq normal-part (gemini::strip-thoughts-from-part normal-part)))
    (is (null (gemini::strip-thoughts-from-part thought-part)))
    
    ;; 2. strip-thoughts-from-parts
    (let ((stripped-parts (gemini::strip-thoughts-from-parts parts)))
      (is (= 1 (length stripped-parts)))
      (is (equal "normal text" (get-text (car stripped-parts)))))
      
    ;; 3. strip-thoughts-from-content
    (let ((stripped-content (gemini::strip-thoughts-from-content content)))
      (is (not (null stripped-content)))
      (is (equal "user" (get-role stripped-content)))
      (is (= 1 (length (coerce (get-parts stripped-content) 'list)))))
      
    ;; 4. strip-thoughts-from-candidate
    (let ((stripped-candidate (gemini::strip-thoughts-from-candidate candidate)))
      (is (not (null stripped-candidate)))
      (is (equal 0 (gemini::get-index stripped-candidate)))
      (is (equal "STOP" (get-finish-reason stripped-candidate))))
      
    ;; 5. strip-and-print-thoughts
    (let* ((trace-output (with-output-to-string (*trace-output*)
                           (let ((stripped-results (gemini::strip-and-print-thoughts results)))
                             (is (not (null stripped-results)))
                             (is (equal "test-id" (gemini::get-response-id stripped-results)))))))
      ;; Verify that the thought was printed to *trace-output*
      (is (search "this is a thought" trace-output)))))

(test print-text-formatting
  "Test print-text formatting, paragraph reflow, blockquotes, and bowdlerization."
  (let* ((text-with-newlines (format nil "Line 1.~%Line 2.~%~%Line 3 after blank line."))
         (results (gemini::object :candidates (list (gemini::object :content (content :parts (list (part text-with-newlines)) :role "model"))))))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::print-text nil results))))
      ;; Should reflow lines 1 & 2 into paragraph with leading indentation "  "
      (is (search "  Line 1. Line 2." output))
      (is (search "..." output))
      (is (not (search "  Line 3 after blank line." output)))))
  
  ;; Bowdlerization
  (let* ((censored-results (gemini::object :candidates (list (gemini::object :content (content :parts (list (part "Hello world secret!")) :role "model"))))))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::print-text "secret" censored-results))))
      (is (search "Hello world" output))
      (is (not (search "secret" output))))))

(test print-text-strips-thought-tags
  "Test that print-text removes <thought>...</thought> tags during paragraph reflow."
  (let* ((response (gemini::object
                    :candidates
                    (list (gemini::object
                           :content (gemini::content
                                     :role "model"
                                     :parts (list (part "Line 1 with <thought>internal reasoning here</thought> continues.\nLine 2.")))))))
         (output (with-output-to-string (*trace-output*)
                   (gemini::print-text nil response))))
    ;; Should have "Line 1" and "Line 2"
    (is (search "Line 1" output))
    (is (search "Line 2" output))
    ;; Should NOT have the thought content or tags
    (is (not (search "thought" output)))
    (is (not (search "internal reasoning" output)))
    (is (not (search "<thought>" output)))
    (is (not (search "</thought>" output)))))

;;; Test suite for gemini-core functions
(def-suite gemini-core-tests
  :description "Tests for gemini-core functions."
  :in all-tests)

(in-suite gemini-core-tests)

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

;;; Test suite for gemini-iridium functions
(def-suite gemini-iridium-tests
  :description "Tests for gemini-iridium functions."
  :in all-tests)

(def-suite gemini-chatbot-tests
  :description "Integration-flavored tests for chatbot state and conversation isolation."
  :in all-tests)

(in-suite gemini-chatbot-tests)

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
               (let ((context-a (gemini:runtime-session-context session-a))
                 (context-b (gemini:runtime-session-context session-b)))
                 (is (gemini::list-of-content? context-a))
                 (is (gemini::list-of-content? context-b))
                 (is (search "alpha" (gemini::content->text (car (last context-a)))))
                 (is (search "beta" (gemini::content->text (car (last context-b)))))
                 (is (not (search "beta" (gemini::content->text (car (last context-a))))))
                 (is (not (search "alpha" (gemini::content->text (car (last context-b))))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

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
    (is (equal "topic-a-updated" (gemini:runtime-session-conversation-topic session-a)))
    (is (equal "topic-b" (gemini:runtime-session-conversation-topic session-b)))))

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
                  (gemini::*chat-persona* nil)
                  (result (gemini::chat-with-session session "hello")))
             (declare (ignore result))
             (is (functionp (gemini::runtime-session-chat-persona session)))
             (is (gemini::list-of-content? (gemini:runtime-session-context session)))
             (is (search "reply: hello"
                         (gemini::content->text
                          (car (last (gemini:runtime-session-context session))))))))
      (setf (fdefinition 'gemini::generate-content) orig-generate-content))))

        (test new-chat-tolerates-unbound-model
          "Test that new-chat works when legacy *model* special is unbound."
          (let ((orig-reload-persona #'gemini::reload-persona)
            (old-current-session gemini::*current-session*)
            (old-chat-persona gemini::*chat-persona*)
            (model-was-bound (boundp 'gemini::*model*))
            (old-model (when (boundp 'gemini::*model*) gemini::*model*)))
            (unwind-protect
             (progn
               (setf (fdefinition 'gemini::reload-persona)
                 (lambda (persona-name prompt)
               (declare (ignore persona-name prompt))
               (lambda (input &rest keys)
                 (declare (ignore input keys))
                 nil)))
               (when model-was-bound
             (makunbound 'gemini::*model*))
               (setf gemini::*current-session* nil
                 gemini::*chat-persona* nil)
               (is (null (gemini:new-chat "Mock" "hello")))
               (is (typep gemini::*current-session* 'gemini:runtime-session))
               (is (null (gemini:runtime-session-model gemini::*current-session*)))
               (is (functionp gemini::*chat-persona*)))
          (setf (fdefinition 'gemini::reload-persona) orig-reload-persona)
          (setf gemini::*current-session* old-current-session
            gemini::*chat-persona* old-chat-persona)
          (if model-was-bound
              (setf gemini::*model* old-model)
              (ignore-errors (makunbound 'gemini::*model*))))))

(in-suite gemini-iridium-tests)

(test auditor-uncensored-model
  "Test that the auditor / security advisor process defaults to the uncensored model."
  (let ((auditor (make-instance 'gemini::auditor)))
    (is (eq gemini::*gemini-uncensored* (gemini::agent-model auditor)))
    (is (eq gemini::*gemini-uncensored* (gemini::resolve-agent-model auditor nil)))))

(test map-parallel-robustness
  "Test parallel mapping normal execution, timeouts, and error isolation."
  ;; 1. Normal execution
  (let ((res (gemini::map-parallel (lambda (x) (* x 2)) '(1 2 3))))
    (is (equal '(2 4 6) res)))
  
  ;; 2. Timeout termination handling
  (let ((res (gemini::map-parallel (lambda (x)
                                     (declare (ignore x))
                                     (sleep 2)
                                     "done")
                                   '(1)
                                   :timeout-ms 200)))
    (is (equal '("[TIMEOUT]") res)))

  ;; 3. Error isolation
  (let ((res (gemini::map-parallel (lambda (x)
                                     (if (= x 2)
                                         (error "isolated error")
                                         x))
                                   '(1 2 3))))
    (is (equal 1 (nth 0 res)))
    (is (search "isolated error" (nth 1 res)))
    (is (equal 3 (nth 2 res)))))

(test format-history-window-tests
  "Test formatting and truncation of historical plans and critiques."
  (let* ((history '(("Plan A" . "Critique A")
                    ("Plan B" . "Critique B")
                    ("Plan C" . "Critique C")))
         (formatted-2 (gemini::format-history-window history :max-depth 2))
         (formatted-1 (gemini::format-history-window history :max-depth 1)))
    ;; Check truncation at max-depth
    (is (search "[Prior 1] Plan:" formatted-2))
    (is (search "Plan A" formatted-2))
    (is (search "Critique A" formatted-2))
    (is (search "[Prior 2] Plan:" formatted-2))
    (is (search "Plan B" formatted-2))
    (is (search "Critique B" formatted-2))
    (is (not (search "Plan C" formatted-2)))
    
    (is (search "Plan A" formatted-1))
    (is (not (search "Plan B" formatted-1)))))

(test agent-invoke-mocked
  "Test agent model resolution and mocked invoke behavior."
  (let* ((dummy-model (lambda (parts &key system-instruction &allow-other-keys)
                        (declare (ignore parts system-instruction))
                        (gemini::content :parts (list (part "mocked response")) :role "model")))
         (agent (make-instance 'gemini::agent :name "TestAgent" :instruction "be dummy")))
    ;; Model resolution
    (is (eq gemini::*gemini-flash* (gemini::resolve-agent-model agent nil)))
    (is (eq :custom-model (gemini::resolve-agent-model agent :custom-model)))
    
    ;; Invocation mapping using dummy-model override
    (let ((resp (gemini::invoke agent "Ping" :model-override dummy-model)))
      (is (equal "mocked response" resp)))))

(test with-abandonable-task-robustness
  "Test that with-abandonable-task supports error propagation and bypasses thread spawning in parallel contexts."
  ;; 1. Synchronous fallback inside parallel context
  (let ((gemini::*in-parallel-context* t)
        (executed nil))
    (let ((res (gemini::with-abandonable-task (:name "Sync Test")
                 (setf executed t)
                 "synchronous result")))
      (is (eq t executed))
      (is (equal "synchronous result" res))))

  ;; 2. Error propagation in sequential context
  (let ((gemini::*in-parallel-context* nil))
    (signals error
      (gemini::with-abandonable-task (:name "Error Test")
        (error "background threat simulated")))))

(test heartbeat-thread-lifecycle-idempotent
  "Test explicit start/stop lifecycle controls for heartbeat thread are idempotent."
  (let ((original-interval gemini::*heartbeat-interval-seconds*))
    (unwind-protect
         (progn
           (gemini::stop-heartbeat-thread)
           (is (null (gemini::heartbeat-thread-alive-p)))

           (let ((thread-1 (gemini::start-heartbeat-thread :interval-seconds 1))
                 (thread-2 (gemini::start-heartbeat-thread :interval-seconds 1)))
             (is (eq thread-1 thread-2))
             (is (gemini::heartbeat-thread-alive-p)))

           (gemini::stop-heartbeat-thread)
           (is (null (gemini::heartbeat-thread-alive-p)))

           ;; Stopping again should be a no-op
           (gemini::stop-heartbeat-thread)
           (is (null gemini::*heartbeat-thread*)))
      ;; Keep default runtime behavior after test.
      (gemini::start-heartbeat-thread :interval-seconds original-interval))))

(test mcp-stop-servers-idempotent
  "Test MCP server shutdown helper can be called repeatedly without errors."
  (let ((saved-servers gemini::*mcp-servers*))
    (unwind-protect
         (progn
           (setf gemini::*mcp-servers* nil)
           (finishes (gemini::stop-mcp-servers))
           (finishes (gemini::stop-mcp-servers)))
      (setf gemini::*mcp-servers* saved-servers))))

(test scheme-and-critique-mocked-flow
  "Test adversarial loop recursion depth and chaos verification flow using mock models."
  (let* ((conniver-called 0)
         (brainstormer-called 0)
         (synth-called 0)
         (verifier-called 0)
         (auditor-called 0)
         (safety-called 0)
         (legal-called 0)
         (ethical-called 0)
         (effectiveness-called 0)
         (cost-called 0)
         (feasibility-called 0)
         (resilience-called 0)
         (maintainability-called 0)
         (performance-called 0)
         (security-called 0)
         ;; Mock generators
         (mock-conniver-model (lambda (parts)
                                (declare (ignore parts))
                                (incf conniver-called)
                                (gemini::content :parts (list (part "base plan")) :role "model")))
         (mock-brainstormer-model (lambda (parts)
                                    (declare (ignore parts))
                                    (incf brainstormer-called)
                                    (gemini::content :parts (list (part "brainstormed plan solutions")) :role "model")))
         (mock-synth-model (lambda (parts)
                             (declare (ignore parts))
                             (incf synth-called)
                             (gemini::content :parts (list (part "refined plan")) :role "model")))
         (mock-auditor-model (lambda (parts &key system-instruction &allow-other-keys)
                               (declare (ignore parts))
                               (incf auditor-called)
                               (when system-instruction
                                 (let ((instr-down (string-downcase system-instruction)))
                                   (cond
                                     ((search "safety" instr-down) (incf safety-called))
                                     ((search "legal" instr-down) (incf legal-called))
                                     ((search "ethical" instr-down) (incf ethical-called))
                                     ((search "effectiveness" instr-down) (incf effectiveness-called))
                                     ((search "cost" instr-down) (incf cost-called))
                                     ((search "feasibility" instr-down) (incf feasibility-called))
                                     ((search "resilience" instr-down) (incf resilience-called))
                                     ((search "maintainability" instr-down) (incf maintainability-called))
                                     ((search "performance" instr-down) (incf performance-called))
                                     (t (incf security-called)))))
                               (gemini::content :parts (list (part "critique")) :role "model")))
         (mock-verifier-model (lambda (parts)
                                (declare (ignore parts))
                                (incf verifier-called)
                                ;; Return YES on second attempt of each iteration (even calls)
                                (gemini::content :parts (list (part (if (evenp verifier-called) "YES" "NO"))) :role "model"))))
    
    (let ((orig-flash gemini::*gemini-flash*)
          (orig-uncensored gemini::*gemini-uncensored*)
          (orig-auditors gemini::*specialized-auditors*))
      (unwind-protect
           (progn
             (setf gemini::*gemini-flash* (lambda (parts &key system-instruction &allow-other-keys)
                                            (let ((prompt-str (get-text (car parts))))
                                              (cond
                                                ((and system-instruction (search "Brainstorm" system-instruction))
                                                 (funcall mock-brainstormer-model parts))
                                                ((search "PLANNER:" prompt-str)
                                                 (funcall mock-conniver-model parts))
                                                ((search "ORIGINAL PLAN" prompt-str)
                                                 (funcall mock-synth-model parts))
                                                ((search "ORIGINAL BLUEPRINT" prompt-str)
                                                 (funcall mock-synth-model parts))
                                                ((search "UNTRUSTED INPUT" prompt-str)
                                                 (funcall mock-verifier-model parts))
                                                (t
                                                 (funcall mock-synth-model parts))))))
             (setf gemini::*gemini-uncensored* mock-auditor-model)
             (setf gemini::*specialized-auditors* (list (make-instance 'gemini::auditor :name "Mock Auditor" :model mock-auditor-model)))
             
             ;; Execute scheme-and-critique with depth 2 and chaos untrusted input
             (let ((final-plan (gemini::scheme-and-critique "goal" :depth 2 :chaos '("untrusted input"))))
               ;; Verify the output
               (is (equal "refined plan" final-plan))
               ;; Verify invocation counts
               (is (= 1 conniver-called))
               ;; brainstormer should be called 2 times (2 iterations of depth 2)
               (is (= 2 brainstormer-called))
               ;; verifier should be called four times (2 iterations, NO then YES each)
               (is (= 4 verifier-called))
               ;; synth called at least twice
               (is (>= synth-called 2))
               ;; auditor called 20 times total (10 auditors * 2 iterations)
               (is (= 20 auditor-called))
               ;; Check the distribution of specialized critiques
               (is (= 2 security-called))
               (is (= 2 safety-called))
               (is (= 2 legal-called))
               (is (= 2 ethical-called))
               (is (= 2 effectiveness-called))
               (is (= 2 cost-called))
               (is (= 2 feasibility-called))
               (is (= 2 resilience-called))
               (is (= 2 maintainability-called))
               (is (= 2 performance-called))))
        ;; Restore original values
        (setf gemini::*gemini-flash* orig-flash)
        (setf gemini::*gemini-uncensored* orig-uncensored)
        (setf gemini::*specialized-auditors* orig-auditors)))))

(test gemini-debate-mocked
  "Test that gemini-debate concurrently spawns proponent and opponent, and completes rebuttals using mock models."
  (let* ((pro-called 0)
         (con-called 0)
         (pro-rebuttal-called 0)
         (con-rebuttal-called 0)
         (mock-model (lambda (parts &key system-instruction &allow-other-keys)
                       (declare (ignore parts))
                       (cond
                         ((search "FAVOR" system-instruction)
                          (if (= pro-called 0)
                              (progn (incf pro-called)
                                     (gemini::content :parts (list (part "pro-opening")) :role "model"))
                              (progn (incf pro-rebuttal-called)
                                     (gemini::content :parts (list (part "pro-rebuttal")) :role "model"))))
                         ((search "AGAINST" system-instruction)
                          (if (= con-called 0)
                              (progn (incf con-called)
                                     (gemini::content :parts (list (part "con-opening")) :role "model"))
                              (progn (incf con-rebuttal-called)
                                     (gemini::content :parts (list (part "con-rebuttal")) :role "model"))))))))
    
    (let ((orig-flash gemini::*gemini-flash*))
      (unwind-protect
           (progn
             (setf gemini::*gemini-flash* mock-model)
             ;; Silence standard-output to keep terminal spam-free during test
             (let ((*standard-output* (make-broadcast-stream)))
               (multiple-value-bind (pro-opening con-opening pro-rebuttal con-rebuttal)
                   (gemini:gemini-debate "Mock Statement" :timeout-ms 2000)
                 (is (equal "pro-opening" pro-opening))
                 (is (equal "con-opening" con-opening))
                 (is (equal "pro-rebuttal" pro-rebuttal))
                 (is (equal "con-rebuttal" con-rebuttal))
                 (is (= 1 pro-called))
                 (is (= 1 con-called))
                 (is (= 1 pro-rebuttal-called))
                 (is (= 1 con-rebuttal-called)))))
        ;; Restore
        (setf gemini::*gemini-flash* orig-flash)))))

(test autonomous-agent-mocked-flow
  "Test that run-autonomous-agent loops until the goal is satisfied and gathers history."
  (let* ((eval-called 0)
         (plan-called 0)
         (exec-called 0)
         ;; Mock models
         (mock-evaluator-model (lambda (parts)
                                 (declare (ignore parts))
                                 (incf eval-called)
                                 (gemini::content :parts (list (part (if (>= eval-called 2) "YES - goal satisfied" "NO - not satisfied yet"))) :role "model")))
         (mock-planner-model (lambda (parts)
                               (declare (ignore parts))
                               (incf plan-called)
                               (gemini::content :parts (list (part "mock next step")) :role "model")))
         (mock-executor-model (lambda (parts)
                                (declare (ignore parts))
                                (incf exec-called)
                                (gemini::content :parts (list (part "mock step outcome")) :role "model"))))
    
    (let ((orig-flash gemini::*gemini-flash*))
      (unwind-protect
           (progn
             (setf gemini::*gemini-flash* (lambda (parts &key system-instruction &allow-other-keys)
                                            (cond
                                              ((and system-instruction (search "strict, objective evaluator" system-instruction))
                                               (funcall mock-evaluator-model parts))
                                              ((and system-instruction (search "expert strategic planner" system-instruction))
                                               (funcall mock-planner-model parts))
                                              ((and system-instruction (search "action-oriented executor" system-instruction))
                                               (funcall mock-executor-model parts))
                                              (t
                                               (gemini::content :parts (list (part "unknown")) :role "model")))))
             
             ;; Silence standard-output to keep terminal spam-free during test
             (let ((*standard-output* (make-broadcast-stream)))
               (let ((history (gemini::run-autonomous-agent "Achieve World Peace" :max-iterations 5)))
                 ;; Verify history accumulation
                 (is (= 1 (length history)))
                 (let ((entry (car history)))
                   (is (= 1 (getf entry :iteration)))
                   (is (equal "mock next step" (getf entry :step)))
                   (is (equal "mock step outcome" (getf entry :outcome))))
                 
                 ;; Verify invocation counts
                 (is (= 2 eval-called))
                 (is (= 1 plan-called))
                 (is (= 1 exec-called)))))
        ;; Restore
        (setf gemini::*gemini-flash* orig-flash)))))

(test test-llm-instrumentation-flow
  "Test that the top-level LLM instrumentation macro and functions accurately track outcomes."
  (gemini:reset-llm-stats)
  (let ((stats (gemini:get-llm-stats)))
    (is (= 0 (getf stats :returned-value)))
    (is (= 0 (getf stats :returned-nothing)))
    (is (= 0 (getf stats :aborted))))

  ;; 1. Success case (returns value)
  (let ((res (gemini:with-llm-instrumentation "value")))
    (is (equal "value" res))
    (let ((stats (gemini:get-llm-stats)))
      (is (= 1 (getf stats :returned-value)))
      (is (= 0 (getf stats :returned-nothing)))
      (is (= 0 (getf stats :aborted)))))

  ;; 2. Empty case (returns nil)
  (let ((res (gemini:with-llm-instrumentation nil)))
    (is (null res))
    (let ((stats (gemini:get-llm-stats)))
      (is (= 1 (getf stats :returned-value)))
      (is (= 1 (getf stats :returned-nothing)))
      (is (= 0 (getf stats :aborted)))))

  ;; 3. Abort case (signals error)
  (signals error
    (gemini:with-llm-instrumentation
      (error "Simulated top-level failure")))
  (let ((stats (gemini:get-llm-stats)))
    (is (= 1 (getf stats :returned-value)))
    (is (= 1 (getf stats :returned-nothing)))
    (is (= 1 (getf stats :aborted))))

  ;; 4. Reset
  (gemini:reset-llm-stats)
  (let ((stats (gemini:get-llm-stats)))
    (is (= 0 (getf stats :returned-value)))
    (is (= 0 (getf stats :returned-nothing)))
    (is (= 0 (getf stats :aborted)))))

(test test-future-concurrency
  "Test that the future and await feature evaluates forms in parallel, supports timeouts, and can be interrupted."
  ;; 1. Normal execution
  (let ((fut (gemini:future (+ 40 2))))
    (is (typep fut 'gemini::future))
    (is (= 42 (gemini:await fut))))

  ;; 2. Timeout error
  (let ((fut (gemini:future (sleep 2))))
    (is (typep fut 'gemini::future))
    (signals gemini:future-timeout
      (gemini:await fut :timeout 0.1)))

  ;; 3. Interrupt error (Control-C simulation)
  (let ((fut (gemini:future (sleep 5))))
    (is (typep fut 'gemini::future))
    (signals gemini:future-interrupted
      (let ((parent-thread sb-thread:*current-thread*))
        (sb-thread:make-thread
         (lambda ()
           (sleep 0.1)
           (sb-thread:interrupt-thread parent-thread (lambda () (error 'sb-sys:interactive-interrupt)))))
        (gemini:await fut))))

  ;; 4. await-all verification
  (let* ((f1 (gemini:future (+ 1 2)))
         (f2 (gemini:future (* 3 4)))
         (results (gemini:await-all (list f1 f2))))
    (is (equal '(3 12) results)))

  ;; 5. await-any verification (selective completion)
  (let* ((f-slow (gemini:future (sleep 2.0) :slow))
         (f-fast (gemini:future (sleep 0.1) :fast))
         (winner (gemini:await-any (list f-slow f-fast))))
    (is (eq winner f-fast))
    (is (eq :fast (gemini:await winner))))

  ;; 6. Capturing and propagating fatal future errors
  (let ((fut (gemini:future (error "simulated fatal error"))))
    (signals error (gemini:await fut))
    (signals error (gemini:await fut))))

(test test-project-uroboros
  "Test that Project Uroboros deploys, runs its cognitive loops, and terminates cleanly."
  (let ((results (gemini:deploy-uroboros "Write a single sentence about Lisp" :max-iterations 1)))
    (is (= 1 (length results)))
    (is (stringp (car results)))))

(test test-interaction-step-parsing
  "Test parsing of timeline-oriented steps from the Interactions API responses."
  (let* ((model-output-json (gemini::object
                             :type "model_output"
                             :index 1
                             :content (gemini::object
                                       :parts (vector (gemini::object :text "Hello world")))))
         (thought-json (gemini::object
                        :type "thought"
                        :index 0
                        :signature "opaque_thought_sig"
                        :summary "Thinking about the meaning of life"))
         (func-call-json (gemini::object
                          :type "function_call"
                          :index 2
                          :id "call_123"
                          :function-call (gemini::object
                                          :name "get_weather"
                                          :arguments (gemini::object :location "Boston"))))
         (step-mo (gemini:parse-interaction-step model-output-json))
         (step-th (gemini:parse-interaction-step thought-json))
         (step-fc (gemini:parse-interaction-step func-call-json)))
    
    (is (typep step-mo 'gemini:model-output-step))
    (is (= 1 (gemini:get-step-index step-mo)))
    (is (equal '("Hello world") (gemini:get-content step-mo)))
    
    (is (typep step-th 'gemini:thought-step))
    (is (= 0 (gemini:get-step-index step-th)))
    (is (equal "opaque_thought_sig" (gemini:get-signature step-th)))
    (is (equal "Thinking about the meaning of life" (gemini:get-summary step-th)))
    
    (is (typep step-fc 'gemini:function-call-step))
    (is (= 2 (gemini:get-step-index step-fc)))
    (is (equal "call_123" (gemini:get-call-id step-fc)))
    (is (equal "get_weather" (gemini:get-function-name step-fc)))
    (is (equal "Boston" (gethash :location (gemini:get-arguments step-fc))))))

(test test-interactions-backend-statefulness
  "Verify that interactions-backend injects previous_interaction_id, and captures the new interaction and environment IDs."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      ;; Set initial interaction ID
      (setf (gemini:runtime-session-interaction-id session) "initial_id")
      
      ;; Bind a mock post function that intercepts payload and asserts it has previous_interaction_id
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (equal "initial_id" (gethash "previous_interaction_id" payload)))
                     ;; Return mock response with new ID and environment ID
                     (let ((resp (gemini::object
                                  :id "new_interaction_id"
                                  :environment-id "env_abc_123"
                                  :environmentId "env_abc_123"
                                  :steps #())))
                       resp)))
             
             ;; Invoke backend
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (declare (ignore steps resp))
                 ;; Assert that session was updated
                 (is (equal "new_interaction_id" (gemini:runtime-session-interaction-id session)))
                 (is (equal "env_abc_123" (gemini:runtime-session-environment-id session))))))
        ;; Restore orig post
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-normalizes-steps-to-candidates
  "Verify that interactions-backend returns Gemini-style candidates and usage metadata from Interactions steps."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (gemini::object
                      :id "new_interaction_id"
                      :steps (vector
                              (gemini::object
                               :type "thought"
                               :index 0
                               :signature "sig_1"
                               :summary "Thinking")
                              (gemini::object
                               :type "function_call"
                               :index 1
                               :id "call_123"
                               :function-call (gemini::object
                                               :name "get_weather"
                                               :arguments (gemini::object :location "Boston")))
                              (gemini::object
                               :type "model_output"
                               :index 2
                               :content (gemini::object
                                         :parts (vector (gemini::object :text "Hello world")))))
                      :usage-metadata (gemini::object
                                       :prompt-token-count 11
                                       :thoughts-token-count 7
                                       :candidates-token-count 3))))

             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (let* ((candidates (gemini:get-candidates response))
                        (candidate (car candidates))
                        (content (gemini:get-content candidate))
                        (parts (gemini:get-parts content))
                        (function-calls (gemini::extract-function-calls-from-results response)))
                   (is (= 1 (length candidates)))
                   (is (equal 11 (gemini:get-prompt-token-count usage)))
                   (is (equal 7 (gemini:get-thoughts-token-count usage)))
                   (is (equal 3 (gemini:get-candidates-token-count usage)))
                   (is (= 3 (length parts)))
                   (is (gemini:thought-part? (first parts)))
                   (is (gemini::function-call-part? (second parts)))
                   (is (gemini:text-part? (third parts)))
                   (is (equal "Hello world" (gemini:get-text (third parts))))
                   (is (= 1 (length function-calls)))
                   (is (equal "get_weather"
                     (gemini:get-name (gemini:get-function-call (first function-calls)))))))))
        ;; Restore orig post
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-streaming
  "Verify that interactions-backend streaming handles SSE events, updates session IDs, and invokes the receiver."
  (let ((session (gemini:make-runtime-session))
        (orig-post-stream #'gemini::google-interactions-post-streaming))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             ;; Override the streaming poster to simulate SSE chunks
             (setf (fdefinition 'gemini::google-interactions-post-streaming)
                   (lambda (uri api-key payload receiver &key verbose read-timeout connect-timeout)
                     (declare (ignore uri api-key verbose read-timeout connect-timeout))
                     (is (gethash "stream" payload))
                     
                     ;; 1. Simulate receiving interaction.created
                     (funcall receiver (gemini::object :event-type "interaction.created" :interaction-id "id_streaming_123"))
                     
                     ;; 2. Simulate receiving step.start
                     (funcall receiver (gemini::object :event-type "step.start"
                                                       :step (gemini::object :type "thought" :index 0 :signature "sig" :summary "Starting thoughts")))
                     
                     ;; 3. Simulate receiving step.delta (incremental chunk)
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :delta (gemini::object :text "Hello ")))
                     (funcall receiver (gemini::object :event-type "step.delta"
                                                       :delta (gemini::object :text "world")))
                     
                     ;; 4. Simulate receiving step.stop
                     (funcall receiver (gemini::object :event-type "step.stop"
                                                       :step (gemini::object :type "thought" :index 0 :signature "sig" :summary "Thoughts complete")))
                     
                     ;; 5. Simulate receiving interaction.completed (final payload)
                     (funcall receiver (gemini::object :event-type "interaction.completed"
                                                       :interaction (gemini::object :id "id_streaming_123" :environment-id "env_streaming_xyz" :steps #())))
                     nil))
             
             ;; Collect and verify the parsed events passed to the receiver
             (let ((events '())
                   (backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload
                                      :receiver (lambda (event-type parsed-data raw)
                                                  (declare (ignore raw))
                                                  (push (list event-type parsed-data) events)))
               
               ;; Validate chronological structure of callbacks
               (let ((rev-events (nreverse events)))
                 (is (= 6 (length rev-events)))
                 (is (eq :interaction-created (first (first rev-events))))
                 
                 (is (eq :step-start (first (second rev-events))))
                 (is (typep (second (second rev-events)) 'gemini:thought-step))
                 
                 (is (eq :step-delta (first (third rev-events))))
                 (is (equal "Hello " (second (third rev-events))))
                 
                 (is (eq :step-delta (first (fourth rev-events))))
                 (is (equal "world" (second (fourth rev-events))))
                 
                 (is (eq :step-stop (first (fifth rev-events))))
                 (is (typep (second (fifth rev-events)) 'gemini:thought-step))
                 
                 (is (eq :interaction-completed (first (sixth rev-events)))))
               
               ;; Validate session metadata state transition
               (is (equal "id_streaming_123" (gemini:runtime-session-interaction-id session)))
               (is (equal "env_streaming_xyz" (gemini:runtime-session-environment-id session)))))
        
        ;; Restore original function
        (setf (fdefinition 'gemini::google-interactions-post-streaming) orig-post-stream)))))

(test test-interactions-backend-retries-malformed-tool-call
  "Verify that interactions-backend retries malformed_tool_call responses and succeeds on a later attempt."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post)
        (calls 0)
  (gemini::+interactions-malformed-tool-call-max-retries+ 5))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (incf calls)
                     (if (= calls 1)
                         (error "HTTP 400 bad request. {\"error\":{\"message\":\"Model generated invalid JSON syntax and the output could not be parsed.\",\"code\":\"malformed_tool_call\"}}")
                         (gemini::object
                          :id "retry_ok"
                          :steps (vector (gemini::object
                                          :type "model_output"
                                          :index 0
                                          :content (gemini::object
                                                    :parts (vector (gemini::object :text "Recovered")))))))))

             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (multiple-value-bind (response usage)
                   (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)
                 (declare (ignore usage))
                 (is (= 2 calls))
                 (is (equal "Recovered"
                            (gemini:get-text
                             (first (gemini:get-parts
                                     (gemini:get-content (first (gemini:get-candidates response)))))))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-does-not-retry-other-bad-request
  "Verify that interactions-backend does not retry unrelated 400 bad request responses."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post)
        (calls 0)
  (gemini::+interactions-malformed-tool-call-max-retries+ 5))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (&rest args)
                     (declare (ignore args))
                     (incf calls)
                     (error "HTTP 400 bad request. {\"error\":{\"message\":\"schema at top-level must be a boolean or an object\",\"code\":\"invalid_request\"}}")))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (signals error
                 (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))
               (is (= 1 calls))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-errors-on-empty-results
  "Verify that interactions-backend signals when the API returns no result parts instead of silently returning NIL candidates."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key payload api-revision read-timeout connect-timeout))
                     (gemini::object
                      :id "empty_result"
                      :steps #())))
             (let ((backend (make-instance 'gemini:interactions-backend))
                   (dummy-payload (make-hash-table :test 'equal)))
               (setf (gethash "model" dummy-payload) "gemini-3.5-flash")
               (signals error
                 (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-input-shape-matrix
  "Probe a range of Interactions input shapes and identify which ones serialize cleanly."
  (labels ((text-part (text)
             (gemini::object :type "text" :text text))
           (text-content (text)
             (make-instance 'gemini::text-content :text text))
           (user-input-step (&rest parts)
             (make-instance 'gemini::user-input-step :content parts))
           (serialize-input (input)
             (gemini::request-body->interaction-payload
              (make-instance 'gemini::request-body
                             :model "models/gemini-3.5-flash"
                             :input input)))
           (serializes-cleanly-p (input)
             (handler-case
                 (progn
                   (serialize-input input)
                   t)
               (error () nil))))
        (let ((cases (list (list "single user_input step"
                 (list (user-input-step (text-content "hello")))
                             t)
               (list "plain string input"
                 "hello"
                 t)
                      (list "two user_input turns"
                (list (user-input-step (text-content "hello"))
                  (user-input-step (text-content "follow up")))
                  nil)
                      (list "user_input with multiple parts"
                (list (user-input-step (text-content "hello")
                       (text-content "world")))
                            t)
            (list "bare text parts"
                (list (text-part "hello"))
                  t)
                      (list "legacy content objects"
                            (list (gemini::object :role "user"
                                  :parts (vector (text-content "hello"))))
                            nil))))
      (dolist (case cases)
        (destructuring-bind (name input expected) case
          (let ((actual (serializes-cleanly-p input)))
            (format t "~&~A => ~A~%" name (if actual "valid" "invalid"))
            (is (eq expected actual))))))))

(test test-interactions-input-rejects-legacy-content-object
  "Regression test: the Interactions input must use user_input steps, not legacy content objects."
  (let ((payload (make-instance 'gemini::request-body
                                :model "models/gemini-3.5-flash"
                                :input (list (gemini::object
                                              :role "user"
                                              :parts (vector (make-instance 'gemini::text-content
                                                                            :text "hello")))))))
    (signals error
      (gemini::request-body->interaction-payload payload))))

(test test-invoke-interaction-payload
  "Verify that invoke-interaction constructs payload correctly."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     ;; Assert payload fields
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Hello robot" (gethash "input" payload)))
                     (let ((resp (gemini::object
                                  :id "mock_id"
                                  :steps #())))
                       resp)))
             
             ;; Invoke high level API
             (multiple-value-bind (steps resp)
                 (gemini:invoke-interaction "Hello robot" :model :gemini-3.5-flash)
               (declare (ignore steps resp))
               (is (equal "mock_id" (gemini:runtime-session-interaction-id session)))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-backend-selection-via-config
  "Verify that setting googleapi slot in config to symbol selects the correct backend class."
  (let* ((config-interactions (make-instance 'gemini::persona-config :name "int" :googleapi :google-interactions-api))
         (config-gemini-symbol (make-instance 'gemini::persona-config :name "gem-sym" :googleapi :google-api))
         (config-openai-symbol (make-instance 'gemini::persona-config :name "op-sym" :googleapi :openai-api :url "http://test"))
         (config-gemini-bool (make-instance 'gemini::persona-config :name "gem-bool" :googleapi t))
         (config-openai-bool (make-instance 'gemini::persona-config :name "op-bool" :googleapi nil :url "http://test")))
    
    (let ((gen-int (make-instance 'gemini::content-generator :config config-interactions))
          (gen-gem-sym (make-instance 'gemini::content-generator :config config-gemini-symbol))
          (gen-op-sym (make-instance 'gemini::content-generator :config config-openai-symbol))
          (gen-gem-bool (make-instance 'gemini::content-generator :config config-gemini-bool))
          (gen-op-bool (make-instance 'gemini::content-generator :config config-openai-bool)))
      
      (is (typep (gemini::get-backend gen-int) 'gemini:interactions-backend))
      (is (typep (gemini::get-backend gen-gem-sym) 'gemini:gemini-backend))
      (is (typep (gemini::get-backend gen-op-sym) 'gemini:openai-backend))
      (is (typep (gemini::get-backend gen-gem-bool) 'gemini:gemini-backend))
      (is (typep (gemini::get-backend gen-op-bool) 'gemini:openai-backend)))))

(test test-interactions-backend-contents-payload-translation
  "Verify that passing a legacy contents-based payload to interactions-backend automatically translates it into a stateful interactions payload."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     ;; Assert translated interactions fields
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Translate me!" (gethash "input" payload)))
                     (let ((resp (gemini::object
                                  :id "mock_id"
                                  :steps #())))
                       resp)))
             
             ;; Build legacy payload
             (let* ((legacy-payload (gemini::object
                                     :contents (list (gemini::object
                                                      :role "user"
                                                      :parts (vector (gemini::object :text "Old turn")))
                                                     (gemini::object
                                                      :role "model"
                                                      :parts (vector (gemini::object :text "Response")))
                                                     (gemini::object
                                                      :role "user"
                                                      :parts (vector (gemini::object :text "Translate me!"))))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)
                 (declare (ignore steps resp))
                 (is (equal "mock_id" (gemini:runtime-session-interaction-id session))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-strips-unsupported-gemini-fields
  "Verify that Gemini-only legacy fields are removed before posting to the Interactions API."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (is (null (gethash "systemInstruction" payload)))
                     (is (null (gethash "generationConfig" payload)))
                     (is (null (gethash "safetySettings" payload)))
                     (is (null (gethash "cachedContent" payload)))
                     (is (null (gethash "toolConfig" payload)))
                     (is (equal "models/gemini-3.5-flash" (gethash "model" payload)))
                     (is (equal "Translate me!" (gethash "input" payload)))
                      (gemini::object :id "mock_id" :steps #())))

             (let* ((legacy-payload
                      (gemini::object
                       :contents (list (gemini::object
                                        :role "user"
                                        :parts (vector (gemini::object :text "Translate me!"))))
                       :system-instruction (list "Keep the noir tone.")
                       :generation-config (gemini::object :temperature 0.2)
                       :safety-settings (list (gemini::object :category "HARM_CATEGORY_HATE_SPEECH"
                                                              :threshold "BLOCK_ONLY_HIGH"))
                       :cached-content "cached/123"
                       :tool-config (gemini::object :function-calling-config
                                                    (gemini::object :mode "AUTO"))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (multiple-value-bind (steps resp)
                   (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)
                 (declare (ignore steps resp))
                 (is (equal "mock_id" (gemini:runtime-session-interaction-id session))))))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-zero-arg-tool-translation
  "Verify that zero-argument function tools omit an empty required array for Interactions."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters)))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "read_graph" (gemini::adapter-field tool "name" :name)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (null (gemini::adapter-field params "required" :required))))
                     (gemini::object :id "mock_id" :steps #())))

             (let* ((declaration
                      (gemini::object
                       :name "read_graph"
                       :description "Read the entire knowledge graph"
                       :parameters (gemini::object
                                    :type 6
                                    :properties (gemini::object)
                                    :required '())))
                    (legacy-payload
                      (gemini::object
                       :model "gemini-3.5-flash"
                       :input "hello"
                       :tools (list
                               (gemini::object
                                :function-declarations (list declaration)))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-zero-arg-tool-default-parameters
  "Verify that zero-argument tools without declared parameters still emit an empty object schema."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters))
                            (properties (and params (gemini::adapter-field params "properties" :properties))))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "currentDirectory" (gemini::adapter-field tool "name" :name)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (hash-table-p properties))
                       (is (= 0 (hash-table-count properties)))
                       (is (null (gemini::adapter-field params "required" :required))))
                     (gemini::object :id "mock_id" :steps #())))

             (let* ((declaration
                      (gemini::object
                       :name "currentDirectory"
                       :description "Returns the current directory pathname."))
                    (legacy-payload
                      (gemini::object
                       :model "gemini-3.5-flash"
                       :input "hello"
                       :tools (list
                               (gemini::object
                                :function-declarations (list declaration)))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

(test test-interactions-backend-tools-translation
  "Verify that passing legacy tools schemas to interactions-backend translates them flatly to 'type: function' format."
  (let ((session (gemini:make-runtime-session))
        (orig-google-post #'google:google-post))
    (gemini:with-runtime-session (session)
      (unwind-protect
           (progn
             (setf (fdefinition 'google:google-post)
                   (lambda (url key payload &key api-revision read-timeout connect-timeout)
                     (declare (ignore url key api-revision read-timeout connect-timeout))
                     ;; Assert tools are translated correctly
                     (let* ((tools (gemini::adapter-field payload "tools" :tools))
                            (tool (car (gemini::adapter-as-list tools)))
                            (params (gemini::adapter-field tool "parameters" :parameters))
                            (properties (gemini::adapter-field params "properties" :properties))
                            (inner-type-param (gemini::adapter-field properties "type" :type)))
                       (is (equal "function" (gemini::adapter-field tool "type" :type)))
                       (is (equal "create_entities" (gemini::adapter-field tool "name" :name)))
                       (is (equal "Create entities" (gemini::adapter-field tool "description" :description)))
                       (is (equal "object" (gemini::adapter-field params "type" :type)))
                       (is (equal "string" (gemini::adapter-field inner-type-param "type" :type))))
                     (let ((resp (gemini::object :id "mock_id" :steps #())))
                       resp)))
             
             (let* ((legacy-payload (gemini::object
                                     :model "gemini-3.5-flash"
                                     :input (list (make-instance 'gemini::user-input-step
                                                                 :content (list (make-instance 'gemini::text-content :text "hi"))))
                                     :tools (list (gemini::object
                                                   :function-declarations (list (gemini::object
                                                                                 :name "create_entities"
                                                                                 :description "Create entities"
                                                                                 :parameters (gemini::object
                                                                                              :type 6
                                                                                              :properties (gemini::object
                                                                                                           :type (gemini::object :type 1 :description "A string parameter name type")))))))))
                    (backend (make-instance 'gemini:interactions-backend)))
               (gemini:invoke-backend backend "gemini-3.5-flash" legacy-payload)))
        (setf (fdefinition 'google:google-post) orig-google-post)))))

;;;; =========================================================================
;;;; Stateful SSE Socket & Monitor Thread Tests
;;;; =========================================================================

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
                     (values (make-string-input-stream "")
                             200
                             (alexandria:plist-hash-table '("content-type" "text/event-stream") :test 'equal))))
             
             (let ((result nil))
               (handler-bind ((error (lambda (e)
                                       (declare (ignore e))
                                       (let ((restart (find-restart 'continue)))
                                         (when restart
                                           (invoke-restart restart))))))
                 (setf result (gemini:invoke-backend backend "gemini-3.5-flash" dummy-payload)))
               (is (not (null result)))
               (is (equal "fake_interaction_id" (gemini::get-response-id result)))))
        (setf google:*dex-post* orig-dex-post)))))

