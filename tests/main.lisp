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
      (is (search "  Line 3 after blank line." output))))
  
  ;; Bowdlerization
  (let* ((censored-results (gemini::object :candidates (list (gemini::object :content (content :parts (list (part "Hello world secret!")) :role "model"))))))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::print-text "secret" censored-results))))
      (is (search "Hello world" output))
      (is (not (search "secret" output))))))

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

;;; Test suite for gemini-iridium functions
(def-suite gemini-iridium-tests
  :description "Tests for gemini-iridium functions."
  :in all-tests)

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

(test scheme-and-critique-mocked-flow
  "Test adversarial loop recursion depth and chaos verification flow using mock models."
  (let* ((conniver-called 0)
         (synth-called 0)
         (verifier-called 0)
         (auditor-called 0)
         ;; Mock generators
         (mock-conniver-model (lambda (parts)
                                (declare (ignore parts))
                                (incf conniver-called)
                                (gemini::content :parts (list (part "base plan")) :role "model")))
         (mock-synth-model (lambda (parts)
                             (declare (ignore parts))
                             (incf synth-called)
                             (gemini::content :parts (list (part "refined plan")) :role "model")))
         (mock-auditor-model (lambda (parts &key &allow-other-keys)
                               (declare (ignore parts))
                               (incf auditor-called)
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
             (setf gemini::*gemini-flash* (lambda (parts &key &allow-other-keys)
                                            (let ((prompt-str (get-text (car parts))))
                                              (cond
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
               ;; verifier should be called four times (2 iterations, NO then YES each)
               (is (= 4 verifier-called))
               ;; synth called at least twice
               (is (>= synth-called 2))
               ;; auditor called 2 times (one per iteration of depth 2)
               (is (= 2 auditor-called))))
        ;; Restore original values
        (setf gemini::*gemini-flash* orig-flash)
        (setf gemini::*gemini-uncensored* orig-uncensored)
        (setf gemini::*specialized-auditors* orig-auditors)))))
