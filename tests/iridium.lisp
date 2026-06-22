;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

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
