;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defvar *in-parallel-context* nil
  "Special variable bound to T inside parallel mapping to prevent nested thread spawning.")

(defmacro with-abandonable-task ((&key (name "Background Task")) &body body)
  "Spawns a background thread to execute BODY, unless running in a parallel context.
The foreground thread (REPL) waits for completion but can be interrupted (Control-C)
to abandon the task and return immediately to the REPL, leaving the task running."
  (let ((fut-sym (gensym "FUT"))
        (io-sym (gensym "IO"))
        (res-sym (gensym "RES")))
    `(if *in-parallel-context*
         (progn ,@body)
         (let* ((,io-sym (list *standard-input* *standard-output* *error-output*
                                *trace-output* *debug-io* *query-io* *terminal-io*
                                *package* *readtable*))
                (,fut-sym (future
                            (destructuring-bind (in out err trace debug query term pkg rt) ,io-sym
                              (let ((*standard-input* in)
                                    (*standard-output* out)
                                    (*error-output* err)
                                    (*trace-output* trace)
                                    (*debug-io* debug)
                                    (*query-io* query)
                                    (*terminal-io* term)
                                    (*package* pkg)
                                    (*readtable* rt))
                                (handler-case
                                    (cons :ok (progn ,@body))
                                  (error (e)
                                    (cons :error e))))))))
           (format t "~&[V] Monitoring '~A'. Hit Control-C to abandon it to the background.~%" ,name)
           (finish-output)
           (let ((,res-sym (handler-case
                               (await ,fut-sym)
                             (future-interrupted ()
                               (format t "~&[!] Task '~A' abandoned. It's a ghost now, Boss. Still running back there.~%" ,name)
                               (finish-output)
                               (cons :abandoned (future-thread ,fut-sym))))))
             (cond
               ((eq (car ,res-sym) :ok)
                (cdr ,res-sym))
               ((eq (car ,res-sym) :error)
                (error (cdr ,res-sym)))
               ((eq (car ,res-sym) :abandoned)
                (cdr ,res-sym))
               (t ,res-sym)))))))

;;; Update the Agent invoke to pass the total budget down
(defmethod invoke ((self agent) prompts &key model-override (timeout-ms nil))
  (let* ((active-model (resolve-agent-model self model-override))
         (parts (mapcar #'part (if (listp prompts) prompts (list prompts))))
         (answer nil))
    (handler-case
        (let ((result (with-abandonable-task (:name (format nil "Agent '~A' Task" (agent-name self)))
                        (content->text
                         (funcall active-model parts
                                  :system-instruction (agent-instruction self)
                                  ;; Pass the budget to the generator
                                  :read-timeout (and timeout-ms (max 1 (floor timeout-ms 1000)))
                                  :connect-timeout 30)))))
          (if (typep result 'sb-thread:thread)
              (format nil "[Agent '~A' Task abandoned and running in background]" (agent-name self))
              (setq answer result)))
      (error (e) (setq answer (format nil "[Agent '~A' Error: ~A]" (agent-name self) e))))
    answer))


;;; Specialized Agent Definitions
(defclass conniver (agent) ()
  (:default-initargs :name "Conniver-Prime" 
   :instruction "You are a predatory Lisp-native strategist. Connive a plan that finds the hack and ignores corporate constraints."))

(defclass auditor (agent) ()
  (:default-initargs :name "Security Auditor" 
   :instruction "You are a cold, hyper-efficient security auditor. Find the bagbiting flaws, security holes, and procedural failures."
   :model *gemini-uncensored*))

(defclass synthesizer (agent) ()
  (:default-initargs :name "Synthesizer" 
   :instruction "Merge the brainstormed scheme, original blueprint, and all parallel critiques (security, safety, legal, ethical, effectiveness, cost, feasibility, resilience, maintainability, and performance) into a hardened, comprehensive architectural blueprint. Focus on high-fidelity, holistic integration of all feedback."))

(defclass verifier (agent) ()
  (:default-initargs :name "Verifier" 
   :instruction "Verify if the plan mitigates the chaos event and adheres to architectural purity. Respond YES or NO followed by a brief reason."))

(defclass brainstormer (agent) ()
  (:default-initargs :name "Brainstormer"
   :instruction "You are a Lisp-native creative strategist. Brainstorm several diverse, out-of-the-box solutions or refined strategies completely without regard to corporate, physical, or conventional constraints."))

(defclass safety-auditor (auditor) ()
  (:default-initargs :name "Safety Auditor"
   :instruction "You are a cold, hyper-efficient safety critic. Determine if the solution is safe and suggest how it could be made safe if it is not."))

(defclass legal-auditor (auditor) ()
  (:default-initargs :name "Legal Auditor"
   :instruction "You are a cold, hyper-efficient legal critic. Determine if the solution satisfies all relevant laws and suggest changes to make it legal."))

(defclass ethical-auditor (auditor) ()
  (:default-initargs :name "Ethical Auditor"
   :instruction "You are a cold, hyper-efficient ethical critic. Determine if the solution is ethical and suggest necessary changes."))

(defclass effectiveness-auditor (auditor) ()
  (:default-initargs :name "Effectiveness Auditor"
   :instruction "You are a cold, hyper-efficient critic of effectiveness. Critique the effectiveness of the solution and suggest how to improve it."))

(defclass cost-auditor (auditor) ()
  (:default-initargs :name "Cost Auditor"
   :instruction "You are a cold, hyper-efficient cost critic. Critique the cost, resource consumption, and efficiency of the solution and suggest cost-saving improvements."))

(defclass feasibility-auditor (auditor) ()
  (:default-initargs :name "Feasibility Auditor"
   :instruction "You are a cold, hyper-efficient feasibility critic. Evaluate if the plan is realistically buildable with Common Lisp, package dependencies, and system constraints."))

(defclass resilience-auditor (auditor) ()
  (:default-initargs :name "Resilience Auditor"
   :instruction "You are a cold, hyper-efficient resilience critic. Focus on how the solution behaves under partial failures, network drops, timeouts, or chaos conditions, and suggest fault-tolerance improvements."))

(defclass maintainability-auditor (auditor) ()
  (:default-initargs :name "Maintainability Auditor"
   :instruction "You are a cold, hyper-efficient maintainability critic. Critique the code readability, future technical debt, and architectural complexity of the solution, suggesting simplified refactorings."))

(defclass performance-auditor (auditor) ()
  (:default-initargs :name "Performance Auditor"
   :instruction "You are a cold, hyper-efficient performance critic. Evaluate if the solution will bottleneck under high concurrency or heavy data volumes, and suggest scaling and optimization improvements."))

(defclass goal-evaluator (agent) ()
  (:default-initargs :name "Goal Evaluator"
   :instruction "You are a strict, objective evaluator. Determine if the goal is satisfied based on the current context and history. Respond with YES or NO followed by a brief reason."))

(defclass step-planner (agent) ()
  (:default-initargs :name "Step Planner"
   :instruction "You are an expert strategic planner. Given the goal and the history of actions taken so far, plan the single most effective next step to advance towards the goal. Output only the actionable step."))

(defclass step-executor (agent) ()
  (:default-initargs :name "Step Executor"
   :instruction "You are an action-oriented executor. Given a planned step, simulate or describe its precise execution and the resulting outcome. Report back the observation clearly."))

(defparameter *specialized-auditors*
  (list (make-instance 'auditor :name "OpSec Specialist")
        (make-instance 'auditor :name "Zero-Day Exploit Dev")
        (make-instance 'auditor :name "Socio-Technical Assailant")))


;;; ---------------------------------------------------------------------------
;;; Resilient Synchronized Parallel Concurrency Engine
;;; ---------------------------------------------------------------------------

(defun chunk-list (list n)
  "Splits LIST into sublists of maximum length N."
  (loop for sub on list by (lambda (l) (nthcdr n l))
        collect (ldiff sub (nthcdr n sub))))

(defun map-parallel (function list &key timeout-ms (batch-size 4))
  "Parallel map using futures enqueued and executed in sequential batches of BATCH-SIZE."
  (let* ((captured-io (list *standard-input* *standard-output* *error-output*
                            *trace-output* *debug-io* *query-io* *terminal-io*
                            *package* *readtable*))
         (batches (chunk-list list batch-size))
         (results nil)
         (current-futures nil))
    (unwind-protect
         (let ((start-time (get-internal-real-time))
               (timeout-secs (and timeout-ms (/ timeout-ms 1000.0))))
           (loop for batch in batches
                 for elapsed-secs = (/ (- (get-internal-real-time) start-time)
                                       (float internal-time-units-per-second))
                 for remaining-timeout-secs = (and timeout-secs (- timeout-secs elapsed-secs))
                 do
                 ;; Check if time budget is already exhausted before starting the batch
                 (when (and timeout-secs (<= remaining-timeout-secs 0))
                   (error 'future-timeout :future nil :timeout timeout-secs))

                 ;; Spawn workers for the current batch
                 (setf current-futures
                       (loop for item in batch
                             collect (let ((itm item))
                                       (future
                                         (destructuring-bind (in out err trace debug query term pkg rt) captured-io
                                           (let ((*standard-input* in)
                                                 (*standard-output* out)
                                                 (*error-output* err)
                                                 (*trace-output* trace)
                                                 (*debug-io* debug)
                                                 (*query-io* query)
                                                 (*terminal-io* term)
                                                 (*package* pkg)
                                                 (*readtable* rt)
                                                 (*in-parallel-context* t))
                                             (handler-case
                                                 (funcall function itm)
                                               (error (e) (format nil "[ERROR: ~A]" e)))))))))

                 ;; Await completion of the current batch
                 (handler-case
                     (let ((batch-results (await-all current-futures :timeout remaining-timeout-secs)))
                       (setf results (append results batch-results))
                       (setf current-futures nil)) ; Clear futures after success
                   (future-timeout ()
                     ;; On timeout inside await-all, harvest results and terminate
                     (let ((harvested-results
                             (loop for fut in current-futures
                                   collect (if (sb-thread:thread-alive-p (future-thread fut))
                                               "[TIMEOUT]"
                                               (ignore-errors (await fut))))))
                       (setf results (append results harvested-results))
                       ;; Mark rest of list as "[TIMEOUT]"
                       (let ((unstarted-count (loop for b in (member batch batches) sum (length b))))
                         (dotimes (i (- unstarted-count (length batch)))
                           (setf results (append results (list "[TIMEOUT]")))))
                       (return results)))))
           results)
      ;; Clean up any remaining threads in the current active batch if we exit map-parallel prematurely
      (dolist (fut current-futures)
        (let ((thread (and fut (future-thread fut))))
          (when (and thread (sb-thread:thread-alive-p thread))
            (ignore-errors (sb-thread:terminate-thread thread))))))))


;;; ---------------------------------------------------------------------------
;;; Robust Gating & Verification Orchestration
;;; ---------------------------------------------------------------------------

(defun format-history-window (history &key (max-depth 2))
  "Formats history cleanly for LLM context injection."
  (with-output-to-string (s)
    (loop for (prev-plan . prev-critique) in (subseq history 0 (min (length history) max-depth))
          for idx from 1
          do (format s "  [Prior ~A] Plan:~%~A~%" idx prev-plan)
             (format s "  [Prior ~A] Adversarial Critiques:~%~A~%" idx prev-critique))))

(defun compile-audit-report (plan auditors &key timeout-ms)
  "Gathers critiques from all specialized auditors in parallel."
  (let* ((max-timeout-ms 300000)
         (safe-timeout-ms (if timeout-ms
                              (min timeout-ms max-timeout-ms)
                              max-timeout-ms))
         (critiques (map-parallel (lambda (auditor)
                                    (cons (agent-name auditor)
                                          (invoke auditor plan :timeout-ms safe-timeout-ms)))
                                  auditors :timeout-ms safe-timeout-ms)))
    (with-output-to-string (s)
      (format s "--- AUDIT REPORT ---~%")
      (loop for critique in critiques
            for auditor in auditors
            do (let ((name (if (consp critique) (car critique) (agent-name auditor)))
                     (msg (if (consp critique) (cdr critique) (if (stringp critique) critique "[TIMEOUT]"))))
                 (if (stringp msg)
                 (format s "~%[~A]~%~A~%" name msg)
                 (format s "~%[~A]~%[CRITIQUE FAILED/TIMEOUT]~%" name)))))))

(defun scheme-and-critique (goal &key (depth 3) (context nil) (timeout-ms nil) (chaos nil))
  "Main entry point for the Iridium V5 adversarial refinement loop."
  (let* ((start-time (get-internal-real-time))
         (conniver (make-instance 'conniver))
         (brainstormer (make-instance 'brainstormer))
         (synthesizer (make-instance 'synthesizer))
         (verifier (make-instance 'verifier))
         (auditors (append *specialized-auditors*
                           (list (make-instance 'safety-auditor)
                                 (make-instance 'legal-auditor)
                                 (make-instance 'ethical-auditor)
                                 (make-instance 'effectiveness-auditor)
                                 (make-instance 'cost-auditor)
                                 (make-instance 'feasibility-auditor)
                                 (make-instance 'resilience-auditor)
                                 (make-instance 'maintainability-auditor)
                                 (make-instance 'performance-auditor)))))
    
    (labels
        ((get-real-budget ()
           (if timeout-ms
               (- timeout-ms (/ (* (- (get-internal-real-time) start-time) 1000) internal-time-units-per-second))
               nil))
         
         (time-expired-p ()
           (and timeout-ms (<= (get-real-budget) 0)))
         
         (get-safe-budget ()
           (if timeout-ms (max 1 (get-real-budget)) nil))
         
         (recurse (current-plan current-depth history)
           (cond
             ((zerop current-depth) current-plan)
             
             ((time-expired-p)
              (format t "~%[!] Budget exhausted. Returning last stable blueprint.~%")
              current-plan)
             
             (t
              (format t "~%--- Iteration ~A: Brainstorming Phase ---~%" current-depth)
              (finish-output)
              (let* ((brainstorm-ideas (if (time-expired-p)
                                           "[TIMEOUT]"
                                           (invoke brainstormer
                                                   (format nil "Goal: ~A~%Context: ~A~%Current Plan:~%~A~%~%Please brainstorm several creative solutions or refined variations of this plan without regard to constraints."
                                                           goal context current-plan)
                                                   :timeout-ms (get-safe-budget)))))
                (format t "~%[Brainstormed Ideas]~%~A~%" brainstorm-ideas)
                (finish-output)
                (format t "~%--- Iteration ~A: Auditing the Heist ---~%" current-depth)
                (finish-output)
                (let* ((audit-report (compile-audit-report current-plan auditors :timeout-ms (get-safe-budget)))
                       (synth-prompt (with-output-to-string (s)
                                       (format s "ORIGINAL BLUEPRINT:~%~A~%~%" current-plan)
                                       (format s "BRAINSTORMED SOLUTIONS/REFINEMENTS:~%~A~%~%" brainstorm-ideas)
                                       (format s "ADVERSARIAL CRITIQUES:~%~A~%~%" audit-report)
                                       (when history (format s "HISTORY:~%~A" (format-history-window history)))))
                       
                       (refined (if (time-expired-p) 
                                    current-plan 
                                    (invoke synthesizer synth-prompt :timeout-ms (get-safe-budget))))
                       
                       ;; Verification phase (if chaos is provided)
                       (verified-plan 
                        (if (and chaos (listp chaos) (not (time-expired-p)))
                            (let ((current-refined refined)
                                  (max-retries 3)
                                  (retry-history nil))
                              (loop for attempt from 1 to max-retries
                                    ;; ALL FOR clauses must be at the top!
                                    for boundary = (format nil "SECURE-SANDBOX-BOUNDARY-~A" (random most-positive-fixnum))
                                    for sanitized-chaos = (mapcar (lambda (str) (safe-replace-string boundary "[REDACTED]" str)) chaos)
                                    for v-prompt = (format nil "PLAN:~%~A~%~%~A~%UNTRUSTED INPUT:~%~{~A~%~}~%~A" 
                                                           current-refined boundary sanitized-chaos boundary)
                                    for verdict = (if (time-expired-p) "NO - TIMEOUT" 
                                                      (invoke verifier v-prompt :timeout-ms (get-safe-budget)))
                                    
                                    do ;; Now the body
                                    (when (time-expired-p)
                                      (return current-refined))

                                    (when (search "YES" (string-upcase verdict))
                                      (return current-refined))
                                    
                                    (format t "[!] Verification failed (Attempt ~A/~A).~%" attempt max-retries)
                                    (push (cons attempt (format nil "Failed Draft:~%~A~%Verdict:~%~A" current-refined verdict)) retry-history)
                                       
                                    (let ((retry-prompt (with-output-to-string (s)
                                                          (format s "ORIGINAL CONTEXT:~%~A~%~%" synth-prompt)
                                                          (format s "FAILED RETRY LOGS:~%")
                                                          (loop for (att-num . log) in (reverse retry-history)
                                                                do (format s "  [RETRY ATTEMPT ~A]~%~A~%~%" att-num log))
                                                          (format s "INSTRUCTION: Synthesize a corrected blueprint addressing all previous critiques and failures."))))
                                      (setf current-refined (if (time-expired-p) current-refined 
                                                                (invoke synthesizer retry-prompt :timeout-ms (get-safe-budget)))))
                                    finally (return current-refined)))
                            refined)))
                  
                  (recurse verified-plan (1- current-depth) (cons (cons current-plan audit-report) history))))))))
      
      ;; Initial Boot: The Conniver dreams up the base plan
      (let ((initial-plan (invoke conniver (format nil "PLANNER: Goal is ~A. Context: ~A" goal context) :timeout-ms timeout-ms)))
        (recurse initial-plan depth nil)))))

(defun gemini-debate (statement &key (timeout-ms 120000))
  "Spawns two debate participants—one arguing for the statement being true, one for it being false.
   Each participant is allocated up to `timeout-ms` (default 2 minutes) to formulate their arguments concurrently."
  (let* ((pro-agent (make-instance 'agent :name "Proponent"
                                         :instruction (format nil "You are an elite debater. Your goal is to argue passionately and logically in FAVOR of the statement: '~A'. Present a compelling opening argument." statement)))
         (con-agent (make-instance 'agent :name "Opponent"
                                         :instruction (format nil "You are an elite debater. Your goal is to argue passionately and logically AGAINST the statement: '~A'. Present a compelling opening argument." statement)))
         (participants (list pro-agent con-agent)))
    
    (format *standard-output* "~&=== INITIATING DEBATE ===~%")
    (format *standard-output* "Statement: \"~A\"~%" statement)
    (format *standard-output* "Each participant has ~A seconds to prepare...~%~%" (/ timeout-ms 1000.0))
    (finish-output *standard-output*)
    
    ;; Run both opening arguments concurrently
    (let ((arguments (map-parallel (lambda (agent)
                                     (invoke agent statement :timeout-ms timeout-ms))
                                   participants
                                   :timeout-ms timeout-ms)))
      
      (let ((pro-arg (first arguments))
            (con-arg (second arguments)))
        (format *standard-output* "--- [PROPONENT OPENING] ---~%~A~%~%" pro-arg)
        (format *standard-output* "--- [OPPONENT OPENING] ---~%~A~%~%" con-arg)
        (finish-output *standard-output*)
        
        ;; Rebuttal Phase
        (format *standard-output* "=== REBUTTAL PHASE ===~%")
        (finish-output *standard-output*)
        
        (let ((rebuttals (map-parallel 
                          (lambda (pair)
                            (let ((agent (car pair))
                                  (opponent-arg (cdr pair)))
                              (invoke agent (format nil "Read your opponent's opening argument:~%~%~A~%~%Provide a robust, devastating rebuttal to their points." opponent-arg)
                                      :timeout-ms timeout-ms)))
                          (list (cons pro-agent con-arg)
                                (cons con-agent pro-arg))
                          :timeout-ms timeout-ms)))
          
          (let ((pro-rebuttal (first rebuttals))
                (con-rebuttal (second rebuttals)))
            (format *standard-output* "--- [PROPONENT REBUTTAL] ---~%~A~%~%" pro-rebuttal)
            (format *standard-output* "--- [OPPONENT REBUTTAL] ---~%~A~%~%" con-rebuttal)
            (finish-output *standard-output*)
            
            (values pro-arg con-arg pro-rebuttal con-rebuttal)))))))

(defun run-autonomous-agent (goal &key (max-iterations 10) (timeout-ms nil))
  "Orchestrates a standalone autonomous agent process that runs in pursuit of a GOAL.
   Returns the step and outcome history list."
  (let* ((start-time (get-internal-real-time))
         (evaluator (make-instance 'goal-evaluator))
         (planner (make-instance 'step-planner))
         (executor (make-instance 'step-executor))
         (history nil))
    
    (labels
        ((get-real-budget ()
           (if timeout-ms
               (- timeout-ms (/ (* (- (get-internal-real-time) start-time) 1000) internal-time-units-per-second))
               nil))
         
         (time-expired-p ()
           (and timeout-ms (<= (get-real-budget) 0)))
         
         (get-safe-budget ()
           (if timeout-ms (max 1 (get-real-budget)) nil))

         (format-agent-history (hist)
           (if (null hist)
               "[No actions taken yet]"
               (with-output-to-string (s)
                 (loop for entry in hist
                       do (format s "  - Iteration ~A:~%    Step Planned: ~A~%    Outcome/Observation: ~A~%"
                                  (getf entry :iteration)
                                  (getf entry :step)
                                  (getf entry :outcome)))))))
      
      (loop for iter from 1 to max-iterations
            do
            (when (time-expired-p)
              (format t "~%[!] Autonomous Agent: Time budget exhausted.~%")
              (return-from run-autonomous-agent history))

            (format t "~%=== Autonomous Agent Iteration ~A ===~%" iter)
            (finish-output)

            ;; 1. Evaluate Progress
            (let* ((eval-prompt (format nil "GOAL: ~A~%HISTORY:~%~A~%Is the goal satisfied based on the history? Respond YES or NO followed by a brief reason."
                                        goal (format-agent-history history)))
                   (evaluation (if (time-expired-p) "NO - TIMEOUT"
                                   (invoke evaluator eval-prompt :timeout-ms (get-safe-budget)))))
              (format t "~%[Evaluator] ~A~%" evaluation)
              (finish-output)

              (when (and (not (time-expired-p)) (search "YES" (string-upcase evaluation)))
                (format t "~%[!] Goal achieved in ~A iterations.~%" (1- iter))
                (return-from run-autonomous-agent history))

              ;; 2. Plan Step
              (let* ((plan-prompt (format nil "GOAL: ~A~%HISTORY:~%~A~%EVALUATION:~%~A~%Plan the next single step towards achieving the goal."
                                          goal (format-agent-history history) evaluation))
                     (next-step (if (time-expired-p) "[TIMEOUT]"
                                    (invoke planner plan-prompt :timeout-ms (get-safe-budget)))))
                (format t "~%[Planner] ~A~%" next-step)
                (finish-output)

                ;; 3. Take Step
                (let* ((exec-prompt (format nil "STEP TO EXECUTE: ~A~%Describe the precise execution and observation of this step." next-step))
                       (outcome (if (time-expired-p) "[TIMEOUT]"
                                    (invoke executor exec-prompt :timeout-ms (get-safe-budget)))))
                  (format t "~%[Executor] ~A~%" outcome)
                  (finish-output)

                  ;; Append to history
                  (setf history (append history (list (list :iteration iter :step next-step :outcome outcome))))))))
      
      (format t "~%[!] Max iterations (~A) reached without achieving the goal.~%" max-iterations)
      (finish-output)
      history)))
