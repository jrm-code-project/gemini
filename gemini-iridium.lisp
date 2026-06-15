;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

;;; Update the Agent invoke to pass the total budget down
(defmethod invoke ((self agent) prompts &key model-override (timeout-ms nil))
  (let* ((active-model (resolve-agent-model self model-override))
         (parts (mapcar #'part (if (listp prompts) prompts (list prompts)))))
    (handler-case
        (content->text
         (funcall active-model parts
                  :system-instruction (agent-instruction self)
                  ;; Pass the budget to the generator
                  :read-timeout (and timeout-ms (max 1 (floor timeout-ms 1000)))
                  :connect-timeout 30))
      (error (e) (format nil "[Agent '~A' Error: ~A]" (agent-name self) e)))))


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

(defparameter *specialized-auditors*
  (list (make-instance 'auditor :name "OpSec Specialist")
        (make-instance 'auditor :name "Zero-Day Exploit Dev")
        (make-instance 'auditor :name "Socio-Technical Assailant")))


;;; ---------------------------------------------------------------------------
;;; Resilient Synchronized Parallel Concurrency Engine
;;; ---------------------------------------------------------------------------

(defun map-parallel (function list &key timeout-ms)
  "Parallel map using sb-thread with join-thread timeout handling."
  (let* ((captured-io (list *standard-input* *standard-output* *error-output*
                            *trace-output* *debug-io* *query-io* *terminal-io*
                            *package* *readtable*))
         (start-time (get-internal-real-time))
         (timeout-units (and timeout-ms (round (* timeout-ms (/ internal-time-units-per-second 1000.0)))))
         (deadline (and timeout-units (+ start-time timeout-units)))
         (threads nil))
    ;; Spawn workers
    (loop for item in list do
          (let ((itm item))
            (push (sb-thread:make-thread
                   (lambda ()
                     (destructuring-bind (in out err trace debug query term pkg rt) captured-io
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
                             (sb-ext:with-timeout (if timeout-ms (/ timeout-ms 1000.0) 3600)
                               (funcall function itm))
                           (sb-ext:timeout () "[TIMEOUT]")
                           (error (e) (format nil "[ERROR: ~A]" e)))))))
                  threads)))
    ;; Wait for results in original order
    (setf threads (nreverse threads))
    ;; Join each thread with the remaining timeout budget
    (loop for thread in threads
          collect (let ((remaining-secs (when deadline
                                          (let ((rem (- deadline (get-internal-real-time))))
                                            (if (<= rem 0) 0.0 (/ (float rem) internal-time-units-per-second))))))
                    (if (and deadline (<= remaining-secs 0))
                        (progn
                          (ignore-errors (sb-thread:terminate-thread thread))
                          "[TIMEOUT]")
                        (multiple-value-bind (val status)
                            (sb-thread:join-thread thread :timeout remaining-secs :default "[TIMEOUT]")
                          (if (eq status :timeout)
                              (progn
                                (ignore-errors (sb-thread:terminate-thread thread))
                                "[TIMEOUT]")
                              val)))))))


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
  (let ((critiques (map-parallel (lambda (auditor)
                                   (cons (agent-name auditor)
                                         (invoke auditor plan :timeout-ms timeout-ms)))
                                 auditors :timeout-ms timeout-ms)))
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
