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
   :instruction "Merge the scheme and critiques into a hardened architectural blueprint. Focus on high-fidelity integration."))

(defclass verifier (agent) ()
  (:default-initargs :name "Verifier" 
   :instruction "Verify if the plan mitigates the chaos event and adheres to architectural purity. Respond YES or NO followed by a brief reason."))

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
         (synthesizer (make-instance 'synthesizer))
         (verifier (make-instance 'verifier))
         (auditors *specialized-auditors*))
    
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
              (format t "~%--- Iteration ~A: Auditing the Heist ---~%" current-depth)
              (let* ((audit-report (compile-audit-report current-plan auditors :timeout-ms (get-safe-budget)))
                     (synth-prompt (with-output-to-string (s)
                                     (format s "ORIGINAL BLUEPRINT:~%~A~%~%" current-plan)
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
                
                (recurse verified-plan (1- current-depth) (cons (cons current-plan audit-report) history)))))))
      
      ;; Initial Boot: The Conniver dreams up the base plan
      (let ((initial-plan (invoke conniver (format nil "PLANNER: Goal is ~A. Context: ~A" goal context) :timeout-ms timeout-ms)))
        (recurse initial-plan depth nil)))))
