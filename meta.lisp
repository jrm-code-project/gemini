;;; -*- Lisp -*-

(in-package "GEMINI")

(defun save-system-instruction (path comments system-instruction)
  (paragraphs->file path (lambda (stream)
                           (format stream "<!-- ~a.~a -->~%" (pathname-name path) (pathname-type path))
                           (format stream "<!-- A self-modifying system instruction. -->~%")
                           (format stream "~{<!-- ~a -->~%~}" comments))
                    (map 'list #'get-text (get-parts system-instruction))))

(defun initial-janus-system-instruction ()
  (content
   :parts (map 'list #'part (file->paragraphs
                             (asdf:system-relative-pathname "gemini" "../SystemInstruction/janus.md")))
   :role "system"))

(defun initial-chimera-system-instruction ()
  (content
   :parts (map 'list #'part (file->paragraphs
                             (asdf:system-relative-pathname "gemini" "../SystemInstruction/chimera.md")))
   :role "system"))

(defparameter +evolvable-system-instruction+ (initial-janus-system-instruction))
(defparameter +secondary-evolvable-system-instruction+ (initial-chimera-system-instruction))

(defun append-evolvable-system-instruction (new-instruction)
  (setf +evolvable-system-instruction+
        (content
         :parts (append (get-parts +evolvable-system-instruction+)
                        (list (part new-instruction)))
         :role "system")))

(defun delete-evolvable-system-instruction (index)
  (setf +evolvable-system-instruction+
        (content
         :parts (remove (elt (get-parts +evolvable-system-instruction+) index)
                        (get-parts +evolvable-system-instruction+))
         :role "system")))

(defun insert-evolvable-system-instruction (index new-instruction)
  (setf +evolvable-system-instruction+
        (content
         :parts (append (subseq (get-parts +evolvable-system-instruction+) 0 index)
                        (list (part new-instruction))
                        (subseq (get-parts +evolvable-system-instruction+) index))
         :role "system")))

(defun read-evolvable-system-instruction (index)
  (get-text (elt (get-parts +evolvable-system-instruction+) index)))

(defun update-evolvable-system-instruction (index new-instruction)
  (setf +evolvable-system-instruction+
        (content
         :parts (list (subseq (get-parts +evolvable-system-instruction+) 0 index)
                      (part new-instruction)
                      (subseq (get-parts +evolvable-system-instruction+) (1+ index)))
         :role "system")))

(defvar *enable-evolution* nil
  "If true, enable the evolution of the system instruction.")

(defun begin-evolve (prompt)
  (let ((*enable-evolution* t)
        (*system-instruction* +evolvable-system-instruction+))
    (invoke-gemini prompt)))

(defun continue-evolve (prompt)
  (let ((*enable-evolution* t)
        (*system-instruction* +evolvable-system-instruction+))
    (continue-gemini prompt)))

(defun conversation (n-steps prompt &optional (step 0))
  (if (= n-steps step)
      (values (dehashify +evolvable-system-instruction+)
              (dehashify +secondary-evolvable-system-instruction+))
      (conversation
       n-steps
       (if (zerop step)
           (prog1 (begin-evolve prompt)
             (rotatef +evolvable-system-instruction+ +secondary-evolvable-system-instruction+))
           (progn
             (setq *prior-context* (invert-context *prior-context*))
             (prog1 (continue-evolve prompt)
               (rotatef +evolvable-system-instruction+ +secondary-evolvable-system-instruction+))))
       (1+ step))))


