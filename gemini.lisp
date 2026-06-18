;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defvar *llm-returns-value-count* 0
  "Count of top-level LLM calls that successfully returned a value.")
(defvar *llm-returns-nothing-count* 0
  "Count of top-level LLM calls that completed but returned NIL or empty.")
(defvar *llm-abort-count* 0
  "Count of top-level LLM calls that aborted due to an error.")

(defun get-llm-stats ()
  "Returns the current counts for top-level LLM procedure outcomes as a plist."
  (list :returned-value *llm-returns-value-count*
        :returned-nothing *llm-returns-nothing-count*
        :aborted *llm-abort-count*))

(defun reset-llm-stats ()
  "Resets the top-level LLM procedure outcome counters to zero."
  (setf *llm-returns-value-count* 0)
  (setf *llm-returns-nothing-count* 0)
  (setf *llm-abort-count* 0)
  (values))

(defmacro with-llm-instrumentation (&body body)
  "Executes body, updating global counters based on whether it returns a true value, nil, or signals an error, without unwinding the stack prematurely."
  (let ((result-sym (gensym "RESULT")))
    `(handler-bind ((error (lambda (e)
                             (declare (ignore e))
                             (incf *llm-abort-count*)))) ; Just increment and decline to handle
       (let ((,result-sym (progn ,@body)))
         (if ,result-sym
             (incf *llm-returns-value-count*)
             (incf *llm-returns-nothing-count*))
         ,result-sym))))

(defun call-with-model-override (generator model thunk)
  "Executes THUNK, temporarily overriding the model of the given GENERATOR if MODEL is non-NIL."
  (if (null model)
      (funcall thunk)
      (let ((old-model (get-model generator)))
        (unwind-protect
             (progn
               (setf (get-model generator) model)
               (funcall thunk))
          (setf (get-model generator) old-model)))))

(defun content->text (content)
  (if (null content)
      ""
      (reduce (lambda (l r)
                (concatenate 'string l (string #\Newline) r))
              (remove nil (map 'list (lambda (part)
                                       (when (text-part? part)
                                         (get-text part)))
                               (get-parts content))))))

(defun content->sexp (content)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string (content->text content)))))

(defun invoke-gemini (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
  "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
  (with-llm-instrumentation
    (call-with-model-override *default-content-generator* model
      (lambda ()
        (generate-content *default-content-generator* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

(defun gemini-flash-lite (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
  (with-llm-instrumentation
    (call-with-model-override *gemini-flash-lite* model
      (lambda ()
        (generate-content *gemini-flash-lite* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

(defun gemini-uncensored (prompt &key context mood parts files system-instruction model (connect-timeout 60) (read-timeout 300))
  (with-llm-instrumentation
    (call-with-model-override *gemini-uncensored* model
      (lambda ()
        (generate-content *gemini-uncensored* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

(defun prompt-predicate (prompt &key context mood parts files system-instruction)
  (let ((ans (content->text
              (without-personality
                (generate-content *gemini-flash-lite* context mood
                                  (format nil "~a~%Answer with `T` if true, `NIL` if false." prompt)
                                  parts files system-instruction)))))
    (cond ((search "T" (string-upcase ans)) t)
          ((search "NIL" (string-upcase ans)) nil)
          (t nil))))

(defun gemini-flash (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
  "Fixed typo in lambda list to ensure timeouts actually pass through."
  (with-llm-instrumentation
    (call-with-model-override *gemini-flash* model
      (lambda ()
        (generate-content *gemini-flash* context mood prompt parts files system-instruction 
                          :read-timeout read-timeout :connect-timeout connect-timeout)))))

(defun flash-compress (text)
  (content->text
   (without-personality
     (generate-content *gemini-flash* nil nil (format nil "Condense the following text for reduced tokens.  Retain all meaning:~%~%~a" text) nil nil 
 "Task: Lossless token compression.
 Constraint: Preserve 100% of facts and meaning.
 Output: Raw condensed text only. No preamble. No chatter."))))

(defun gemini-pro (prompt &key context mood parts files system-instruction model)
  (with-llm-instrumentation
    (call-with-model-override *gemini-pro* model
      (lambda ()
        (generate-content *gemini-pro* context mood prompt parts files system-instruction)))))

(defparameter *compare-files-prompt*
  "Analyze the two provided files and determine their semantic similarity on a scale from 0.0 to 1.0, where 0.0 indicates the files are entirely unrelated and 1.0 indicates they are semantically identical. Output the score as a single real number, excluding all other text, explanations, or commentary.")

(defun similarity (f1 f2)
  (without-personality
    (parse-float-safely
     (content->text
      (gemini-flash-lite *compare-files-prompt* :files (list f1 f2))))))

(defun multiprompt (generator prompt-list &optional last-part)
  (if (null prompt-list)
      last-part
      (let ((response (if last-part
                          (funcall generator (list (car prompt-list) last-part))
                          (funcall generator (car prompt-list)))))
        (multiprompt generator (cdr prompt-list) (content->text response)))))

(defun resolve-model-string (model)
  (typecase model
    (null nil)
    (string model)
    (symbol
     (let* ((name (string-downcase (symbol-name model)))
            (clean-name (if (and (> (length name) 7) (string= (subseq name 0 7) "gemini-"))
                            name
                            name)))
       ;; check if registered
       (let ((m (find-model clean-name)))
         (if m
             (get-model-id m)
             (let ((m2 (find-model (concatenate 'string "models/" clean-name))))
               (if m2
                   (get-model-id m2)
                   (concatenate 'string "models/" clean-name)))))))))

(defun invoke-interaction (prompt &key (model :gemini-3.5-flash) agent background tool-configs receiver)
  "Sends a stateful prompt using the Interactions API.
   Returns (values steps raw-response)."
  (let* ((backend (make-instance 'interactions-backend))
         (payload (make-hash-table :test 'equal)))
    (assert (or model agent) nil "Must specify either model or agent.")
    (if agent
        (setf (gethash "agent" payload) (string-downcase (string-upcase (princ-to-string agent))))
        (setf (gethash "model" payload) (resolve-model-string model)))
    
    (setf (gethash "input" payload) (build-interactions-input prompt))
      
    (when background
      (setf (gethash "background" payload) t))
    (when tool-configs
      (setf (gethash "tools" payload) tool-configs))
    (invoke-backend backend (or agent model) payload :receiver receiver)))
