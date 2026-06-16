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
  "Executes body, updating global counters based on whether it returns a true value, nil, or signals an error."
  (let ((result-sym (gensym "RESULT")))
    `(handler-case
         (let ((,result-sym (progn ,@body)))
           (if ,result-sym
               (incf *llm-returns-value-count*)
               (incf *llm-returns-nothing-count*))
           ,result-sym)
       (error (e)
         (incf *llm-abort-count*)
         (error e)))))

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

(defun invoke-gemini (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
  (with-llm-instrumentation
    (generate-content *default-content-generator* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))

(defun gemini-flash-lite (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  (with-llm-instrumentation
    (generate-content *gemini-flash-lite* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))

(defun gemini-uncensored (prompt &key context mood parts files system-instruction (connect-timeout 60) (read-timeout 300))
  (with-llm-instrumentation
    (generate-content *gemini-uncensored* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))

(defun prompt-predicate (prompt &key context mood parts files system-instruction)
  (let ((ans (content->text
              (without-personality
                (generate-content *gemini-flash-lite* context mood
                                  (format nil "~a~%Answer with `T` if true, `NIL` if false." prompt)
                                  parts files system-instruction)))))
    (cond ((search "T" (string-upcase ans)) t)
          ((search "NIL" (string-upcase ans)) nil)
          (t nil))))

(defun gemini-flash (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  "Fixed typo in lambda list to ensure timeouts actually pass through."
  (with-llm-instrumentation
    (generate-content *gemini-flash* context mood prompt parts files system-instruction 
                      :read-timeout read-timeout :connect-timeout connect-timeout)))

(defun flash-compress (text)
  (content->text
   (without-personality
     (generate-content *gemini-flash* nil nil (format nil "Condense the following text for reduced tokens.  Retain all meaning:~%~%~a" text) nil nil 
 "Task: Lossless token compression.
 Constraint: Preserve 100% of facts and meaning.
 Output: Raw condensed text only. No preamble. No chatter."))))

(defun gemini-pro (prompt &key context mood parts files system-instruction)
  (with-llm-instrumentation
    (generate-content *gemini-pro* context mood prompt parts files system-instruction)))

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
