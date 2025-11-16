;;; -*- Lisp -*-

;;; A replacement REPL that can evaluate Lisp forms or send free-form
;;; text to the Gemini API.  If you type in a Lisp form, it is
;;; evaluated, but if it cannot be read as a single Lisp form, it is
;;; assumed to be free-form text to be sent to Gemini.

(in-package "GEMINI")

(defun repl-string-dispatch (input if-form if-free-form)
  "Dispatches INPUT to either IF-FORM or IF-FREE-FORM based on its content."
  (let ((trimmed (str:trim input)))
    (multiple-value-bind (form count)
        (handler-case
            (read-from-string trimmed)
          (error () (values nil 0)))
      (if (= count (length trimmed))
          (funcall if-form form)
          (funcall if-free-form trimmed)))))

(defun repl-eval-print-form (form)
  "Evaluates a Lisp FORM and prints the result."
  (let ((values (with-llm-debugger (multiple-value-list (eval form)))))
    (setq *** **
          ** *
          * (if (null values) nil (car values))
          +++ ++
          ++ +
          + form)
    (let ((printed-values
            (cond ((null values) (format nil "~&;; No values~%"))
                  ((= (length values) 1)
                   (format nil "~&;; Value: ~S~%" (car values)))
                  (t (format nil "~&;; Values:~%~{;;   * ~S~%~}" values)))))
      (format *standard-output* "~A" printed-values)
      ;; Put a function call and response into the context.
      (push (content :parts (list
                             (let ((text-part (make-hash-table :test 'equal)))
                               (setf (gethash "text" text-part) (format nil "eval ~s" form))
                               text-part))
                     :role "user")
            *context*)
      (push (content :parts (list
                             (let ((call-part (make-hash-table :test 'equal)))
                               (setf (gethash "functionCall" call-part)
                                     (function-call
                                      :name "eval"
                                      :args (let ((args (make-hash-table :test 'equal)))
                                              (setf (gethash "string" args)
                                                    (format nil "~S" form))
                                              args)))
                               call-part))
                     :role "model")
            *context*)
      (push (content :parts (list
                             (let ((response-part (make-hash-table :test 'equal)))
                               (setf (gethash "functionResponse" response-part)
                                     {
                                     :name "eval"
                                     :response (let ((response (make-hash-table :test 'equal)))
                                                 (setf (gethash "result" response)
                                                       (if (null values)
                                                           "No values"
                                                           (if (= (length values) 1)
                                                               (format nil "~S" (car values))
                                                               (format nil "~{~S~^, ~}" values))))
                                                 response)
                                     })
                               response-part))
                     :role "function")
            *context*)
      (push (content :parts
                     (list (let ((text-part (make-hash-table :test 'equal)))
                             (setf (gethash "text" text-part)
                                   (format nil "~A" printed-values))
                             text-part))
                     :role "model")
            *context*)
      (setq *prior-context* *context*))
    (apply #'values values)))

(defparameter +general-repl-system-instruction+
  (str:join
   #\newline
   `("You are an assistant engaging in pair programming with the user."
     "We are writing Common Lisp code."
     "You have access to tools to introspect the Common Lisp runtime environment."
     "You make use the `eval` tool to evaluate expressions to probe the runtime.")))

(defparameter +repl-output-requirements+
  (str:join
   #\newline
   `("Output text without surrounding quotations or triple backticks.")))

(defun repl-system-instruction ()
  (content
   :parts (list (part +general-repl-system-instruction+)
                (part +repl-output-requirements+))))

(defun repl-send-to-gemini (free-form)
  "Sends a FREE-FORM string to the Gemini API for continuation."
  (let ((response (let ((*system-instruction* (repl-system-instruction)))
                    (continue-gemini *default-persona* free-form))))
    (if (stringp response)
        (format *standard-output* "~&~A~%" response)
        (format *standard-output* "~&Error: ~A~%" response))
    (finish-output *standard-output*)
    response))

(defun llm-repl ()
  "Start a REPL for interacting with the LLM."
  (let ((prompt (format nil "(LLM) ~a> " (package-name *package*))))
    (loop
      (finish-output *standard-output*)
      (format t "~A" prompt)
      (finish-output *standard-output*)
      (repl-string-dispatch
       (read-line)
       #'repl-eval-print-form
       #'repl-send-to-gemini))))
