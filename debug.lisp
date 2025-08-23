;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +general-debug-system-instructions+
  (str:join
   #\newline
   `("You are a Common Lisp expert with many years experience."
     "You are particularly adept at debugging Common Lisp errors."
     "You will be given an error message and a backtrace."
     "You may use tools to introspect the Common Lisp environment to aid in your debugging.")))

(defun debug-system-instruction ()
  (content
   :parts (list (part +general-debug-system-instructions+))))

(defun debug-prompt (error-message backtrace)
  (format nil "An error has been signalled by Common Lisp.
  **Error Message**: ~a~%
  **Backtrace**: ~a~%
Please provide debugging assistance, attempt to diagnose the error, and suggest a fix."
          error-message backtrace))

(defun invoke-llm-debugger (&key error-message backtrace)
  (let ((*system-instruction* (debug-system-instruction))
        (*include-thoughts* t))
    (invoke-gemini (debug-prompt error-message backtrace))))

(defun call-with-llm-debugger-hook (thunk)
  "Creates a hook that can be used as a debugger hook for LLM-based debugging."
  (let* ((old-debugger-hook *debugger-hook*)
         (*debugger-hook*
           (lambda (condition &optional prior-debugger-hook)
             (let ((error-message (format nil "~a" condition))
                   (backtrace (with-output-to-string (s)
                                (trivial-backtrace:print-backtrace condition :output s :verbose t))))
               (let ((response (invoke-llm-debugger
                                :error-message error-message
                                :backtrace backtrace)))
                 (format t "~&LLM Debugger Response: ~a~%" response)
                 (when old-debugger-hook
                   (funcall old-debugger-hook condition prior-debugger-hook)))))))
    (funcall thunk)))

(defmacro with-llm-debugger (&body body)
  "Execute BODY with the LLM debugger hook installed."
  `(CALL-WITH-LLM-DEBUGGER-HOOK
    (LAMBDA () ,@body)))
