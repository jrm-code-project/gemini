;;; -*- Mode: Lisp; coding: utf-8; -*-

(in-package "GEMINI")

(defmacro report-elapsed-time (tag &body body)
  (let ((aborted-var (gensym "ABORTED-"))
        (elapsed-time-var (gensym "ELAPSED-TIME-"))
        (start-time-var (gensym "START-TIME-"))
        (tag-var (gensym "TAG-")))
    `(let ((,aborted-var t)
           (,start-time-var (local-time:now))
           (,tag-var ,tag))
       (unwind-protect
            (progn
              (format *trace-output* "~&;; Invoking ~a...~%" ,tag-var)
              (finish-output *trace-output*)
              (prog1
                  (progn ,@body)
                (setf ,aborted-var nil)))
         (let ((,elapsed-time-var (local-time:timestamp-difference (local-time:now) ,start-time-var)))
           (format *trace-output* "~&;; ~a ~:[finished in~;aborted after~] ~,2f seconds.~%" ,tag-var ,aborted-var ,elapsed-time-var)
           (finish-output *trace-output*))))))

(defmacro future (&body body)
  "Spawns a thread to evaluate BODY, captures any fatal errors, and returns a FUTURE object."
  (let ((fut-sym (gensym "FUT"))
        (res-sym (gensym "RES")))
    `(let* ((,fut-sym (make-future)))
       (setf (future-thread ,fut-sym)
             (sb-thread:make-thread
              (lambda ()
                (let ((,res-sym (handler-case
                                    (cons :ok (progn ,@body))
                                  (error (e)
                                    (cons :error e)))))
                  (sb-thread:with-mutex ((future-lock ,fut-sym))
                    (if (eq (car ,res-sym) :ok)
                        (setf (future-state ,fut-sym) :completed
                              (future-value ,fut-sym) (cdr ,res-sym))
                        (setf (future-state ,fut-sym) :failed
                              (future-error ,fut-sym) (cdr ,res-sym))))))
              :name "Future Evaluation Thread"))
       ,fut-sym)))
