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
