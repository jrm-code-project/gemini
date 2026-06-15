;;; -*- Lisp -*-

(in-package "GEMINI")

(defun load-gemini-config ()
  (let ((config-file (uiop/configuration:xdg-config-pathname "gemini/config.lisp")))
    (when (and config-file (probe-file config-file))
      (load config-file))))

(eval-when (:load-toplevel :execute)
  (load-gemini-config))
