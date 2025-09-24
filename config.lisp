;;; -*- Lisp -*-

(in-package "GEMINI")

(defun load-gemini-config ()
  (let ((config-file (uiop/configuration:xdg-config-pathname "gemini/config.lisp")))
    (when (probe-file config-file)
      (load (uiop/configuration:xdg-config-pathname "gemini/config.lisp")))))

(eval-when (:load-toplevel :execute)
  (load-gemini-config))
