;;; -*- Mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defparameter +echo-model+ "models/gemini-flash-lite-latest")
(defparameter +echo-temperature+ 0.01)
(defparameter +echo-max-output-tokens+ 1024)

;;; A recursive echo of the controlling LLM.
;;; The Holy Ghost of the trinity.

(defun dave (system-instructions prompt)
  (without-personality
    (let ((+default-model+ +echo-model+)
          (*system-instruction* (content :parts (map 'list #'part system-instructions) :role "system")))
      (invoke-gemini prompt))))

