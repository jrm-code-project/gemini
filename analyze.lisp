;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +analyze-source-system-instruction+
  "You are a world class Common Lisp programmer.  You will be analyzing files from the '~a' system.  Your analysis should be thorough and insightful, demonstrating a deep understanding of Common Lisp programming practices.")

(defparameter +analyze-source-file-prompt+
    "You have been given the following source code from the file '~a' in system '~a'.  Please analyze the code and provide a detailed summary of its functionality, purpose, and any notable features or patterns you observe.  In addition, not where the code is not following best practices, where there are potential bugs, and where the code can be improved.  Give up to three concrete, actionable suggestions for how the code could be improved or refactored.  If there are no suggestions for improvement, say so explicitly.  Indicate for each suggestion whether the change is a critical improvement or merely nice to have. Be sure to include your reasoning for each suggestion.")

(defun lisp-file-contents (source-file)
  "Return the contents of a Lisp source file in a given ASDF system."
  (str:join #\newline
            (collect 'list
              (catenate (scan (list "```lisp"))
                        (scan-file source-file #'read-line)
                        (scan (list "```"))))))

(defun analyze-system-definition (system)
  "Analyze the system definition file in a specified system."
  (let ((*system-instruction*
          (content
           :parts
           (list
            (part (format nil +analyze-source-system-instruction+ system)))
           :role "system"))
        (*include-thoughts* t))
    (invoke-gemini
     (list
      (part (format nil "What follows is the definition of the `~a` system.  Analyze this file." system))
      (part (lisp-file-contents (asdf:system-source-file (asdf:find-system system))))
      ))))

(defun analyze-source (system file)
  "Analyze the source code of a given file in a specified system and provide a detailed summary of its functionality, purpose, and notable features."
  (let ((*system-instruction*
          (content
           :parts
           (list
            (part (format nil +analyze-source-system-instruction+ system)))
           :role "system"))
        (*include-thoughts* t))
    (invoke-gemini
     (list
      (part (format nil "What follows is the definition of the `~a` system.  Do not analyze this file, it is here so that you understand the dependencies within the system." system))
      (part (lisp-file-contents (asdf:system-source-file (asdf:find-system system))))
      (part (format nil +analyze-source-file-prompt+ file system))
      (part (lisp-file-contents (asdf:component-pathname (asdf:find-component (asdf:find-system system) file))))
      ))))
