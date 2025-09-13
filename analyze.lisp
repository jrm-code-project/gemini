;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +analyze-source-system-instruction+
  "You are a world class Common Lisp programmer.  You will be analyzing files from the '~a' system.  Your analysis should be thorough and insightful, demonstrating a deep understanding of Common Lisp programming practices."
  "LLM system instruction to analyze a Common Lisp ASDF system.")

(defparameter +analyze-system-definition-prompt+
  (str:join
   #\Space
   (list
    "The following is the definition of the `~a` system."
    "Analyze the system definition, checking for best practices."
    "Make sure there is a `package` definition with no dependencies depended upon by all other components."))
  "LLM prompt to analyze a Common Lisp ASDF system definition.")

(defun analyze-system-definition (system-name)
  "Analyze the system definition file in a specified system."
  (check-type system-name (or string symbol))
  (let ((asdf-system (asdf:find-system system-name)))
    (unless asdf-system
      (error "ASDF system '~a' not found." system-name))
    (let ((source-file (asdf:system-source-file asdf-system)))
      (unless (and source-file (probe-file source-file))
        (error "Source file for system '~a' not found." system-name))

      (let ((*system-instruction*
              (content
               :parts
               (list
                (part (format nil +analyze-source-system-instruction+ system-name)))
               :role "system"))
            (*include-thoughts* t))
        (invoke-gemini
         (list
          (part (format nil +analyze-system-definition-prompt+ system-name))
          (part (uiop:read-file-string source-file))))))))

(defparameter +analyze-source-file-prompt+
  "You have been given the following source code from the file '~a' in system '~a'.  Please analyze the code and provide a detailed summary of its functionality, purpose, and any notable features or patterns you observe.  In addition, not where the code is not following best practices, where there are potential bugs, and where the code can be improved.  Give up to three concrete, actionable suggestions for how the code could be improved or refactored.  If there are no suggestions for improvement, say so explicitly.  Indicate for each suggestion whether the change is a critical improvement or merely nice to have. Be sure to include your reasoning for each suggestion."
  "LLM prompt to analyze a Common Lisp source file.")

(defun get-system-and-component (system-name component-name)
  "Retrieve the ASDF system and component for a given component in a specified system."
  (check-type system-name (or string symbol))
  (check-type component-name (or string symbol))
  (let ((asdf-system (asdf:find-system system-name)))
    (unless asdf-system
      (error "ASDF system '~a' not found." system-name))
    (let ((component (asdf:find-component asdf-system component-name)))
      (unless component
        (error "Component '~a' not found in system '~a'." component-name system-name))
      (values asdf-system component))))

(defun get-system-and-component-contents (system-name component-name)
  "Retrieve the ASDF system, system contents, component, and component contents
 for a given component in a specified system.  The file contents are wrapped in Lisp markdown code blocks."
  (multiple-value-bind (system component) (get-system-and-component system-name component-name)
    (let ((system-pathname (asdf:system-source-file system))
          (component-pathname (asdf:component-pathname component)))
      (unless (and system-pathname (probe-file system-pathname))
        (error "Source file for system '~a' not found." system-name))
      (unless (and component-pathname (probe-file component-pathname))
        (error "Source file for component '~a' not found." component-name))
      (values system (uiop:read-file-string system-pathname)
              component (uiop:read-file-string component-pathname)))))

(defun analyze-component (system-name component-name)
  "Analyze the source code of a given file in a specified system and provide a detailed summary of its functionality, purpose, and notable features."
  (multiple-value-bind (system system-contents component component-contents)
      (get-system-and-component-contents system-name component-name)
    (declare (ignore system component))
    (let ((*system-instruction*
            (content
             :parts
             (list
              (part (format nil +analyze-source-system-instruction+ system-name)))
             :role "system"))
          (*include-thoughts* t))
      (invoke-gemini
       (list
        (part (format nil "What follows is the definition of the `~a` system.  Do not analyze this file, it is here so that you understand the dependencies within the system." system-name))
        (part system-contents)
        (part (format nil +analyze-source-file-prompt+ component-name system-name))
        (part component-contents))))))

(defparameter +analyze-file-system-instruction+
  (str:join #\newline
            (list
             (str:join #\Space
                       (list
                        "You are a world class Common Lisp programmer."
                        "You will be analyzing a Common Lisp file one top-level form at a time."
                        "Your analysis should be thorough and insightful, demonstrating a deep understanding of Common Lisp programming practices."
                        "If there is no package definition, assume an appropriate one exists elsewhere and do not mention this."
                        "Assume that popular utility packages such as `alexandria` and `series` have been loaded and made available."
                        "Assume that undefined functions are defined elsewhere and do not mention this."
                        "Do not suggest using defconstant, even if it would be appropriate."))
             "For each top-level form:"
             "  * Determine its purpose."
             "  * Determine whether a library function might achieve the same thing."
             "  * Check that it follows best practices."
             "  * Look for typical Common Lisp errors and pitfalls."
             "  * Look for bugs."
             "Then report on the form:"
             "  * Describe the purpose of the form."
             "  * If the form is missing a docstring, suggest one."
             "  * If the form fails to follow best practices, say so."
             "  * If there are bugs or errors, describe them."
             "  * If improvements can be made, give up to three concrete, actionable suggestions for improvement."
             "  * For each suggestion, indicate whether it is a critical improvement or merely nice to have."
             "  * For each suggestion, describe your reasoning."
             "  * If the form is error free and does not need improvement, say so."))
  "LLM system instruction to analyze a Common Lisp file one top-level form at a time.")

(defparameter +analyze-file-form-prompt+
  "Here is the next form in the file:"
  "LLM prompt to analyze a single top-level form in a Common Lisp file.")

(defun analyze-file (filename)
  "Analyze a Common Lisp file one top-level form at a time."
  (check-type filename (or pathname string))
  (let ((forms (file-forms filename))
        (*temperature* 0.1)
        (*include-thoughts* t)
        (*system-instruction*
          (content :parts (list (part +analyze-file-system-instruction+))
                   :role "system")))
    (unless (consp forms)
      (error "No top-level forms found in file '~a'." filename))
    (let iter ((analysis (invoke-gemini
                          (list (part +analyze-file-form-prompt+)
                                (part (car forms)))))
               (more (cdr forms)))
         (princ analysis)
         (terpri)
         (unless (null more)
           (iter (continue-gemini
                  (list (part +analyze-file-form-prompt+)
                        (part (car more))))
                 (cdr more))))))
