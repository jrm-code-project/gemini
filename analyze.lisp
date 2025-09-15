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
    "Make sure there is a primary package definition component that has no *other internal ASDF dependencies* and is depended upon by all other components."))
  "LLM prompt to analyze a Common Lisp ASDF system definition.")

(defun analyze-system-definition (system-name)
  "Analyzes the ASDF system definition file for the specified SYSTEM-NAME.
   It checks for best practices, especially concerning the primary package
   definition component and its internal ASDF dependencies, by sending
   the system definition to an LLM for detailed analysis."
  (check-type system-name (or string symbol))
  (let ((asdf-system (asdf:find-system system-name)))
    (unless asdf-system
      (error "ANALYZE-SYSTEM-DEFINITION: ASDF system '~a' not found." system-name))
    (let ((source-file (asdf:system-source-file asdf-system)))
      (unless (and source-file (probe-file source-file))
        (error "ANALYZE-SYSTEM-DEFINITION: Source file for system '~a' not found." system-name))

      (let ((*system-instruction*
              (content
               :parts
               (list
                (part (format nil +analyze-source-system-instruction+ system-name)))
               :role "system")))
        (invoke-gemini
         (list
          (part (format nil +analyze-system-definition-prompt+ system-name))
          (part (handler-case (uiop:read-file-string source-file)
                  (error (c)
                    (error "ANALYZE-SYSTEM-DEFINITION: Failed to read source file '~a': ~a"
                           source-file c))))))))))

(defparameter +analyze-source-file-prompt+
  "Given the following ASDF system definition for '~a' and the source code for component '~a', analyze the component's code and provide a detailed summary of its functionality, purpose, and any notable features or patterns you observe.  In addition, note where the code is not following best practices, where there are potential bugs, and where the code can be improved.  Give up to three concrete, actionable suggestions for how the code could be improved or refactored.  If there are no suggestions for improvement, say so explicitly.  Indicate for each suggestion whether the change is a critical improvement or merely nice to have. Be sure to include your reasoning for each suggestion."
  "LLM prompt to analyze a Common Lisp source file.")

(defun get-system-and-component (system-name component-name)
  "Retrieves the ASDF system object and the specified component object
   for a given component in a specified system. Returns two values:
   the ASDF system object and the component object. Signals an error
   if the system or component is not found."
  (check-type system-name (or string symbol))
  (check-type component-name (or string symbol))
  (let ((asdf-system (asdf:find-system system-name)))
    (unless asdf-system
      (error "GET-SYSTEM-AND-COMPONENT: ASDF system '~a' not found." system-name))
    (let ((component (asdf:find-component asdf-system component-name)))
      (unless component
        (error "GET-SYSTEM-AND-COMPONENT: Component '~a' not found in system '~a'." component-name system-name))
      (values asdf-system component))))

(defun get-system-and-component-contents (system-name component-name)
  "Retrieves the ASDF system object, its definition file content,
   the ASDF component object, and its source file content.
   Returns four values:
      1. The ASDF system object.
      2. A string containing the system definition file's content.
      3. The ASDF component object.
      4. A string containing the component's source file content.
   Signals an error if the system, component, or their respective
   source files are not found."
  (multiple-value-bind (system component) (get-system-and-component system-name component-name)
    (let ((system-pathname (asdf:system-source-file system))
          (component-pathname (asdf:component-pathname component)))
      (unless (and system-pathname (probe-file system-pathname))
        (error "GET-SYSTEM-AND-COMPONENT-CONTENTS: Source file for system '~a' not found." system-name))
      (unless (and component-pathname (probe-file component-pathname))
        (error "GET-SYSTEM-AND-COMPONENT-CONTENTS: Source file for component '~a' not found." component-name))
      (values system (handler-case (uiop:read-file-string system-pathname)
                       (error (c)
                         (error "GET-SYSTEM-AND-COMPONENT-CONTENTS: Failed to read system definition file '~a': ~a"
                                system-pathname c)))
              component (handler-case (uiop:read-file-string component-pathname)
                            (error (c)
                                (error "GET-SYSTEM-AND-COMPONENT-CONTENTS: Failed to read component source file '~a': ~a"
                                     component-pathname c)))))))

(defparameter +system-definition-context-instruction+
  "What follows is the definition of the `~a` system. Do not analyze this file, it is here so that you understand the dependencies within the system."
  "LLM instruction to provide system definition as context without analysis.")

(defun analyze-component (system-name component-name)
  "Analyzes the source code of a specific ASDF component within a given system
   using an LLM. It provides the LLM with the system definition for context
   and the component's source code. The LLM is prompted to summarize
   functionality, purpose, and features, identify deviations from best practices,
   potential bugs, and suggest up to three concrete improvements with reasoning
   and severity."
  (multiple-value-bind (system system-contents component component-contents)
      (get-system-and-component-contents system-name component-name)
    (declare (ignore system component))
    (let ((*system-instruction*
            (content
             :parts
             (list
              (part (format nil +analyze-source-system-instruction+ system-name)))
             :role "system")))
      (invoke-gemini
       (list
        (part (format nil +system-definition-context-instruction+ system-name))
        (part system-contents)
        (part (format nil +analyze-source-file-prompt+ system-name component-name))
        (part component-contents))))))

(defparameter +analyze-form-prompt+
  (str:join
   #\Newline
   (list 
    "  * Use the preceeding forms as context for analysis."
    "  * Determine the purpose of the form."
    "  * Determine whether a library function might achieve the same thing."
    "  * Check that it follows best practices."
    "  * Look for opportunities to simplify or improve the code."
    "  * Look for typical Common Lisp errors and pitfalls."
    "  * Look for bugs."
    "Then report on the form:"
    "  * Describe the purpose of the form."
    "  * Suggest a docstring if one is needed, or if it can be improved, otherwise don't mention it."
    "  * If the form fails to follow best practices, say so."
    "  * If there are bugs or errors, describe them."
    "  * If improvements can be made, give up to three concrete, actionable suggestions for improvement."
    "  * For each suggestion, indicate whether it is a critical improvement, major improvement, minor improvement, merely nice to have, or barely worth mentioning."
    "  * For each suggestion, describe your reasoning."
    "  * If the form is error free and does not need improvement, say so."))
  "LLM prompt detailing the analysis criteria and reporting format for a single Common Lisp top-level form.")

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
                        "Assume variables decorated with *earmuffs* have been declared as special variables."
                        "Implicit dependencies are expected, so do not mention them."
                        "Do not suggest using defconstant, even if it would be appropriate or a best practice."
                        "Prefer `check-type` over type declarations for robustness."))
             "For each top-level form:"
             +analyze-form-prompt+))
  "LLM system instruction to analyze a Common Lisp file one top-level form at a time.")

(defparameter +analyze-file-form-prompt+
  (str:join #\Newline
            (list "Analyze the following top-level form in a Common Lisp file." +analyze-form-prompt+))
  "LLM prompt to analyze a single top-level form in a Common Lisp file.")

(defun analyze-file (filename &key (temperature 0.01) (verbose t))
  "Analyze a Common Lisp file one top-level form at a time."
  (check-type filename (or pathname string))
  (let ((forms (file-forms filename))
        (*temperature* temperature)
        (*system-instruction*
          (content :parts (list (part +analyze-file-system-instruction+))
                   :role "system"))
        (start-prompt-tokens *accumulated-prompt-tokens*)
        (start-response-tokens *accumulated-response-tokens*)
        (start-time (get-universal-time))
        (aborted t))
    (unless (consp forms)
      (error "No top-level forms found in file '~a'." filename))
    (unwind-protect
         (prog1 (let iter ((current-form (car forms))
                           (analysis (invoke-gemini
                                      (list (part +analyze-file-form-prompt+)
                                            (part (car forms)))))
                           (results '())
                           (more (cdr forms)))
                  (when verbose
                    (format t "~&~%;; Analyzing form:~%~%~a~%--------~%~a~%~%" current-form analysis))
                  (if (null more)
                      (reverse (acons current-form analysis results))
                      (iter (car more)
                            (continue-gemini
                             (list (part +analyze-file-form-prompt+)
                                   (part (car more))))
                            (acons current-form analysis results)
                            (cdr more))))
           (setq aborted nil))
      (when verbose
        (let ((total-prompt-tokens (- *accumulated-prompt-tokens* start-prompt-tokens))
              (total-response-tokens (- *accumulated-response-tokens* start-response-tokens))
              (elapsed-time (- (get-universal-time) start-time)))
          (format t "~&;; Analysis ~:[complete in~;aborted at~] ~d seconds.~%" aborted elapsed-time)
          (format t ";; Prompt tokens:   ~7,d~%" total-prompt-tokens)
          (format t ";; Response tokens: ~7,d~%" total-response-tokens))))))
