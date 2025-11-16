;;; -*- Lisp -*-

(in-package "GEMINI")

(defun lisp-introspection-tools-and-handlers ()
  "Return a list of lisp-introspection-related functions and their handlers."
  (list
   (cons
    (function-declaration
     :name "alreadyLoadedSystems"
     :description "Returns a list of all ASDF systems that are already loaded in the Lisp environment."
     :behavior :blocking
     :response (schema :type :array
                       :items (schema :type :string)))
    #'asdf:already-loaded-systems)

   (cons
    (function-declaration
     :name "architecture"
     :description (or (documentation 'uiop/os:architecture 'function)
                      "Returns the architecture of the machine.")
     :behavior :blocking
     :response (schema :type :string))
    #'uiop:architecture)

   (cons
    (function-declaration
     :name "booleanp"
     :description "Predicate to check if the value of a symbol is a boolean in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to check."))
                         :required (vector :symbol))
     :response (schema :type :boolean))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym
                 (boundp sym))
            (if (or (eq (symbol-value sym) 't)
                    (eq (symbol-value sym) 'nil))
                jsonx:+json-true+
                jsonx:+json-false+)
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "boundp"
     :description "Checks if a symbol is bound to a value in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to check."))
                         :required (vector :symbol))
     :response (schema :type :boolean))
    (lambda (&key symbol)
      (let* ((name (string-upcase symbol))
             (sym (find-symbol name)))
        (if (or (and sym
                     (boundp sym))
                (and (not sym)
                     (string-equal name "NIL")))
            jsonx:+json-true+
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "checkLispSyntax"
     :description "Returns true if the given expression is a syntactically correct and complete Lisp expression.  Returns false if the expression is incomplete or has a syntax error."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :string
                                             (schema :type :string
                                                     :description "The string to check for proper syntax."))
                         :required (vector :string))
     :response (schema :type :boolean))
    (lambda (&key string)
      (handler-case
          (let* ((narrow-string (str:trim string))
                 (length (length narrow-string))
                 (*read-eval* nil))
            (declare (ignore length))
            (let ((form (read-full-forms narrow-string)))
              (if (null form)
                  jsonx:+json-false+
                  jsonx:+json-true+)))
        (error () jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "describe"
     :description "Describe the given symbol using the Common Lisp `describe` function."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to describe."))
                         :required (vector :symbol))
     :response (schema :type :string
                       :description "The description of symbol, it's value, it's documentation, and any other relevant information.  Returns an error message if the symbol is not found."))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if sym
            (with-output-to-string (*standard-output*)
              (describe sym))
            "Error: Symbol not found."))))

   (cons
    (function-declaration
     :name "eval"
     :description "**`eval(string: str)`:**
*   **Purpose:** The primary tool for executing Common Lisp code. It takes a string containing a complete Lisp expression (an s-expression or an atom) and evaluates it.
*   **Persistence:** Any definitions (e.g., using `defun` for functions, `defparameter` or `defvar` for global variables, `defclass` for object-oriented structures) made via `eval` will persist in the Lisp environment for future `eval` calls.
*   **Output:** The printed representation of the result of the evaluation is returned.
*   **Caution:** The Lisp environment is the very one in which the LLM client is running."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :string
                                             (schema :type :string
                                                     :description "The string containing the Lisp expression to evaluate.  This should be a complete s-expression or a single Lisp atom."))
                         :required (vector :string))
     :response (schema :type :array
                       :items (schema :type :string)
                       :description "The printed representation of the result of evaluating the expression, or an error message if the expression cannot be evaluated."))
    (lambda (&key string)
      (let* ((narrow-string (str:trim string))
             (length (length narrow-string)))
        (declare (ignore length))
        (if (or (eq *enable-eval* :yolo)
                (yes-or-no-p "May I evaluate ~a?" narrow-string))
            (let ((form (read-full-forms narrow-string)))
              (if form
                  (progn
                    (format *trace-output* "~&;; Evaluating: ~s~%" form)
                    (finish-output *trace-output*)
                    (let ((values (multiple-value-list (eval form))))
                      (cond ((null values) (format *trace-output* "~&;; No values~%"))
                            ((= (length values) 1)
                             (format *trace-output* "~&;; Value: ~S~%" (car values)))
                            (t (format *trace-output* "~&;; Values:~%~{;;   * ~S~%~}" values)))
                      (values-list values)))
                  (progn
                    (format *trace-output* "~&;; Incomplete expression: ~s~%" narrow-string)
                    (finish-output *trace-output*)
                    (error "End of expression reached early.  Check your expression.  Parentheses **must** be balanced."))))
            "**Evaluation rejected by the user.**"))))

   (cons
    (function-declaration
     :name "fboundp"
     :description "Checks if a symbol is bound to a function in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol 
                                             (schema :type :string
                                                     :description "The name of the symbol to check."))
                         :required (vector :symbol))
     :response (schema :type :boolean))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym
                 (fboundp sym))
            jsonx:+json-true+
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "integerp"
     :description "Predicate to check if the value of a symbol is an integer in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to check."))
                         :required (vector :symbol))
     :response (schema :type :boolean))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym
                 (boundp sym)
                 (integerp (symbol-value sym)))
            jsonx:+json-true+
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "lispImplementationType"
     :description (or (documentation 'lisp-implementation-type 'function)
                      "Returns the type of the Lisp implementation.")
     :behavior :blocking
     :response (schema :type :string))
    #'lisp-implementation-type)

   (cons
    (function-declaration
     :name "lispImplementationVersion"
     :description (or (documentation 'lisp-implementation-version 'function)
                      "Returns the version of the Lisp implementation.")
     :behavior :blocking
     :response (schema :type :string))
    #'lisp-implementation-version)

   (cons
    (function-declaration
     :name "listAllLocalAsdfSystems"
     :description "Returns a list of all ASDF systems that can be loaded in the Lisp environment."
     :behavior :blocking
     :response (schema :type :array
                       :items (schema :type :string)))
    #'list-all-local-asdf-systems)

   (cons
    (function-declaration
     :name "listPackages"
     :description "Returns a list of all packages in the Lisp environment."
     :behavior :blocking
     :response (schema :type :array
                       :items (schema :type :string)))
    (lambda () (map 'list #'package-name (list-all-packages))))

   (cons
    (function-declaration
     :name "loadableAsdfSystems"
     :description "Returns a list of all ASDF systems that are loadable, but not yet loaded in the Lisp environment."
     :behavior :blocking
     :response (schema :type :array
                       :items (schema :type :string)))
    (lambda ()
      (set-difference (list-all-local-asdf-systems) (asdf:already-loaded-systems) :test #'equal)))

   (cons
    (function-declaration
     :name "loadAsdfSystem"
     :description "Loads an ASDF system by name."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :system
                                             (schema :type :string
                                                     :description "The name of the system to load."))
                         :required (vector :system))
     :response (schema :type :string))
    (lambda (&key system)
      (format nil "~s" (asdf:load-system system))))

   (cons
    (function-declaration
     :name "loadQuicklispSystem"
     :description "Loads a Quicklisp system by name."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :system
                                             (schema :type :string
                                                     :description "The name of the system to load."))
                         :required (vector :system))
     :response (schema :type :string))
    (lambda (&key system)
      (format nil "~s" (ql:quickload system))))

   (cons
    (function-declaration
     :name "macroexpand"
     :description (or (documentation 'macroexpand 'function)
                      "Invokes the Common Lisp `macroexpand` function on the given expression and returns the result.")
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :expression
                                             (schema :type :string
                                                     :description "The string containing the Lisp expression to macroexpand.  This should be a complete s-expression."))
                         :required (vector :expression))
     :response (schema :type :array
                       :items (schema :type :string)
                       :description "The printed representation of the result of macroexpanding the expression, or an error message if the expression cannot be macroexpanded."))
    (lambda (&key expression)
      (let* ((narrow-string (str:trim expression))
             (length (length narrow-string)))
        (declare (ignore length))
        (let ((form (read-full-forms narrow-string)))
          (if form
              (let ((expanded (multiple-value-list (macroexpand form))))
                (format *trace-output* "~{~s~^~%~}" expanded)
                expanded)
              (progn
                (format *trace-output* "~&;; Incomplete expression: ~s~%" narrow-string)
                (finish-output *trace-output*)
                (error "End of expression reached early.  Check your expression.  Parentheses **must** be balanced.")))))))

   (cons
    (function-declaration
     :name "macroexpand-1"
     :description (or (documentation 'macroexpand-1 'function)
                      "Invokes the Common Lisp `macroexpand-1` function on the given expression and returns the result.")
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :expression
                                             (schema :type :string
                                                     :description "The string containing the Lisp expression to macroexpand.  This should be a complete s-expression."))
                         :required (vector :expression))
     :response (schema :type :string
                       :description "The printed representation of the result of macroexpanding the expression, or an error message if the expression cannot be macroexpanded."))
    (lambda (&key expression)
      (let* ((narrow-string (str:trim expression))
             (length (length narrow-string)))
        (declare (ignore length))
        (let ((form (read-full-forms narrow-string)))
          (if form
              (let ((expanded (multiple-value-list (macroexpand-1 form))))
                (format *trace-output* "~{~s~^~%~}" expanded)
                expanded)
              (progn
                (format *trace-output* "~&;; Incomplete expression: ~s~%" narrow-string)
                (finish-output *trace-output*)
                (error "End of expression reached early.  Check your expression.  Parentheses **must** be balanced.")))))))

   (cons
    (function-declaration
     :name "printSymbolValue"
     :description "Returns the printed representation of the value of a symbol in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to retrieve the value of."))
                         :required (vector :symbol))
     :response (schema :type :string
                       :description "The printed representation of the value of symbol, or an error message if the symbol is not found."))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym (boundp sym))
            (format nil "~s" (symbol-value sym))
            ""))))

   (cons
    (function-declaration
     :name "stringp"
     :description "Predicate to check if the value of a symbol is a string in the Lisp environment."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :symbol
                                             (schema :type :string
                                                     :description "The name of the symbol to check."))
                         :required (vector :symbol))
     :response (schema :type :boolean))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym
                 (boundp sym)
                 (stringp (symbol-value sym)))
            jsonx:+json-true+
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "symbolValueAsBoolean"
     :description "Returns the boolean value of a symbol in the Lisp environment.  Returns false if symbol is not bound to a boolean."
     :behavior :blocking
     :parameters (schema
                  :type :object
                  :properties (object
                               :symbol (schema
                                        :type :string
                                        :description "The name of the symbol to retrieve the value of."))
                  :required (vector :symbol))
     :response (schema :type :string
                       :description "The boolean value of symbol."))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym (boundp sym) (symbol-value sym))
            jsonx:+json-true+
            jsonx:+json-false+))))

   (cons
    (function-declaration
     :name "symbolValueAsInteger"
     :description "Returns the integer value of a symbol in the Lisp environment.  Returns 0 if symbol value is not an integer."
     :behavior :blocking
     :parameters (schema
                  :type :object
                  :properties (object
                               :symbol (schema
                                        :type :string
                                        :description "The name of the symbol to retrieve the value of."))
                  :required (vector :symbol))
     :response (schema :type :string
                       :description "The integer value of symbol."))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym (boundp sym) (integerp (symbol-value sym)))
            (symbol-value sym)
            0))))

   (cons
    (function-declaration
     :name "symbolValueAsString"
     :description "Returns the value of a symbol in the Lisp environment."
     :behavior :blocking
     :parameters (schema
                  :type :object
                  :properties (object :symbol
                                      (schema :type :string
                                              :description "The name of the symbol to retrieve the value of."))
                  :required (vector :symbol))
     :response (schema
                :type :string
                :description "The string value of symbol.  Return the empty string if symbol is not bound to a string."))
    (lambda (&key symbol)
      (let ((sym (find-symbol (string-upcase symbol))))
        (if (and sym
                 (boundp sym)
                 (stringp (symbol-value sym)))
            (symbol-value sym)
            ""))))

   (cons
    (function-declaration
     :name "systemApropos"
     :description "Returns a list of systems available to load via Quicklisp apropos of a search string."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :term
                                             (schema :type :string
                                                     :description "The search term to use for the apropos search."))
                         :required (vector :term))
     :response (schema :type :array
                       :items (schema :type :string)))
    (lambda (&key term)
      (mapcar #'ql-dist:name (ql:system-apropos-list term))))

   (cons
    (function-declaration
     :name "systemDescription"
     :description "Returns a description of a system available in ASDF."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :system
                                             (schema :type :string
                                                     :description "The name of the system to describe."))
                         :required (vector :system))
     :response (schema :type :array
                       :items (schema :type :string)))
    (lambda (&key system)
      (asdf:system-description (asdf:find-system system))))

   (cons
    (function-declaration
     :name "systemList"
     :description "Returns a list of systems available to load via Quicklisp."
     :behavior :blocking
     :response (schema :type :array
                       :items (schema :type :string)))
    (lambda ()
      (mapcar #'ql-dist:name (ql:system-list))))
   ))
