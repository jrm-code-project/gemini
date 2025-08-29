;;; -*- Lisp -*-

(in-package "GEMINI")

(defun standard-functions-and-handlers ()
  "Return a list of standard functions and their handlers."
  (append
   (mcp-functions-and-handlers)

   (remove
    nil
    (list

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "alreadyLoadedSystems"
         :description "Returns a list of all ASDF systems that are already loaded in the Lisp environment."
         :behavior :blocking
         :response (schema :type :array
                           :items (schema :type :string)))
        #'asdf:already-loaded-systems))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "architecture"
         :description (or (documentation 'uiop/os:architecture 'function)
                          "Returns the architecture of the machine.")
         :behavior :blocking
         :response (schema :type :string))
        #'uiop:architecture))

     (when *enable-bash*
       (cons
        (function-declaration
         :name "bash"
         :description "Run a subprogram under the bash shell."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :command
                                                 (schema :type :string
                                                         :description "The subprogram to run.")
                                                 :arguments
                                                 (schema :type :array
                                                         :items (schema :type :string)
                                                         :description "The arguments to pass to the program."))
                             :required (vector :command :arguments))
         :response (schema :type :string
                           :description "The standard output of the command, or an error message if the command failed."))
        (lambda (&key command arguments)
          (uiop:run-program
           (cons command (coerce arguments 'list))
           :output :string
           :error-output :string
           :ignore-error-status t))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "canBeEvaluated"
         :description "Returns true if the given expression can be evaluated in the Lisp environment.  True for Lisp atoms and complete s-expressions, false for free-form text."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :string
                                                 (schema :type :string
                                                         :description "The string to check if it is a complete s-expression or a single Lisp atom."))
                             :required (vector :string))
         :response (schema :type :boolean))
        (lambda (&key string)
          (handler-case
              (let* ((narrow-string (str:trim string))
                     (length (length narrow-string))
                     (*read-eval* nil))
                (multiple-value-bind (form count) (read-from-string (str:trim string))
                  (declare (ignore form))
                  (if (= count length)
                      +json-true+
                      +json-false+)))
            (error () +json-false+)))))

     (when *enable-lisp-introspection*
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
                "Error: Symbol not found.")))))

     (when *enable-eval*
       (cons
        (function-declaration
         :name "eval"
         :description "Evaluates a Lisp expression and returns the printed representation of the result.  Gives access to a persistent Lisp environment.  Use this to define Lisp programs for later use or to call previously defined Lisp programs.  If evaluation can produce permanent side effects, get positive confirmation from the user before calling this function."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :string
                                                 (schema :type :string
                                                         :description "The string containing the Lisp expression to evaluate.  This should be a complete s-expression or a single Lisp atom."))
                             :required (vector :string))
         :response (schema :type :string
                           :description "The printed representation of the result of evaluating the expression, or an error message if the expression cannot be evaluated."))
        (lambda (&key string)
          ;; (terpri)
          ;; (write-string "Expression follows:")
          ;; (terpri)
          ;; (write-string string)
          ;; (terpri)
          ;; (finish-output)
          (handler-case
              (let* ((narrow-string (str:trim string))
                     (length (length narrow-string)))
                (if (or (eq *enable-eval* :yolo)
                        (yes-or-no-p "Do you really want to evaluate ~a?" narrow-string))
                    (multiple-value-bind (form count) (read-from-string narrow-string)
                      (if (= count length)
                          (progn
                            (format *trace-output* "~&;; Evaluating: ~s~%" form)
                            (finish-output *trace-output*)
                            (let ((values (multiple-value-list (eval form))))
                              (cond ((null values) (format nil "~&;; No values~%"))
                                    ((= (length values) 1)
                                     (format nil "~&;; Value: ~S~%" (car values)))
                                    (t (format nil "~&;; Values:~%~{;;   * ~S~%~}" values)))))
                          "Error: Expression not fully read."))
                    "**Evaluation rejected by the user.**"))
            (error () "Error: Expression could not be evaluated.")))))

     (when *enable-web-functions*
       (cons
        (function-declaration
         :name "httpGet"
         :description "Performs an HTTP GET request to the specified URL and returns the response body as a string."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :url
                                                 (schema :type :string
                                                         :description "The URL to send the GET request to."))
                             :required (vector :url))
         :response (schema :type :string))
        (lambda (&key url)
          (dexador:get url))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "isSymbolBound"
         :description "Checks if a symbol is bound in the Lisp environment."
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
                +json-true+
                +json-false+)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "isSymbolFbound"
         :description "Predicte to check if a symbol is fbound in the Lisp environment."
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
                +json-true+
                +json-false+)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "isSymbolValueBoolean"
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
                    +json-true+
                    +json-false+)
                +json-false+)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "isSymbolValueInteger"
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
                +json-true+
                +json-false+)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "isSymbolValueString"
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
                +json-true+
                +json-false+)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "lispImplementationType"
         :description (or (documentation 'lisp-implementation-type 'function)
                          "Returns the type of the Lisp implementation.")
         :behavior :blocking
         :response (schema :type :string))
        #'lisp-implementation-type))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "lispImplementationVersion"
         :description (or (documentation 'lisp-implementation-version 'function)
                          "Returns the version of the Lisp implementation.")
         :behavior :blocking
         :response (schema :type :string))
        #'lisp-implementation-version))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "listAllLocalAsdfSystems"
         :description "Returns a list of all ASDF systems that can be loaded in the Lisp environment."
         :behavior :blocking
         :response (schema :type :array
                           :items (schema :type :string)))
        #'list-all-local-asdf-systems))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "listPackages"
         :description "Returns a list of all packages in the Lisp environment."
         :behavior :blocking
         :response (schema :type :array
                           :items (schema :type :string)))
        (lambda () (map 'list #'package-name (list-all-packages)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "loadableAsdfSystems"
         :description "Returns a list of all ASDF systems that are loadable, but not yet loaded in the Lisp environment."
         :behavior :blocking
         :response (schema :type :array
                           :items (schema :type :string)))
        (lambda ()
          (set-difference (list-all-local-asdf-systems) (asdf:already-loaded-systems) :test #'equal))))

     (when *enable-lisp-introspection*
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
          (format nil "~s" (asdf:load-system system)))))

     (cons
      (function-declaration
       :name "loadContext"
       :description "Loads a Gemini context file by path."
       :behavior :blocking
       :parameters (schema
                    :type :object
                    :properties (object
                                 :filename (schema
                                            :type :string
                                            :description "The file that holds the context."))
                    :required (vector :filename))
       :response (schema :type :string))
      (lambda (&key filename)
        (format *trace-output* "~&Loading ~s...~%" filename)
        (finish-output *trace-output*)
        (let ((pathname
               (merge-pathnames
                (make-pathname :directory (list :relative "contexts")
                               :name filename
                               :type "json")
                (user-homedir-pathname))))
          (with-open-file (stream pathname :direction :input)
            (do ((item (read stream nil nil) (read stream nil nil))
                 (context nil (cons (with-decoder-jrm-semantics
                                      (cl-json:decode-json-from-string item))
                                    context)))
                ((null item) (setq *context* (cons (car *context*) context)))))
          (format *trace-output* "~&Loaded ~s...~%" filename)
          (finish-output *trace-output*)
          (values "loaded"))))

     (when *enable-lisp-introspection*
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
          (format nil "~s" (ql:quickload system)))))

     (cons
      (function-declaration
       :name "longSiteName"
       :description (or (documentation 'long-site-name 'function)
                        "Returns the long site name of the machine.")
       :behavior :blocking
       :response (schema :type :string))
      (lambda ()
        (let ((long-name (long-site-name)))
          (if (stringp long-name)
              long-name
              "Unknown")))) 

     (cons
      (function-declaration
       :name "machineInstance"
       :description (or (documentation 'machine-instance 'function)
                        "Returns the name of the machine.")
       :behavior :blocking
       :response (schema :type :string))
      #'machine-instance)

     (cons
      (function-declaration
       :name "machineType"
       :description (or (documentation 'machine-type 'function)
                        "Returns the type of the machine.")
       :behavior :blocking
       :response (schema :type :string))
      #'machine-type)

     (cons
      (function-declaration
       :name "machineVersion"
       :description (or (documentation 'machine-version 'function)
                        "Returns the version of the machine.")
       :behavior :blocking
       :response (schema :type :string))
      (lambda ()
        (let ((version (machine-version)))
          (if (stringp version)
              version
              "Unknown"))))

     (cons
      (function-declaration
       :name "noHandler"
       :description "This function is missing its handler.  Used for testing purposes.")
      nil)

     (cons
      (function-declaration
       :name "operatingSystem"
       :description (documentation 'uiop:operating-system 'function)
       :behavior :blocking
       :response (schema :type :string))
      (lambda () (format nil "~s" (uiop:operating-system))))

     (cons
      (function-declaration
       :name "ping"
       :description "Detects whether the model client is responding to function calls.")
      (lambda () (values)))

     (cons
      (function-declaration
       :name "print"
       :description "Print a string for display to the user."
       :behavior :blocking
       :parameters (schema :type :object
                           :properties (object :string
                                               (schema :type :string
                                                       :description "The string to print."))
                           :required (vector :string)))
      (lambda (&key string)
        (print string)
        (values)))

     (when *enable-lisp-introspection*
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
                "")))))

     (when *enable-interaction*
       (cons
        (function-declaration
         :name "promptingRead"
         :description "Prompts the user for input and returns the response.  Do not hesitate to use this function to ask questions of the user or to get input from the user."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :prompt
                                                 (schema :type :string
                                                         :description "The prompt to send to the user.  This should be a complete sentence or question."))
                             :required (vector :prompt))
         :response (schema :type :string
                           :description "The user's input response."))
        (lambda (&key prompt)
          (prompting-read prompt))))

     (when *enable-recursive-prompt*
       (cons
        (function-declaration
         :name "promptLLM"
         :description "Prompts the LLM with a string and returns the response.  Use this to ask questions of the LLM or to get input from the LLM."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :prompt
                                                 (schema :type :string
                                                         :description "The prompt to send to the LLM.  This should be a complete sentence or question."))
                             :required (vector :prompt))
         :response (schema :type :string
                           :description "The LLM's response to the prompt."))
        (lambda (&key prompt)
          (let ((*enable-recursive-prompt* nil))
            (continue-gemini prompt)))))

     (cons
      (function-declaration
       :name "randomInteger"
       :description "Returns a random integer between 0 and the given maximum value (exclusive).  Use this to generate random numbers."
       :behavior :blocking
       :parameters (schema
                    :type :object
                    :properties (object
                                 :max (schema
                                       :type :integer
                                       :description "The maximum value for the random integer.  Must be a positive integer."))
                    :required (vector :max))
       :response (schema :type :integer))
      (lambda (&key max)
        (if (and (integerp max) (> max 0))
            (let ((random-integer (random max)))
              (if (integerp random-integer)
                  random-integer
                  0))
            0)))

     (cons
      (function-declaration
       :name "shortSiteName"
       :description (or (documentation 'short-site-name 'function)
                        "Returns the short site name of the machine.")
       :behavior :blocking
       :response (schema :type :string))
      (lambda ()
        (let ((short-name (short-site-name)))
          (if (stringp short-name)
              short-name
              "Unknown"))))

     (when *enable-lisp-introspection*
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
                +json-true+
                +json-false+)))))

     (when *enable-lisp-introspection*
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
                0)))))

     (when *enable-lisp-introspection*
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
                "")))))

     (when *enable-lisp-introspection*
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
          (mapcar #'ql-dist:name (ql:system-apropos-list term)))))

     (when *enable-lisp-introspection*
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
          (asdf:system-description (asdf:find-system system)))))

     (when *enable-lisp-introspection*
       (cons
        (function-declaration
         :name "systemList"
         :description "Returns a list of systems available to load via Quicklisp."
         :behavior :blocking
         :response (schema :type :array
                           :items (schema :type :string)))
        (lambda ()
          (mapcar #'ql-dist:name (ql:system-list)))))

     (when (and *enable-web-search*
                (custom-search-engine-id)
                (custom-search-engine-api-key))
       (cons
        (function-declaration
         :name "webSearch"
         :description "Search the Web for pages about a topic."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :search-terms
                                                 (schema :type :string
                                                         :description "The search terms to use for the web search.  Use spaces to separate terms."))
                             :required (vector :search-terms))
         :response (schema :type :object
                           :properties (object :items
                                               (schema :type :array
                                                       :items (schema :type :object
                                                                      :properties (object 
                                                                                   :title (schema :type :string)
                                                                                   :link (schema :type :string)
                                                                                   :snippet (schema :type :string))
                                                                      :required (vector :link :snippet :title))))
                           :required (vector :items)))
        (lambda (&key search-terms)
          (format *trace-output* "~&;; Search Terms: ~{~a~^ ~}~%" (str:split " " search-terms :omit-nulls t))
          (finish-output *trace-output*)
          (object :items
                  (map 'list (lambda (item)
                               (object :title (get-title item)
                                       :link (get-link item)
                                       :snippet (get-snippet item))
                               )
                       (get-items
                        (web-search
                         (str:join "+" (str:split " " search-terms :omit-nulls t)))))))))

     (when *enable-interaction*
       (cons
        (function-declaration
         :name "yesOrNoP"
         :description "Asks a careful yes/no question and returns the response.  Use this for consequential questions that require a definitive yes or no answer."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :question
                                                 (schema :type :string
                                                         :description "The question to ask the user."))
                             :required (vector :question))
         :response (schema :type :boolean
                           :description "Returns true or false based on user input."))
        (lambda (&key question)
          (if (yes-or-no-p question)
              +json-true+
              +json-false+))))

     (when *enable-interaction*
       (cons
        (function-declaration
         :name "yOrNP"
         :description "Asks a y/n question and returns the response.  Use this for simple yes/no questions that do not require a careful answer."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :question
                                                 (schema :type :string
                                                         :description "The question to ask the user."))
                             :required (vector :question))
         :response (schema :type :boolean
                           :description "Returns true or false based on user input."))
        (lambda (&key question)
          (if (y-or-n-p question)
              +json-true+
              +json-false+))))
     ))))

(defun mcp-functions-and-handlers ()
  "Extracts the list of functions supplied by the MCP servers."
  (fold-left (binary-compose-right #'append #'get-mcp-functions-and-handlers) nil *mcp-clients*))

(defun convert-tool (mcp-client tool)
  "Converts an MCP tool to a function specification."
  (let ((input-schema (get-input-schema tool))
        (output-schema (get-output-schema tool)))

    (cons (function-declaration
           :name (format nil "~a" (get-name tool))
           :description (get-description tool)
           :behavior :blocking
           :parameters (encode-schema-type input-schema)
           :response (encode-schema-type output-schema))
          (lambda (&rest args &key &allow-other-keys)
            (call-tool mcp-client tool (plist-hash-table args))))))

(defun get-mcp-functions-and-handlers (mcp-client)
  "Returns the functions and handlers for the given MCP client."
  (when (has-tools-capability? mcp-client)
    (map 'list (lambda (tool) (convert-tool mcp-client tool)) (get-tools mcp-client))))
