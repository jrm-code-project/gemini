;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter *enable-interaction* t
  "If true, enables the Gemini model to interact with the user via read and yes-or-no prompts.")

(defparameter *enable-lisp-introspection* t
  "If true, enables the Gemini model to introspect the Lisp environment, including functions, variables, and packages.")

(defparameter *enable-web-functions* t
  "If true, enables the Gemini model to call web functions such as HTTP GET and POST.")

(defparameter *enable-web-search* t
  "If true, enables the Gemini model to perform web searches.")

(defvar *enable-recursive-prompt* t
  "If true, enables recursive prompting of the LLM.")

(defun standard-functions-and-handlers ()
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

    (when *enable-lisp-introspection*
      (cons
       (function-declaration
        :name "canBeEvaluated"
        :description "Returns true if the given expression can be evaluated in the Lisp environment.  True for Lisp atoms and complete s-expressions, false for free-form text."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "string" (schema :type :string)))
                            :required (list "string"))
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
        :name "eval"
        :description "Evaluates an expression and returns the printed representation of the result."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "string" (schema :type :string)))
                            :required (list "string"))
        :response (schema :type :string
                          :description "The printed representation of the result of evaluating the expression, or an error message if the expression cannot be evaluated."))
       (lambda (&key string)
         (handler-case
             (let* ((narrow-string (str:trim string))
                    (length (length narrow-string)))
               (multiple-value-bind (form count) (read-from-string (str:trim string))
                 (if (= count length)
                     (format nil "~s" (eval form))
                     "Error: Expression not fully read.")))
           (error () "Error: Expression could not be evaluated.")))))

    (when *enable-web-functions*
      (cons
       (function-declaration
        :name "httpGet"
        :description "Performs an HTTP GET request to the specified URL and returns the response body as a string."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "url" (schema :type :string)))
                            :required (list "url"))
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
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :description "Checks if a symbol is fbound in the Lisp environment."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :description "Checks if the value of a symbol is a boolean in the Lisp environment."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :description "Checks if the value of a symbol is an integer in the Lisp environment."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :description "Checks if the value of a symbol is a string in the Lisp environment."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
                            :properties (list
                                         (cons
                                          "system" (schema :type :string)))
                            :required (list "system"))
        :response (schema :type :string))
       (lambda (&key system)
         (format nil "~s" (asdf:load-system system)))))

    (when *enable-lisp-introspection*
      (cons
       (function-declaration
        :name "loadQuicklispSystem"
        :description "Loads a Quicklisp system by name."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "system" (schema :type :string)))
                            :required (list "system"))
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
                          :properties (list
                                       (cons
                                        "string" (schema :type :string)))
                          :required (list "string")))
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
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
                            :properties (list
                                         (cons
                                          "prompt" (schema :type :string)))
                            :required (list "prompt"))
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
                            :properties (list
                                         (cons
                                          "prompt" (schema :type :string)))
                            :required (list "prompt"))
        :response (schema :type :string
                          :description "The LLM's response to the prompt."))
       (lambda (&key prompt)
         (let ((*enable-recursive-prompt* nil))
           (gemini-continue prompt)))))

    (cons
     (function-declaration
      :name "randomInteger"
      :description "Returns a random integer between 0 and the given maximum value (exclusive).  Use this to generate random numbers."
      :behavior :blocking
      :parameters (schema :type :object
                          :properties (list
                                       (cons
                                        "max" (schema :type :integer)))
                          :required (list "max"))
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
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
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
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "symbol" (schema :type :string)))
                            :required (list "symbol"))
        :response (schema :type :string
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
                            :properties (list
                                         (cons
                                          "term" (schema :type :string)))
                            :required (list "term"))
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
                            :properties (list
                                         (cons
                                          "system" (schema :type :string)))
                            :required (list "system"))
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
                            :properties (list
                                         (cons
                                          "searchTerms" (schema :type :string)))
                            :required (list "searchTerms"))
        :response (schema :type :object
                          :properties (list
                                       (cons "items"
                                             (schema :type :array
                                                     :items (schema :type :object
                                                                    :properties (list
                                                                                 (cons "title" (schema :type :string))
                                                                                 (cons "link" (schema :type :string))
                                                                                 (cons "snippet" (schema :type :string)))
                                                                    :required (list "title" "link" "snippet")))))
                          :required (list "items")))
       (lambda (&key search-terms)
         (format *trace-output* "~&;; Search Terms: ~{~a~^ ~}~%" (str:split " " search-terms :omit-nulls t))
         (finish-output *trace-output*)
         (let* ((results (web-search
                          (str:join "+" (str:split " " search-terms :omit-nulls t))))
                (items (get-items results))
                (response (make-hash-table :test 'equal)))
           (setf (gethash "items" response)
                 (map 'list (lambda (item)
                              (let ((response-item (make-hash-table :test 'equal)))
                                (setf (gethash "title" response-item) (get-title item)
                                      (gethash "link" response-item) (get-link item)
                                      (gethash "snippet" response-item) (get-snippet item))
                                response-item))
                      items))
           response))))

    (when *enable-interaction*
      (cons
       (function-declaration
        :name "yesOrNoP"
        :description "Asks a careful yes/no question and returns the response.  Use this for consequential questions that require a definitive yes or no answer."
        :behavior :blocking
        :parameters (schema :type :object
                            :properties (list
                                         (cons
                                          "question" (schema :type :string)))
                            :required (list "question"))
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
                            :properties (list
                                         (cons
                                          "question" (schema :type :string)))
                            :required (list "question"))
        :response (schema :type :boolean
                          :description "Returns true or false based on user input."))
       (lambda (&key question)
         (if (y-or-n-p question)
             +json-true+
             +json-false+))))
    )))

