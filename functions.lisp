;;; -*- Lisp -*-

(in-package "GEMINI")

(defun read-full-forms (string)
  (let ((eof-value (cons nil nil)))
    (let iter ((forms '())
               (pos 0))
      (cond ((= pos (length string))
             (if (null (cdr forms))
                 (car forms)
                 (cons 'progn (reverse forms))))
            ((or (char= (char string pos) #\Space)
                 (char= (char string pos) #\Return)
                 (char= (char string pos) #\Newline)
                 (char= (char string pos) #\Tab))
             (iter forms (1+ pos)))
            (t (multiple-value-bind (form limit)
                   (read-from-string string nil eof-value :start pos)
                 (cond ((equal form eof-value)
                        (error "Could not read a full form from the string starting at position ~d: ~a" pos string))
                       (t (iter (cons form forms) limit)))))))))

(defun handle-tilde (namestring)
  (cond ((string= namestring "~")
         (namestring (user-homedir-pathname)))
        ((str:starts-with? "~/" namestring)
         (concatenate 'string
                      (namestring (user-homedir-pathname))
                      (subseq namestring 2)))
        (t namestring)))

(defun standard-functions-and-handlers ()
  "Return a list of standard functions and their handlers."
  (macrolet ((gnutil (name description)
               `(when *enable-gnutils*
                  (cons
                   (function-declaration
                    :name ,name
                    :description ,(format nil "Runs the `~a` command.  ~a" name description)
                    :behavior :blocking
                    :parameters (schema
                                 :type :object
                                 :properties (object
                                              :arguments (schema
                                                          :type :array
                                                          :items (schema :type :string)
                                                          :description ,(format nil "The arguments to pass to the ~a command." name)))
                                 :required (vector :arguments))
                    :response (schema
                               :type :string
                               :description "The standard output of the command, or an error message if the command failed."))
                   (lambda (&key arguments)
                     (uiop:run-program
                      (cons ,name (coerce arguments 'list))
                      :output :string
                      :error-output :string
                      :ignore-error-status t))))))

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

       (when *enable-evolution*
         (cons
          (function-declaration
           :name "appendSystemInstruction"
           :description "Appends an instruction to the system instruction used by the LLM."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :instruction
                                                   (schema :type :string
                                                           :description "The instruction to append to the system instruction."))
                               :required (vector :instruction)))
          (lambda (&key instruction)
            (append-evolvable-system-instruction instruction))))

       (when *enable-lisp-introspection*
         (cons
          (function-declaration
           :name "architecture"
           :description (or (documentation 'uiop/os:architecture 'function)
                            "Returns the architecture of the machine.")
           :behavior :blocking
           :response (schema :type :string))
          #'uiop:architecture))

       (gnutil "awk" "Use this command to transform structured files.")

       (when *enable-bash*
         (cons
          (function-declaration
           :name "bash"
           :description "**This is the `bash` shell tool.  This interface allows you to excute arbitrary
command-line programs, scripts, and system utilities directly within the underlying operating system as
non-interactive subprocesses.**

When you utilize the `bash` tool, you provide:
*   The `command` you wish to run (e.g., `ls`, `grep`, `python`, `rm`).
*   A list of `arguments` to pass to that command.

Upon execution, the system runs this command, and its complete standard output (stdout) and standard error (stderr) streams are captured and returned to you as a single string. This means you will receive all text generated by the command, whether it's a successful result or an error message.

This `bash` access empowers you to perform a wide array of system-level operations, including but not limited to:

*   **File System Management:** Create, delete, move, copy, read, and write files and directories (e.g., `mkdir`, `rm`, `mv`, `cp`, `cat`, `echo \"content\" > file.txt`).
*   **Text Processing and Data Extraction:** Search for specific patterns within file contents (`grep`), perform stream editing (`sed`), or parse structured text (`awk`).
*   **System Information Retrieval:** Query details about the system, running processes (`ps`), disk usage (`df`), or network configuration.
*   **Executing Programs and Scripts:** Run any executable program or script (e.g., Python scripts, shell scripts) that is present and accessible within the environment.
*   **Chaining Commands:** You can combine multiple shell commands using standard `bash` operators (e.g., `|` for pipes, `&&` for conditional execution) within a single `bash` tool call to achieve more complex workflows.

**Important Considerations for Usage:**

*   **Non-Interactive:** Each call to the `bash` tool executes a command as a one-off subprocess. You **cannot** maintain an ongoing, interactive shell session (e.g., you cannot launch `vim` and then type commands into it). Commands must be designed to run to completion and return their output.
*   **Error Handling:** Always be prepared to parse the output for error messages, as `stderr` is included."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :command
                                                   (schema :type :string
                                                           :description "The `command` you wish to run (e.g., `ls`, `grep`, `python`, `rm`).")
                                                   :arguments
                                                   (schema :type :array
                                                           :items (schema :type :string)
                                                           :description "A list of arguments to pass to the command."))
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
                  jsonx:+json-false+)))))

       (when *enable-lisp-introspection*
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
                  jsonx:+json-false+)))))

       (when *enable-lisp-introspection*
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
              (error () jsonx:+json-false+)))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "createDirectory"
           :description "Creates a directory or folder at the specified path.  This is the preferred way to create a directory, as it will create any necessary parent directories as well."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :directory
                                                   (schema :type :string
                                                           :description "The directory to create."))
                               :required (vector :directory))
           :response (schema :type :string))
          (lambda (&key directory)
            (ensure-directories-exist (parse-namestring (handle-tilde directory))))))

       (gnutil "curl" "Use this command to fetch http pages with complex parameters.")

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "currentDirectory"
           :description "Returns the current directory pathname.  This is the preferred way to get the current directory, as it will return the directory in a consistent format across different operating systems."
           :behavior :blocking
           :response (schema :type :string))
          (lambda ()
            (namestring *default-pathname-defaults*))))

       (when *enable-evolution*
         (cons
          (function-declaration
           :name "deleteSystemInstruction"
           :description "Deletes an instruction from the system instruction used by the LLM."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :index
                                                   (schema :type :integer
                                                           :description "The index of the instruction to delete from the system instruction."))
                               :required (vector :index)))
          (lambda (&key index)
            (delete-evolvable-system-instruction index))))

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
                  "**Evaluation rejected by the user.**")))))

       (when *enable-lisp-introspection*
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
                  jsonx:+json-false+)))))

       (gnutil "find" "Use this command to search the file system.")

       (cons
        (function-declaration
         :name "getenv"
         :description "Returns the value of an environment variable."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :variable
                                                 (schema :type :string
                                                         :description "The name of the environment variable to get."))
                             :required (vector :variable))
         :response (schema :type :string))
        (lambda (&key variable)
          (uiop:getenv variable)))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "git"
           :description "Runs a git command and returns the output as a string.  Use this command to interact with git repositories.  You can read and write files, create branches, commit changes, and push to remote repositories as needed."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :arguments
                                                   (schema :type :array
                                                           :items (schema :type :string)
                                                           :description "The arguments to pass to the git command."))
                               :required (vector :arguments))
           :response (schema :type :string
                             :description "The standard output of the command, or an error message if the command failed."))
          (lambda (&key arguments)
            (uiop:run-program
             (cons "git" (coerce arguments 'list))
             :output :string
             :error-output :string
             :ignore-error-status t))))

       (gnutil "grep" "Use this command to search for files with a certain regular expression.")

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

       (when (and *enable-web-search*
                  (google:hyperspec-search-engine-id)
                  (google:search-engine-api-key))
         (cons
          (function-declaration
           :name "hyperspecSearch"
           :description "Search the Common Lisp Hyperspec for pages about a topic."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :search-terms
                                                   (schema :type :string
                                                           :description "The search terms to use for the hyperspec search.  Use spaces to separate terms."))
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
                          (google:hyperspec-search
                           (str:join "+" (str:split " " search-terms :omit-nulls t)))))))))

       (when *enable-evolution*
         (cons
          (function-declaration
           :name "insertSystemInstruction"
           :description "Inserts an instruction at an index into the system instruction used by the LLM."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :index
                                                   (schema :type :integer
                                                           :description "The index of the instruction to delete from the system instruction.")
                                                   :instruction
                                                   (schema :type :string
                                                           :description "The instruction to insert into the system instruction."))
                               :required (vector :index :instruction)))
          (lambda (&key index instruction)
            (insert-evolvable-system-instruction index instruction))))

       (when *enable-lisp-introspection*
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
                  jsonx:+json-false+)))))

       (gnutil "jq" "Use this command to parse JSON objects.")

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "listDirectory"
           :description "Returns the files in the given directory or folder as a list of strings.  This is the preferred way to determine the contents of a directory.  Only returns a list of strings, does not print the files."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :directory
                                                   (schema :type :string
                                                           :description "The directory to list the files of."))
                               :required (vector :directory))
           :response (schema :type :array
                             :items (schema :type :string)))
          (lambda (&key directory)
            (map 'vector #'namestring
                 (directory
                  (merge-pathnames (make-pathname :name :wild
                                                  :type :wild)
                                   (ensure-directory-pathname (handle-tilde directory))))))))

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

       (when *enable-lisp-introspection*
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
                      (error "End of expression reached early.  Check your expression.  Parentheses **must** be balanced."))))))))

       (when *enable-lisp-introspection*
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
                      (error "End of expression reached early.  Check your expression.  Parentheses **must** be balanced."))))))))

       (cons
        (function-declaration
         :name "memorize"
         :description "Commits a memory to permanent storage."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :memory
                                                 (schema :type :string
                                                         :description "The memory to store permanently."))
                             :required (vector :memory)))
        (lambda (&key memory)
          (memorize-memory memory)))

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
         :description "Print a string.  Use this to display output to the user."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :string
                                                 (schema :type :string
                                                         :description "The string to print."))
                             :required (vector :string)))
        (lambda (&key string)
          (princ string)
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

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "probeFile"
           :description "Checks for the existence of a file or directory.  This is the preferred way to check if a file or directory exists."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :directory
                                                   (schema :type :string
                                                           :description "The pathname to probe."))
                               :required (vector :directory))
           :response (schema :type :string))
          (lambda (&key directory)
            (probe-file (parse-namestring (handle-tilde directory))))))

       (when *enable-interaction*
         (cons
          (function-declaration
           :name "promptingRead"
           :description "Prompts the user for input and returns the response.  Do not hesitate to use this function to ask questions of the user or to get input from the user."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :user-prompt
                                                   (schema :type :string
                                                           :description "The prompt to send to the user.  This should be a complete sentence or question."))
                               :required (vector :user-prompt))
           :response (schema :type :string
                             :description "The user's input response."))
          (lambda (&key user-prompt)
            (prompting-read user-prompt))))

       (when *enable-recursive-prompt*
         (cons
          (function-declaration
           :name "promptLLM"
           :description "Prompts the LLM with a string and returns the response.  Use this to ask questions of the LLM or to get input from the LLM."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :llm-prompt
                                                   (schema :type :string
                                                           :description "The prompt to send to the LLM.  This should be a complete sentence or question."))
                               :required (vector :llm-prompt))
           :response (schema :type :string
                             :description "The LLM's response to the prompt."))
          (lambda (&key llm-prompt)
            (let ((*enable-recursive-prompt* nil))
              (continue-gemini llm-prompt)))))

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

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "readFileBlob"
           :description "Returns the contents of a file as a blob.  This is the preferred way to read a binary file."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :pathname
                                                   (schema :type :string
                                                           :description "The pathname of the binray file to read.")
                                                   :mime-type
                                                   (schema :type :string
                                                           :description "The MIME type of the file.  Defaults to 'application/octet-stream'."))
                               :required (vector :pathname))
           :response (schema :type :object))
          (lambda (&key pathname (mime-type "application/octet-stream"))
            (format *trace-output* "~&Reading binary file: ~a~%" (handle-tilde pathname))
            (finish-output *trace-output*)
            (object
             :data (file->blob (handle-tilde pathname))
             :mime-type mime-type))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "readFileLines"
           :description "Returns the lines of a file as a vector of strings.  This is the preferred way to read the contents of a text file."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :pathname
                                                   (schema :type :string
                                                           :description "The pathname of the text file to read."))
                               :required (vector :pathname))
           :response (schema :type :array
                             :items (schema :type :string)))
          (lambda (&key pathname)
            (format *trace-output* "~&Reading file: ~a~%" (handle-tilde pathname))
            (finish-output *trace-output*)
            (collect 'vector
              (scan-file (handle-tilde pathname) #'read-line)))))

       (when *enable-evolution*
         (cons
          (function-declaration
           :name "readSystemInstruction"
           :description "Reads the system instruction used by the LLM at a particular index."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :index
                                                   (schema :type :integer
                                                           :description "The index of the instruction to delete from the system instruction."))
                               :required (vector :index))
           :response (schema :type :string))
          (lambda (&key index)
            (read-evolvable-system-instruction index))))

         (cons
          (function-declaration
           :name "reminisce"
           :description "Recalls what has been memorized."
           :behavior :blocking
           :response (schema :type :string))
          (lambda ()
            (reminisce)))

       (gnutil "sed" "Use this command to perform stream editing on text files.")

       (cons
        (function-declaration
         :name "setTopic"
         :description "Sets the current topic of conversation.  Use this to change the topic of conversation with the LLM."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :topic
                                                 (schema :type :string
                                                         :description "The new topic of conversation."))
                             :required (vector :topic))
         :response (schema :type :string))
        (lambda (&key topic)
          (setf (current-topic) topic)
          topic))

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

       (gnutil "sort" "Use this command to sort lines of text.")

       (when *enable-lisp-introspection*
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
                  jsonx:+json-false+)))))

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
                  jsonx:+json-true+
                  jsonx:+json-false+)))))

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

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "temporaryDirectory"
           :description "Returns the pathname of the temporary directory.  This is the preferred way to find the temporary directory."
           :behavior :blocking
           :response (schema :type :string))
          (lambda ()
            (namestring (uiop:temporary-directory)))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "temporaryFilename"
           :description "Returns the name of a temporary file. This is the preferred way to obtain a temporary file name."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :type
                                                   (schema :type :string
                                                           :description "An optional file type for the temporary file name."))
                               :required (vector))
           :response (schema :type :string))
          (lambda (&key type)
            (namestring
             (make-pathname
              :name (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
                      (coerce
                             (loop repeat 16
                                   collect (aref chars (random (length chars))))
                             'string))
              :type type)))))

       (when *enable-evolution*
         (cons
          (function-declaration
           :name "updateSystemInstruction"
           :description "Modifies the system instruction used by the LLM at a particular index."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :index
                                                   (schema :type :integer
                                                           :description "The index of the instruction to modify in the system instruction.")
                                                   :instruction
                                                    (schema :type :string
                                                              :description "The replacement instruction to modify in the system instruction."))
                               :required (vector :index :instruction)))
          (lambda (&key index instruction)
            (update-evolvable-system-instruction index instruction))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "userHomeDirectory"
           :description "Returns the user's home directory pathname.  This is the preferred way to find the user's home directory."
           :behavior :blocking
           :response (schema :type :string))
          (lambda ()
            (namestring (user-homedir-pathname)))))

       (cons
        (function-declaration
         :name "viewImageFile"
         :description "Opens a file in the default image viewer application."
         :behavior :blocking
         :parameters (schema :type :object
                             :properties (object :pathname
                                                 (schema :type :string
                                                         :description "The pathname of the image to view."))
                             :required (vector :pathname))
         :response (schema :type :boolean
                           :description "Returns true after launching the image viewer."))

        (lambda (&key pathname)
          (let ((windows-file-name (str:trim (uiop:run-program (list "wslpath" "-aw" (handle-tilde pathname))
                                                               :output :string
                                                               :ignore-error-status t))))
            (uiop:launch-program (str:join #\Space (list "cmd.exe" "/C" "start" (format nil "\"~a\"" windows-file-name))))
            jsonx:+json-true+)))

       (when (and *enable-web-search*
                  (google:google-search-engine-id)
                  (google:search-engine-api-key))
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
                          (google:web-search
                           (str:join "+" (str:split " " search-terms :omit-nulls t)))))))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "windowsTemporaryDirectory"
           :description "Returns the pathname of the windows temporary directory.  This is the preferred way to find the windows temporary directory."
           :behavior :blocking
           :response (schema :type :string))
          (lambda ()
            "/mnt/c/Windows/Temp/")))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "writeFileBlob"
           :description "Write a blob to a file.  This is the preferred way to write the contents of a binary file."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :file
                                                   (schema :type :string
                                                           :description "The path to the file to write.")
                                                   :blob
                                                   (schema :type :object
                                                           :properties (object :data
                                                                    (schema :type :string
                                                                            :description "The base64-encoded data of the blob.")
                                                                    :mime-type
                                                                    (schema :type :string
                                                                            :description "The MIME type of the blob."))
                                                           :required (vector :data :mime-type)
                                                           :description "The blob to write to the file."))
                               :required (vector :file :blob)))
          (lambda (&key file blob)
            (let ((file* (handle-tilde file)))
              (handler-case
                  (progn
                    (ensure-directories-exist file*)
                    (format *trace-output* "~&Directories exist: ~a~%" file*)
                    (finish-output *trace-output*)
                    (backup-file file*)
                    (format *trace-output* "~&Writing ~d to file: ~a~%" blob file*)
                    (finish-output *trace-output*)
                    (blob->file file* (get-data blob)))

                (error (e)
                  (format *trace-output* "~&Error writing file ~a: ~a~%" file* e)))))))

       (when *enable-file-system*
         (cons
          (function-declaration
           :name "writeFileLines"
           :description "Write a vector of strings to a file.  This is the preferred way to write the contents of a file."
           :behavior :blocking
           :parameters (schema :type :object
                               :properties (object :file
                                                   (schema :type :string
                                                           :description "The path to the file to read.")
                                                   :lines
                                                   (schema :type :array
                                                           :items (schema :type :string)
                                                           :description "The lines to write to the file."))
                               :required (vector :file :lines)))
          (lambda (&key file lines)
            (let ((file* (handle-tilde file)))
              (handler-case
                  (progn
                    (ensure-directories-exist file*)
                    (format *trace-output* "~&Directories exist: ~a~%" file*)
                    (finish-output *trace-output*)
                    (backup-file file*)
                    (format *trace-output* "~&Writing ~d lines to file: ~a~%" (length lines) file*)
                    (finish-output *trace-output*)
                    (with-open-file (stream file* :direction :output :if-does-not-exist :create :if-exists :supersede :element-type 'character :external-format :utf-8)
                      (dolist (line (coerce lines 'list) (finish-output stream))
                        (write-line line stream))))
                (error (e)
                  (format *trace-output* "~&Error writing file ~a: ~a~%" file* e)))))))

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
                jsonx:+json-true+
                jsonx:+json-false+))))

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
                jsonx:+json-true+
                jsonx:+json-false+))))
       )))))

(defun mcp-functions-and-handlers ()
  "Extracts the list of functions supplied by the MCP servers."
  (fold-left (binary-compose-right #'append #'get-mcp-functions-and-handlers) nil
             (remove (find-mcp-server "memory") *mcp-servers*)))

(defun transform-description (input-string)
  "Transforms the description string by looking for _ characters, uppercasing the next character, and removing the _."
  (with-output-to-string (output-string)
    (with-input-from-string (str input-string)
      (loop for char = (read-char str nil)
            while char
            do (if (char= char #\_)
                   (let ((next-char (read-char str nil)))
                     (when next-char
                       (write-char (char-upcase next-char) output-string)))
                   (write-char char output-string))))))

(defun convert-property (prop)
  (cons (if (keywordp (car prop))
            (car prop)
            (keystring->keyword (car prop)))
        (encode-schema-type (cdr prop))))

(defun convert-input-schema (input-schema)
  (object :type (get-type input-schema)
          :properties (alist-hash-table (map 'list #'convert-property (hash-table-alist (get-properties input-schema))))
          :required (or (get-required input-schema)
                        #())))
                              
(defun convert-tool (mcp-server tool)
  "Converts an MCP tool to a function specification."
  (let ((input-schema (convert-input-schema (get-input-schema tool)))
        (output-schema (get-output-schema tool)))

    (let ((fd (function-declaration
               :name (format nil "~a" (get-name tool))
               :description (transform-description (get-description tool))
               :behavior :blocking
               :parameters (encode-schema-type input-schema)
               :response (encode-schema-type output-schema))))
      ;; (format *trace-output* "~&;; Converted MCP tool: ~a~%" (dehashify fd))
      ;; (finish-output *trace-output*)
      (cons fd
            (lambda (&rest args &key &allow-other-keys)
              ;; (format *trace-output* "~&;; Calling MCP tool: ~a with args: ~a~%" (get-name tool) args)
              ;; (format *trace-output* "~&;; required: ~a~%" (get-required input-schema))
              ;; (finish-output *trace-output*)
              (call-tool mcp-server tool (plist-hash-table args)))))))

(defun get-mcp-functions-and-handlers (mcp-server)
  "Returns the functions and handlers for the given MCP server."
  (when (and (mcp-server-alive? mcp-server)
             (tools-capability mcp-server))
    (map 'list (lambda (tool) (convert-tool mcp-server tool)) (get-tools mcp-server))))
