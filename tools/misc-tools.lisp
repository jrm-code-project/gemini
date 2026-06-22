;;; -*- Lisp -*-

(in-package "GEMINI")

(defun misc-tools-and-handlers ()
  "Return a list of miscellaneous functions and their handlers."
  (remove
   nil
   (list
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
      :name "reminisce"
      :description "Recalls what has been memorized."
      :behavior :blocking
      :response (schema :type :string))
     (lambda ()
       (reminisce)))

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
         (uiop:launch-program (str:join #\Space (list "cmd.exe" "/C" "start" (format nil "~a" windows-file-name))))
         jsonx:+json-true+)))
    )))
