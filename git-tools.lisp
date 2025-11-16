;;; -*- Lisp -*-

(in-package "GEMINI")

(defun git-tools-and-handlers ()
  "Return a list of git-related functions and their handlers."
  (list
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
       :ignore-error-status t)))))
