;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun filesystem-tools-and-handlers ()
  "Return a list of filesystem-related functions and their handlers."
  (remove
   nil
   (list

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
       (ensure-directories-exist (parse-namestring (handle-tilde directory)))))

    (cons
     (function-declaration
      :name "currentDirectory"
      :description "Returns the current directory pathname.  This is the preferred way to get the current directory, as it will return the directory in a consistent format across different operating systems."
      :behavior :blocking
      :response (schema :type :string))
     (lambda ()
       (namestring *default-pathname-defaults*)))

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
                              (ensure-directory-pathname (handle-tilde directory)))))))

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
       (probe-file (parse-namestring (handle-tilde directory)))))

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
        :mime-type mime-type)))

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
         (scan-file (handle-tilde pathname) #'read-line))))

    (cons
     (function-declaration
      :name "temporaryDirectory"
      :description "Returns the pathname of the temporary directory.  This is the preferred way to find the temporary directory."
      :behavior :blocking
      :response (schema :type :string))
     (lambda ()
       (namestring (uiop:temporary-directory))))

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
         :type type))))

    (cons
     (function-declaration
      :name "userHomeDirectory"
      :description "Returns the user's home directory pathname.  This is the preferred way to find the user's home directory."
      :behavior :blocking
      :response (schema :type :string))
     (lambda ()
       (namestring (user-homedir-pathname))))

    (cons
     (function-declaration
      :name "windowsTemporaryDirectory"
      :description "Returns the pathname of the windows temporary directory.  This is the preferred way to find the windows temporary directory."
      :behavior :blocking
      :response (schema :type :string))
     (lambda ()
       "/mnt/c/Windows/Temp/"))

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
             (format *trace-output* "~&Error writing file ~a: ~a~%" file* e))))))

    (cons
     (function-declaration
      :name "writeFileLines"
      :description "Write a vector of strings to a file.  This is the preferred way to write the contents of a file."
      :behavior :blocking
      :parameters (schema :type :object
                          :properties (object :file
                                              (schema :type :string
                                                      :description "The path to the file to write.")
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
             (format *trace-output* "~&Error writing file ~a: ~a~%" file* e))))))
    )))
