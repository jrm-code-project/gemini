;;; -*- Lisp -*-

(in-package "GEMINI")

(defun googleapis-pathname ()
  "Returns the base pathname for Google APIs configuration files,
   typically located in the user's XDG config directory."
  (uiop/configuration:xdg-config-pathname "googleapis/"))

(defun default-apikey-pathname ()
  "Returns the default pathname for the API key file within the
   Google APIs configuration directory."
  (merge-pathnames "apikey" (googleapis-pathname)))

(defun default-project-pathname ()
  "Returns the default pathname for the default project file within the
   Google APIs configuration directory."
  (merge-pathnames "default-project" (googleapis-pathname)))

(defun default-project ()
  "Reads and returns the default Google Cloud project name from its
   designated configuration file. Returns NIL if the file does not exist
   or is empty."
  (let ((pathname (default-project-pathname)))
    (when (probe-file pathname)
      (with-open-file (stream pathname :direction :input)
        (let ((line (read-line stream nil)))
          (when line
            (str:trim line)))))))

(defun project-apikey-pathname (project)
  "Constructs the pathname for the API key file specific to a given
   Google Cloud PROJECT within the Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project)
                                  :name "apikey")
                   (googleapis-pathname)))

(defun apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-apikey-pathname project))))
      (probe-file (default-apikey-pathname))))

(defun google-api-key ()
  "Retrieves the Google API key. It first attempts to read it from
   the API key file (either project-specific or default), then falls back
   to the GOOGLE_API_KEY environment variable.
   Signals an error if no API key is found."
  (or (let ((pathname (apikey-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_API_KEY")
      (error "No Google API key found. Set the environment variable GOOGLE_API_KEY or create a file at ~a."
             (namestring (apikey-pathname)))))
