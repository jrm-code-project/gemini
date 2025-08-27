;;; -*- Lisp -*-

;;; Fetch the API keys.

;;; We need an API key for Gemini and optionally a Custom Search
;;; Engine API key if we want to have access to web search
;;; capabilities.

;;; The API keys can be scoped to a specific Google Cloud project.

;;; The API keys can be stored in files in the user's XDG config
;;; directory tree, or set in environment variables.

;;; It costs nothing to set up a Google Cloud account and create a
;;; project, but you will be charged for usage of the Gemini API and
;;; custom search engine if you exceed the free tier.  The costs as of
;;; 2025 appear to be modest (a few dollars per month) for a hobbyist.
;;; When you sign up, you get a $300 credit for the first 90 days, so
;;; you can judge for yourself if the costs are acceptable.

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

(defun default-custom-search-engine-apikey-pathname ()
  "Returns the default pathname for the Google Custom Search Engine API key file
   within the Google APIs configuration directory."
  (merge-pathnames "cse-apikey" (googleapis-pathname)))

(defun default-custom-search-engine-id-pathname ()
  "Returns the default pathname for the Google Custom Search Engine ID file
   within the Google APIs configuration directory."
  (merge-pathnames "cse-id" (googleapis-pathname)))

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

(defun project-cse-pathname (project)
  "Constructs the pathname for the Custom Search Engine configuration
   directory specific to a given Google Cloud PROJECT within the
   Google APIs configuration directory."
  (merge-pathnames (make-pathname :directory (list :relative project "CustomSearchEngine"))
                   (googleapis-pathname)))

(defun project-custom-search-engine-apikey-pathname (project)
  "Constructs the pathname for the Custom Search Engine API key file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-cse-pathname project) "apikey"))

(defun project-custom-search-engine-id-pathname (project)
  "Constructs the pathname for the Custom Search Engine ID file
   specific to a given Google Cloud PROJECT within the Google APIs
   configuration directory."
  (merge-pathnames (project-cse-pathname project) "id"))

(defun apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-apikey-pathname project))))
      (probe-file (default-apikey-pathname))))

(defun custom-search-engine-apikey-pathname ()
  "Determines the effective pathname for the Google API key.
   It first checks for a project-specific API key (if a default project
   is set), then falls back to the default API key pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-custom-search-engine-apikey-pathname project))))
      (probe-file (default-custom-search-engine-apikey-pathname))))

(defun custom-search-engine-id-pathname ()
  "Determines the effective pathname for the Google Custom Search Engine ID.
   It first checks for a project-specific ID (if a default project
   is set), then falls back to the default ID pathname.
   Returns the pathname if found, otherwise NIL."
  (or (let ((project (default-project)))
        (when project
          (probe-file (project-custom-search-engine-id-pathname project))))
      (probe-file (default-custom-search-engine-id-pathname))))

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

(defun custom-search-engine-api-key ()
  "Retrieves the Google Custom Search Engine API key. It first attempts to read it from
   the CSE API key file (either project-specific or default), then falls back
   to the GOOGLE_CSE_API_KEY environment variable.
   Signals an error if no CSE API key is found."
  (or (let ((pathname (custom-search-engine-apikey-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_CSE_API_KEY")
      (error "No Google Custom Search Engine API key found.  Set the environment variable GOOGLE_CSE_API_KEY or create file at ~a." (namestring (custom-search-engine-apikey-pathname)))))

(defun custom-search-engine-id ()
  "Retrieves the Google Custom Search Engine ID. It first attempts to read it from
   the CSE ID file (either project-specific or default), then falls back
   to the GOOGLE_CSE_ID environment variable.
   Signals an error if no CSE ID is found."
  (or (let ((pathname (custom-search-engine-id-pathname)))
        (and pathname
             (with-open-file (stream pathname :direction :input)
               (let ((line (read-line stream nil)))
                 (when line
                   (str:trim line))))))
      (uiop:getenv "GOOGLE_CSE_ID")
      (error "No Google Custom Search Engine ID found.  Set the environment variable GOOGLE_CSE_ID or cerate file at ~a." (namestring (custom-search-engine-id-pathname)))))
