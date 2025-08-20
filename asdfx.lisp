;;; -*- Lisp -*-

(in-package "GEMINI")

;;; Extension to ASDF to list available systems.
(defun get-asdf-default-dirs ()
  "Returns a list of common default directories ASDF scans recursively."
  (let ((dirs '()))
    (labels ((add-if-exists (path)
               (let ((abs-path (uiop:ensure-directory-pathname path)))
                 (when (uiop:directory-exists-p abs-path)
                   (push abs-path dirs)))))
      (add-if-exists (uiop:xdg-data-home "common-lisp/")) ; XDG standard
      (add-if-exists (uiop:subpathname (user-homedir-pathname) "common-lisp/")) ; Older/common home dir
      ;; Add other known default locations if necessary, e.g., implementation-specific ones.
      dirs)))

(defun get-source-registry-conf-files ()
  "Returns a list of all ASDF source registry configuration files."
  (let ((config-dirs '()))
    ;; Standard XDG config dir
    (push (uiop:xdg-config-home "common-lisp/source-registry.conf") config-dirs)
    (push (uiop:xdg-config-home "common-lisp/source-registry.conf.d/") config-dirs)

    ;; Fallback for older/non-XDG systems (less common but possible)
    (push (uiop:subpathname (user-homedir-pathname) ".config/common-lisp/source-registry.conf") config-dirs)
    (push (uiop:subpathname (user-homedir-pathname) ".config/common-lisp/source-registry.conf.d/") config-dirs)

    (let ((files '()))
      (dolist (dir-or-file (delete-duplicates config-dirs :test #'equal))
        (cond ((uiop:file-exists-p dir-or-file)
               (pushnew (truename dir-or-file) files :test #'equal))
              ((uiop:directory-exists-p dir-or-file)
               (dolist (file (uiop:directory-files dir-or-file "*.conf"))
                 (pushnew (truename file) files :test #'equal)))))

      (nreverse files))))

(defun parse-source-registry-config (file-path)
  "Reads and parses a single ASDF source-registry.conf file.
   Returns the parsed S-expression."
  (handler-case
      (uiop:read-file-forms file-path)
    (error (e)
      (format *error-output* "~&Warning: Could not read ASDF config file ~A: ~A~%" file-path e)
      nil)))

(defun collect-all-source-registry-forms ()
  "Collects and parses all source registry configuration forms."
  (let ((all-forms '()))
    ;; Process individual files in .conf.d first (alphabetical order, usually)
    (loop for file in (sort (get-source-registry-conf-files) #'string< :key #'namestring)
          do (setf all-forms (append all-forms (parse-source-registry-config file))))

    ;; Handle CL_SOURCE_REGISTRY environment variable
    (let ((env-var (uiop:getenv "CL_SOURCE_REGISTRY")))
      (when env-var
        ;; The env var can contain multiple paths separated by ":" on Unix or ";" on Windows
        ;; or a single s-expression starting with (:source-registry ...)
        (if (char= (char env-var 0) #\()
            ;; Assume it's an s-expression
            (handler-case (setf all-forms (append all-forms (read-from-string env-var)))
              (error (e)
                (format *error-output* "~&Warning: Could not parse CL_SOURCE_REGISTRY environment variable: ~A~%" e)))
            ;; Assume it's a path list
            (dolist (path (uiop:split-string env-var :separator (string (uiop:directory-separator-for-host))))
              (unless (string= path "")
                (push (list :tree (uiop:parse-unix-namestring path)) all-forms))))))

    (nreverse all-forms)))

(defun interpret-source-registry-forms (forms)
  "Interprets ASDF source registry forms and returns two lists:
   (list-of-directories-to-scan recursive-directories-to-scan exclusion-paths)."
  (let ((direct-dirs '())
        (tree-dirs '())
        (exclusion-paths '())
        (inherit-p nil))
    (dolist (form forms)
      (cond ((and (listp form) (keywordp (car form)))
             (case (car form)
               (:tree
                (dolist (path-designator (cdr form))
                  (push (uiop:ensure-directory-pathname path-designator) tree-dirs)))
               (:directory
                (dolist (path-designator (cdr form))
                  (push (uiop:ensure-directory-pathname path-designator) direct-dirs)))
               (:exclude
                (dolist (path-designator (cdr form))
                  (push (uiop:ensure-pathname path-designator :wild-inferiors nil :ensure-directory nil)
                        exclusion-paths)))
               (:inherit-configuration
                (setf inherit-p t))
               ;; ASDF has other directives like :module-relative-path, etc.
               ;; For simplicity, we'll focus on the most common ones.
               (t
                (format *error-output* "~&Warning: Unknown ASDF source registry directive: ~A~%" (car form)))))
            (t
             (format *error-output* "~&Warning: Malformed ASDF source registry form: ~A~%" form))))

    ;; If :inherit-configuration is not explicitly present, ASDF still inherits by default
    ;; unless :ignore-inherited-configuration is used. For this "lister", assume inherit unless explicitly ignored.
    (when (or inherit-p (not (member :ignore-inherited-configuration (car forms))))
      (dolist (dir (get-asdf-default-dirs))
        (push dir direct-dirs)))

    (values (nreverse direct-dirs)
            (nreverse tree-dirs)
            (nreverse exclusion-paths))))

(defun is-excluded-p (pathname exclusion-paths)
  "Checks if a pathname matches any of the exclusion patterns."
  (find pathname exclusion-paths :test #'uiop::pathname-match-p))

(defun collect-asd-files (root-dir &key recursive p-exclude)
  "Collects .asd files from a directory, optionally recursively, with exclusions."
  (let ((found-files '()))
    (labels ((scan (current-dir)
               (unless (is-excluded-p current-dir p-exclude)
                 (dolist (file (uiop:directory-files current-dir "*.asd"))
                   (unless (is-excluded-p file p-exclude)
                     (push file found-files)))
                 (when recursive
                   (mapc #'scan (uiop:subdirectories current-dir))))))
      (scan root-dir))
    (nreverse found-files)))

(defun list-all-local-asdf-systems ()
  "Lists all ASDF systems found on the local filesystem by parsing
   the source registry configuration."
  (multiple-value-bind (direct-dirs tree-dirs exclusion-paths)
      (interpret-source-registry-forms (collect-all-source-registry-forms))
    (let ((system-names (make-hash-table :test #'equal))) ; Use hash table for efficient deduplication

      ;; Collect from direct directories
      (dolist (dir direct-dirs)
        (dolist (file (collect-asd-files dir :recursive nil :p-exclude exclusion-paths))
          (setf (gethash (pathname-name file) system-names) t)))

      ;; Collect from recursive directories
      (dolist (dir tree-dirs)
        (dolist (file (collect-asd-files dir :recursive t :p-exclude exclusion-paths))
          (setf (gethash (pathname-name file) system-names) t)))

      ;; Convert hash table keys to a sorted list
      (sort (hash-table-keys system-names) #'string<))))
