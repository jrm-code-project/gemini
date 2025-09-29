;;; -*- Lisp -*-

(in-package "GEMINI")

(defun seconds-per-minute () 60)
(defun minutes-per-hour () 60)
(defun hours-per-day () 24)
(defun minutes-per-day () (* (minutes-per-hour) (hours-per-day)))
(defun seconds-per-day ()
  (* (seconds-per-minute) (minutes-per-day)))
(defun absolute-day ()
  "Return the day since the start of the epoch (UTC)."
  (floor (get-universal-time) (seconds-per-day)))

;; Coerce to T or NIL
(defun ->boolean (thing)
  (if thing
      t
      nil))

(defun string->word-list (string)
  (str:split #\Space (str:trim string) :omit-nulls t))

(defun strings->word-list (strings)
  (mappend #'string->word-list strings))

(defun deflow (string)
  "Removes newlines from STRING and replaces them with spaces, ensuring that the result is a single line."
  (str:join #\Space 
            (remove-if (lambda (line) (zerop (length line)))
                       (map 'list #'str:trim (str:split #\newline string :omit-nulls t)))))

(defconstant +line-length-limit+ 80)

(defun reflow-line (line)
  "Reflow a single line to a list of lines, each no longer than 80 characters."
  (if (<= (length line) +line-length-limit+)
      (list line)
      (let ((space-pos (or (position #\Space line :from-end t :end (1+ +line-length-limit+))
                           (position #\Space line :start (1+ +line-length-limit+)))))
        (if (null space-pos)
            (list line)
            (cons (subseq line 0 space-pos)
                  (reflow-line (subseq line (min (1+ space-pos) (length line)))))))))

(defun key? (thing)
  "Checks if THING is a valid key (either a keyword or a string)."
  (or (keywordp thing)
      (stringp thing)))

(defun alist? (thing)
  "Checks if THING is an association list (alist).
   An alist is a list where each element is a cons cell, and the car
   of each cons cell is a key (keyword or string)."
  (or (and (consp thing)
           (every (lambda (entry)
                    (and (consp entry)
                         (key? (car entry))))
                  thing))
      (null thing)))

(defun keyword->keystring (keyword)
  "Converts a Lisp keyword to a camelCase string.
   If the keyword is not a valid keyword, it signals an error."
  (check-type keyword keyword)
  (cl-json:lisp-to-camel-case (symbol-name keyword)))

(defun keystring->keyword (keystring)
  "Converts a camelCase string to a Lisp keyword.
   If the string is not a valid camelCase string, it signals an error."
  (check-type keystring string)
  (intern (cl-json:simplified-camel-case-to-lisp keystring) (find-package "KEYWORD")))

(defun ->keyword (thing)
  (etypecase thing
    (keyword thing)
    (string (keystring->keyword thing))
    (symbol (->keyword (symbol-name thing)))))

(defun ->keystring (thing)
  (etypecase thing
    (keyword (keyword->keystring thing))
    (string thing)))

(defun object (&rest fields)
  "Creates an object with the specified FIELDS.
   Returns a hash table representing the object structure."
  (plist-hash-table fields))

(defun keys (thing)
  "Returns a list of keys from an alist or a hash table.
   Signals an error if THING is neither an alist nor a hash table."
  (cond ((alist? thing) (mapcar (compose #'->keyword #'car) thing))
        ((hash-table-p thing) (mapcar #'->keyword (hash-table-keys thing)))
        (t (error "Can't get keys of ~s" thing))))

(defun key-test (keyword)
  "Returns a predicate function that checks if its argument is equal to
   either KEYWORD (if it's a keyword) or STRING (if it's a string)."
  (lambda (thing)
    (eq (->keyword thing) (->keyword keyword))))

(defun required-key-test (required-key)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains the specified REQUIRED-KEY (provided as a list of keyword and string)."
  (let ((has-required-key? (key-test required-key)))
    (lambda (thing)
      (find-if has-required-key? (keys thing)))))

(defun required-keys-test (required-keys)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains ALL the keys specified in REQUIRED-KEYS."
  (let ((required-key-tests (mapcar #'required-key-test required-keys)))
    (lambda (thing)
      (every (lambda (test)
               (funcall test thing))
             required-key-tests))))

(defun valid-key-test (valid-keys)
  "Returns a predicate that checks if a key is present in either the
   KEYWORDS list (for keyword keys) or the STRINGS list (for string keys)."
  (lambda (thing)
    (member (->keyword thing) valid-keys)))

(defun valid-keys-test (valid-keys)
  "Returns a predicate that checks if all keys in a given object (alist or hash table)
   are present in either the KEYWORDS list or the STRINGS list."
  (let ((valid-key? (valid-key-test valid-keys)))
    (lambda (thing)
      (every valid-key? (keys thing)))))

(defun is-object-test (required-keys &optional additional-valid-keys)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains all the REQUIRED-KEYS and only the REQUIRED-KEYS and ADDITONAL-VALID-KEYS."
  (let ((required-keys-test (required-keys-test required-keys))
        (valid-keys-test (valid-keys-test (append required-keys additional-valid-keys))))
    (lambda (thing)
      (and (or (alist? thing)
               (hash-table-p thing))
           (funcall required-keys-test thing)
           (funcall valid-keys-test thing)))))

(defun list-of-test (predicate)
  "Returns a predicate that checks if THING is a list where all elements
   satisfy the provided PREDICATE.  An empty list is NOT considered valid."
  (lambda (thing)
    (and (consp thing)
         (every predicate thing))))

(defun singleton-list-of-test (predicate)
  "Returns a predicate that checks if THING is a list containing exactly
   one element, and that element satisfies the provided PREDICATE."
  (lambda (thing)
    (or (and (consp thing)
             (null (cdr thing))
             (funcall predicate (car thing)))
        (and (vectorp thing)
             (= (length thing) 1)
             (funcall predicate (svref thing 0))))))

(defmacro define-field (name getter)
  "Define a GET-<name> and (SETF GET-<name>) generic function.
Does not add values to ALISTS."
  `(PROGN
     (DEFGENERIC ,getter (OBJECT)
       (:DOCUMENTATION ,(format nil "Retrieves the '~a' field from an object." name))
       (:METHOD ((OBJECT HASH-TABLE))
         (OR (GETHASH ,(->keyword name) OBJECT)
             (GETHASH ,(->keystring name) OBJECT)))
       (:METHOD ((OBJECT LIST))
         (CDR (ASSOC ,(->keyword name) OBJECT :KEY #'->KEYWORD))))

     (DEFGENERIC (SETF ,getter) (NEW-VALUE OBJECT)
       (:DOCUMENTATION ,(format nil "Sets the '~a' field in an object." name))
       (:METHOD (NEW-VALUE (OBJECT HASH-TABLE))
         (SETF (GETHASH ,(->keyword name) OBJECT) NEW-VALUE))
       (:METHOD (NEW-VALUE (OBJECT LIST))
         (LET ((ENTRY (ASSOC ,(->keyword name) OBJECT :KEY #'->KEYWORD)))
           (IF ENTRY
               (SETF (CDR ENTRY) NEW-VALUE)
               (ERROR "Key ~s not found in alist ~s" ,(->keyword name) OBJECT)))))))

(defmacro define-standard-field (name)
  "Define a GET-<name> function and export it."
  (let ((getter-name (intern (format nil "GET-~A" (string-upcase (symbol-name name))) (find-package "GEMINI"))))
    `(PROGN
       (DEFINE-FIELD ,name ,getter-name)
       (EXPORT ',getter-name))))

(defmacro define-standard-fields (&rest names)
  "Bulk define fields."
  `(PROGN
     ,@(mapcar (lambda (name)
                 `(DEFINE-STANDARD-FIELD ,name))
               names)))

(defun object-ref-function (keyword)
  "Returns a function that retrieves the value associated with KEYWORD
   in an alist or hash table. If the key is not found, it returns NIL."
  (let ((symbol (find-symbol (format nil "GET-~A" (string-upcase (symbol-name keyword))) (find-package "GEMINI"))))
    (if (and symbol (fboundp symbol))
        (symbol-function symbol)
        (lambda (object)
          (if (hash-table-p object)
              (gethash keyword object)
              (cdr (assoc keyword object :key #'->keyword)))))))

(defun function-minimum-arity (func)
  (collect-length
   (until-if
    (lambda (symbol)
      (member symbol lambda-list-keywords))
    (scan 'list (sb-introspect:function-lambda-list func)))))

(defun function-return-type (func)
  "Returns the return type of a function as a string.
   If the function has no declared return type, it returns NIL."
  (let ((raw-function-type (sb-introspect:function-type func)))
    (and (consp raw-function-type)
         (eq (first raw-function-type) 'function)
         (let ((raw-return-type (third raw-function-type)))
           (if (consp raw-return-type)
               (if (eq (first raw-return-type) 'values)
                   (second raw-return-type)
                   raw-return-type)
               raw-return-type)))))

(defun returns-string? (func)
  (eq (function-return-type func) 'string))

(defun returns-boolean? (func)
  (eq (function-return-type func) 'boolean))

(defun prompting-read (prompt &optional (default nil))
  "Prompts the user for input, returning the input as a string.
   If DEFAULT is provided, it will be used as the default value."
  (format *query-io* "~&~a: " prompt)
  (let ((input (read-line *query-io* nil nil)))
    (if (string= input "")
        (or default "")
        input)))

(defun filter-external-symbols (package filter)
  "Return a list of external symbols in PACKAGE that match the FILTER function."
  (loop for sym being the external-symbols in package
        when (funcall filter sym)
          collect sym))

(defun filter-symbols (package filter)
  "Return a list of internal-symbols in PACKAGE that match the FILTER function."
  (loop for sym being the symbols in package
        when (funcall filter sym)
          collect sym))

(defun filter-visible-symbols (package filter)
  "Return a list of visible symbols in PACKAGE."
  (let* ((external (filter-external-symbols package filter))
         (internal (filter-symbols package
                                   (lambda (sym)
                                     (and (not (member (symbol-name sym) external :test #'equal :key #'symbol-name))
                                          (funcall filter sym)))))
         (inherited (mappend (lambda (package)
                               (filter-external-symbols
                                package
                                (lambda (sym)
                                  (and (not (member (symbol-name sym) external :test #'equal :key #'symbol-name))
                                       (not (member (symbol-name sym) internal :test #'equal :key #'symbol-name))
                                       (funcall filter sym)))))
                             (package-use-list package))))
    (remove-duplicates (sort (append external internal inherited) #'string< :key #'symbol-name))))

(defun visible-functions (package)
  "Return a list of visible functions defined in the given PACKAGE."
  (filter-visible-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (not (macro-function sym))
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun external-functions (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (not (macro-function sym))
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun visible-macros (package)
  "Return a list of macros visible in the given PACKAGE."
  (filter-visible-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (macro-function sym)
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun external-macros (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols
   package
   (lambda (sym)
     (and (fboundp sym)
          (macro-function sym)
          (not (str:starts-with? "%" (symbol-name sym)))))))

(defun visible-variables (package)
  "Return a list of variables visible in the given PACKAGE."
  (filter-visible-symbols package 
                          (lambda (sym)
                            (and (boundp sym)
                                 (not (str:starts-with? "%" (symbol-name sym)))))))

(defun external-variables (package)
  "Return a list of external functions defined in the given PACKAGE."
  (filter-external-symbols package 
                           (lambda (sym)
                             (and (boundp sym)
                                  (not (str:starts-with? "%" (symbol-name sym)))))))

(defun filter-package-list ()
  "Return a list of packages to consider for code generation."
  (sort (append
         (mappend (lambda (pn)
                    (let ((p (find-package pn)))
                      (when p (list p))))
                  '("SB-CLTL2"
                    "SB-MOP"))
         (remove-if
          (lambda (package)
            (let ((name (package-name package)))
              (or (equal name "KEYWORD")
                  (str:starts-with? "QL-" name)
                  (str:starts-with? "SB-" name)
                  (str:starts-with? "SLYNK" name)
                  (str:ends-with? "-ASD" name)
                  (str:ends-with? "-SYSTEM" name)
                  (str:ends-with? "-TEST" name)
                  (str:ends-with? "-TESTS" name)
                                        ;(find #\/ name)
                  (find #\. name)
                  )))
          (list-all-packages)))
        #'string<
        :key #'package-name))

(defun get-top-level-functions ()
  "Return a list of top-level functions defined anywhere."
  (sort (remove-duplicates (mappend #'external-functions (filter-package-list)))
        #'string< :key #'symbol-name))

(defun get-top-level-macros ()
  "Return a list of top-level macros defined anywhere."
  (sort (remove-duplicates (mappend #'external-macros (filter-package-list)))
        #'string< :key #'symbol-name))

(defun get-top-level-variables ()
  "Return a list of top-level functions defined anywhere."
  (sort (remove-duplicates (mappend #'external-variables (filter-package-list)))
        #'string< :key #'symbol-name))

(defun read-file-bytes (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when stream
      (let* ((size (file-length stream))
             (bytes (make-array size :element-type '(unsigned-byte 8))))
        (read-sequence bytes stream)
        bytes))))

(defun ensure-directory-pathname (pathname)
  "Ensures that the given PATHNAME ends with a directory separator.
   If PATHNAME is a string, it converts it to a pathname first."
  (let ((path (if (stringp pathname)
                  (truename (merge-pathnames pathname))
                  pathname)))
    (if (and (or (eq (pathname-name path) :unspecific)
                 (null (pathname-name path))
                 (and (stringp (pathname-name path))
                      (zerop (length (pathname-name path)))))
             (or (eq (pathname-type path) :unspecific)
                 (null (pathname-type path))
                 (and (stringp (pathname-type path))
                      (zerop (length (pathname-type path))))))
        path
        (make-pathname
         :directory
         (reverse
          (cons (concatenate 'string
                             (cond ((stringp (pathname-name path)) (pathname-name path))
                                   ((or (null (pathname-name path))
                                        (eq (pathname-name path) :unspecific)) "")
                                   (t (error "Unexpected pathname name: ~s" (pathname-name  path))))
                             (cond ((stringp (pathname-type path))
                                    (concatenate 'string "." (pathname-type path)))
                                   ((or (null (pathname-type path))
                                        (eq (pathname-type path) :unspecific)) "")
                                   (t (error "Unexpected pathname type: ~s" (pathname-type path)))))
                (reverse (pathname-directory path))))))))

(defun next-suffix (suffix)
  "Given a suffix like '~' or '~1', returns the next suffix in sequence.
   For example, '~' becomes '~1', '~1' becomes '~2', and so on."
  (if (string= suffix "~")
      "~1"
      (let* ((number-part (subseq suffix 1))
             (number (parse-integer number-part :junk-allowed t)))
        (if number
            (format nil "~~~a" (1+ number))
            (error "Invalid suffix format: ~s" suffix)))))

(defun backup-pathname (pathname &optional (suffix "~"))
  (let ((backup (pathname (concatenate 'string (namestring pathname) suffix))))
    (if (probe-file backup)
        (backup-pathname pathname (next-suffix suffix))
        backup)))

(defun backup-file (pathname)
  (when (probe-file pathname)
    (rename-file pathname (backup-pathname pathname))))

(defun ends-of-lines (filename)
  (with-open-file (stream filename :direction :input)
    (do ((indexes (list 0) (cons (file-position stream) indexes)))
        ((eql (read-line stream nil :eof) :eof)
         (values (reverse indexes) (file-position stream))))))

(defun ends-of-forms (filename)
  (with-open-file (stream filename :direction :input)
    (do ((indexes (list 0) (cons (file-position stream) indexes)))
        ((eql (read stream nil :eof) :eof) (reverse indexes)))))

(defun file-forms (filename)
  (multiple-value-bind (ends-of-lines end-of-file) (ends-of-lines filename)
    (let ((ends-of-forms (ends-of-forms filename))
          (text (uiop:read-file-string filename)))
      (let ((ends-of-forms* (map 'list (lambda (end-of-form)
                                         (find end-of-form ends-of-lines :test #'<=))
                                 ends-of-forms)))
        (do ((indexes ends-of-forms* (cdr indexes))
             (forms '() (cons (str:trim (subseq text (car indexes) (cadr indexes))) forms)))
            ((null (cdr indexes))
             (if (< (car indexes) end-of-file)
                 (reverse (cons (str:trim (subseq text (car indexes) end-of-file)) forms))
                 (reverse forms))))))))

(defun guess-mime-type (path)
  "Simple helper to guess MIME type based on file extension."
  (let ((extension (pathname-type path)))
    (cond 
      ((string-equal extension "csv")  "text/csv")
      ((string-equal extension "gif")  "image/gif")
      ((string-equal extension "html") "text/html")
      ((string-equal extension "jpeg") "image/jpeg")
      ((string-equal extension "jpg")  "image/jpeg")
      ((string-equal extension "json") "application/json")
      ((string-equal extension "lisp") "text/plain") ; Common Lisp files are plain text
      ((string-equal extension "pdf")  "application/pdf")
      ((string-equal extension "png")  "image/png")
      ((string-equal extension "txt")  "text/plain")
      ((string-equal extension "webp") "image/webp")
      ((string-equal extension "xml")  "application/xml")
      (t "application/octet-stream"))))

(defun file->blob (path)
  (handler-case
      (let ((bytes (read-file-bytes path)))
        (and bytes (cl-base64:usb8-array-to-base64-string bytes)))
    (file-error (e)
      (format *error-output* "Error reading file ~a: ~a~%" path e)
      nil)
    (error (e)
      (format *error-output* "Error encoding file ~a: ~a~%" path e)
      nil)))
