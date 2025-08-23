;;; -*- Lisp -*-

(in-package "GEMINI")

(defun ->boolean (thing)
  (if (null thing)
      nil
      t))

(defun deflow (string)
  "Removes newlines from STRING and replaces them with spaces, ensuring that the result is a single line."
  (let ((lines
            (remove-if (lambda (line) (zerop (length line)))
                       (map 'list #'str:trim (str:split #\newline string)))))
    (and lines 
         (str:join #\Space lines))))

(defun reflow-comment (lines)
  "Reflow a list of lines into a single string."
  (let iter ((lines lines)
             (words nil)
             (new-line ";;")
             (answer ""))
    (cond ((> (length new-line) 80)
           (iter lines words ";;" (concatenate 'string answer new-line "
")))
          ((null words)
           (cond ((null lines) (concatenate 'string answer new-line))
                 ((zerop (length (car lines)))
                  (iter (cdr lines) nil ";;" (concatenate 'string answer new-line "
")))
                 (t
                  (iter (cdr lines) (str:split #\Space (car lines)) ";;" (concatenate 'string answer new-line "
")))))
          (t (iter lines (cdr words) (if (zerop (length new-line))
                                         (car words)
                                         (concatenate 'string new-line " " (car words)))
               answer)))))

(defun dehashify (object)
  (cond ((hash-table-p object)
         (mapcar #'dehashify (hash-table-alist object)))
        ((consp object)
         (let ((dehashed-car (dehashify (car object)))
               (dehashed-cdr (dehashify (cdr object))))
           (if (and (eq dehashed-car (car object))
                    (eq dehashed-cdr (cdr object)))
               object
               (cons dehashed-car dehashed-cdr))))
        ((stringp object) object)
        ((vectorp object)
         (map 'vector #'dehashify object))
        (t object)))

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
  (let ((object (make-hash-table :test 'equal)))
    (let iter ((fields fields))
      (when fields
        (let ((field-name (->keyword (first fields)))
              (field-value (second fields)))
          (setf (gethash field-name object) field-value))
        (iter (cddr fields))))
    object))

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
   satisfy the provided PREDICATE."
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
  "Define a GET-<name> and (SETF GET-<name>) generic function."
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
        symbol
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

(defun web-search (query)
  (let ((response (dex:get (format nil "https://www.googleapis.com/customsearch/v1?cx=~a&q=~a"
                                   (custom-search-engine-id)
                                   query)
                           :headers `(("x-goog-api-key". ,(custom-search-engine-api-key))))))
    (if (stringp response)
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string response))
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string
           (flex:octets-to-string response :external-format :utf-8))))))
