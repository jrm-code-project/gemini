;;; -*- Lisp -*-

(in-package "GEMINI")

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
  (intern (cl-json:camel-case-to-lisp keystring) (find-package "KEYWORD")))

(defun ->keyword (thing)
  (etypecase thing
    (keyword thing)
    (string (keystring->keyword thing))))

(defun ->keystring (thing)
  (etypecase thing
    (keyword (keyword->keystring thing))
    (string thing)))

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

(defun is-object-test (required-keys valid-keys)
  "Returns a predicate that checks if a given object (alist or hash table)
   contains all the REQUIRED-KEYS and only the VALID-KEYS."
  (let ((required-keys-test (required-keys-test required-keys))
        (valid-keys-test (valid-keys-test valid-keys)))
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
    (and (consp thing)
         (null (cdr thing))
         (funcall predicate (car thing)))))

(defun object-ref-function (keyword)
  "Returns a function that retrieves the value associated with KEYWORD
   in an alist or hash table. If the key is not found, it returns NIL."
  (let ((key (->keyword keyword)))
    (lambda (thing)
      (cond ((alist? thing) (cdr (assoc key thing :key #'->keyword)))
            ((hash-table-p thing) (or (gethash key thing)
                                      (gethash (->keystring key) thing)))
            (t (error "Can't get value for ~s from ~s" key thing))))))







