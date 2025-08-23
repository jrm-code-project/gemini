;;; -*- Lisp -*-

(in-package "GEMINI")

(defclass json-boolean ()
  ((value :initarg :value)))

(defmethod json:encode-json ((object json-boolean) &optional stream)
  (princ (slot-value object 'value) stream)
  nil)

(defmethod print-object ((object json-boolean) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (slot-value object 'value))))

(defparameter +json-false+ (make-instance 'json-boolean :value "false"))
(defparameter +json-null+  (make-instance 'json-boolean :value "null"))
(defparameter +json-true+  (make-instance 'json-boolean :value "true"))
(defparameter +json-empty-list+ #())
(defparameter +json-empty-object+ (make-hash-table))

(defun set-decoder-jrm-semantics ()
  "Set the JSON decoder semantics to the following:
  * Strings and Numbers are decoded naturally, reals becoming floats.
  * The literal name true is decoded to +json-true+, false to +json-false+ and null to +json-null+.
  * Arrays are decoded to sequences of the type *JSON-ARRAY-TYPE*.
  * Objects are decoded to hash-tables.  Object keys are converted by the
function *JSON-IDENTIFIER-NAME-TO-LISP* and then interned in the
package *JSON-SYMBOLS-PACKAGE*."
  (setq cl-json::+json-lisp-symbol-tokens+
        `(("false" . ,+json-false+)
          ("null"  . ,+json-null+)
          ("true"  . ,+json-true+)))
  (cl-json:set-custom-vars
   :integer #'cl-json::parse-number
   :real    #'cl-json::parse-number
   :boolean #'cl-json::json-boolean-to-lisp
   :beginning-of-array #'cl-json::init-accumulator
   :array-member #'cl-json::accumulator-add
   :end-of-array #'cl-json::accumulator-get-sequence
   :array-type 'vector
   :beginning-of-object #'cl-json::init-accumulator
   :object-key #'cl-json::accumulator-add-key
   :object-value #'cl-json::accumulator-add-value
   :end-of-object (compose #'alist-hash-table #'cl-json::accumulator-get)
   :beginning-of-string #'cl-json::init-string-stream-accumulator
   :string-char #'cl-json::string-stream-accumulator-add
   :end-of-string #'cl-json::string-stream-accumulator-get
   :aggregate-scope (union cl-json:*aggregate-scope-variables*
                           '(cl-json::*accumulator* cl-json::*accumulator-last*))
   :internal-decoder #'cl-json:decode-json))

(defmacro with-decoder-jrm-semantics (&body body)
  "Execute BODY in a dynamic environment where the decoder semantics
is such as set by SET-DECODER-JRM-SEMANTICS."
  `(CL-JSON:WITH-SHADOWED-CUSTOM-VARS
     (SET-DECODER-JRM-SEMANTICS)
     ,@body))

(eval-when (:load-toplevel :execute)
  (with-decoder-jrm-semantics
    (let* ((json-string1 "{\"a\":1,\"c\":true,\"d\":false,\"e\":null,\"f\":[],\"g\":{\"foo\":42,\"bar\":[1,2,3]}}")
           (json-string2
             (cl-json:encode-json-to-string
              (cl-json:decode-json-from-string json-string1))))
    (assert (equal json-string1
                   json-string2))
      ())))

(defun json-alist-entry? (s)
  (and (consp s)
       (or (symbolp (car s))
           (stringp (car s)))))

(defun is-json-alist? (s)
  (every #'json-alist-entry? s))

(defun encode-json-list-try-alist (s stream)
  (if (is-json-alist? s)
      (cl-json::encode-json-alist s stream)
      (cl-json::encode-json-list-guessing-encoder s stream)))

(eval-when (:load-toplevel :execute)
  (setq cl-json::*json-list-encoder-fn* 'encode-json-list-try-alist))
