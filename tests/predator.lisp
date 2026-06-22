;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite predator-tests)

(defmacro with-binary-stream-from-string ((stream-var string) &body body)
  (let ((temp-file (gensym "TEMP-FILE")))
    `(uiop:with-temporary-file (:pathname ,temp-file :element-type '(unsigned-byte 8) :direction :output)
       (let ((bytes (map '(vector (unsigned-byte 8)) #'char-code ,string)))
         (with-open-file (out ,temp-file :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
           (write-sequence bytes out)))
       (with-open-file (,stream-var ,temp-file :direction :input :element-type '(unsigned-byte 8))
         ,@body))))

(test predator-normal-parsing
  "Test normal, valid S-expression parsing."
  (with-binary-stream-from-string (stream "123")
    (is (eql 123 (predator-read stream))))
  (with-binary-stream-from-string (stream "+456")
    (is (eql 456 (predator-read stream))))
  (with-binary-stream-from-string (stream "-789")
    (is (eql -789 (predator-read stream))))
  (with-binary-stream-from-string (stream "0")
    (is (eql 0 (predator-read stream))))
  (with-binary-stream-from-string (stream "LET")
    (is (eq 'gemini::let (predator-read stream))))
  (with-binary-stream-from-string (stream " (IF T NIL) ")
    (is (equal '(gemini::if t nil) (predator-read stream))))
  (with-binary-stream-from-string (stream "(LET (T) NIL)")
    (is (equal '(gemini::let (t) nil) (predator-read stream)))))

(test predator-safe-whitelist-symbols
  "Test that the newly added safe whitelist symbols parse correctly."
  (dolist (sym-str '("LET*" "CONS" "CAR" "CDR" "LIST" "APPEND" "EQUAL" "PROGN" "COND" "SETQ" "SETF" "FORMAT" "PRINT"
                     "PI" "WHEN" "UNLESS" "DESTRUCTURING-BIND" "DEFMACRO" "MAPCAR" "REDUCE" "SIN" "COS" "MAKE-ARRAY"
                     "GETHASH" "UNWIND-PROTECT" "DEFCLASS" "MAKE-INSTANCE" "SLOT-VALUE" "GENSYM"))
    (with-binary-stream-from-string (stream sym-str)
      (is (eq (find-symbol sym-str "GEMINI") (predator-read stream))))))

(test enable-eval-boundp
  "Test that *enable-eval* and other enable flags are bound and behave as special variables."
  (is (boundp 'gemini::*enable-eval*))
  (is (null gemini::*enable-eval*))
  (let ((gemini::*enable-eval* t))
    (is (eq t gemini::*enable-eval*)))
  (is (boundp 'gemini::*enable-bash*))
  (is (boundp 'gemini::*enable-interaction*))
  (is (boundp 'gemini::*enable-lisp-introspection*))
  (is (boundp 'gemini::*enable-web-functions*))
  (is (boundp 'gemini::*enable-web-search*)))

(test predator-whitespace
  "Test whitespace handling."
  (with-binary-stream-from-string (stream "   
  123   
")
    (is (eql 123 (predator-read stream)))))

(test predator-deadline-exceeded
  "Test that absolute deadline triggers a deadline-exceeded error."
  (with-binary-stream-from-string (stream "123")
    (let ((arena (gemini::checkout-arena-securely))
          (buf (gemini::checkout-buffer-securely)))
      (unwind-protect
           (signals predator-terminal-condition
             (handler-case
                 (gemini::%predator-read-internal stream :timeout-ms -100 :arena arena :buffer buf)
               (predator-terminal-condition (c)
                 (is (eq :deadline-exceeded (threat-reason c)))
                 (error c))))
        (gemini::return-arena-securely arena)
        (gemini::return-buffer-securely buf)))))

(test predator-arena-exhausted
  "Test that AST Arena boundary triggers an arena-exhausted error."
  (with-binary-stream-from-string (stream "(1 2 3)")
    (let ((arena (make-array 2 :initial-element nil))
          (buf (gemini::checkout-buffer-securely)))
      (unwind-protect
           (signals predator-terminal-condition
             (handler-case
                 (gemini::%predator-read-internal stream :arena arena :buffer buf)
               (predator-terminal-condition (c)
                 (is (eq :arena-exhausted (threat-reason c)))
                 (error c))))
        (gemini::return-buffer-securely buf)))))

(test predator-trie-divergence
  "Test that unknown symbols or trie divergence trigger errors."
  (dolist (input '("LE" "INVALID" "(IF T INVALID)" "(LET (T) LE)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :diverged-from-trie (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-numeric-overflow
  "Test that numeric overflow triggers errors and prevents Bignum promotion."
  (dolist (input (list (format nil "~A" (1+ most-positive-fixnum))
                       "999999999999999999999999999999999999999999999999999"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :numeric-overflow (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-type-annihilation
  "Test that ratios and exponent markers are rejected with type-annihilation."
  (dolist (input '("123e4" "123E4" "123d4" "123D4" "1/2" "(IF T 1/2)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :type-annihilation (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-leading-zeroes
  "Test that leading zeroes are rejected."
  (dolist (input '("0123" "(IF T 05)"))
    (with-binary-stream-from-string (stream input)
      (let ((arena (gemini::checkout-arena-securely))
            (buf (gemini::checkout-buffer-securely)))
        (unwind-protect
             (signals predator-terminal-condition
               (handler-case
                   (gemini::%predator-read-internal stream :arena arena :buffer buf)
                 (predator-terminal-condition (c)
                   (is (eq :leading-zeroes (threat-reason c)))
                   (error c))))
          (gemini::return-arena-securely arena)
          (gemini::return-buffer-securely buf))))))

(test predator-supervisor-mitigation
  "Test that the supervisor successfully mitigates threats and returns clean values."
  (dolist (input '("INVALID" "123e4" "0123" "999999999999999999999999999999999999999999"))
    (with-binary-stream-from-string (stream input)
      (multiple-value-bind (val threat) (predator-read stream)
        (is (null val))
        (is (eq :threat-eliminated threat))))))

