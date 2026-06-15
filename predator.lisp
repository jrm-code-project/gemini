;;;; predator.lisp
;;;; Hardened S-expression parser: The Predator Reader v4.0 (Terminal State Machine)

(in-package "GEMINI")

(define-condition predator-terminal-condition (error)
  ((reason :initarg :reason :reader threat-reason)))

;; THE PARSER TIER (Pure State Machine, No Heap Allocation)
(defstruct (predator-context (:constructor make-predator-context))
  (stream nil :type stream)
  (arena nil :type simple-vector)
  (buffer nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (arena-ptr 0 :type fixnum)
  (unread-register -1 :type fixnum)
  (deadline 0 :type fixnum))

(defparameter *max-global-nodes* 4096)
(defparameter *buffer-size* 4096)

(defvar *arena-pool* nil)
(defvar *buffer-pool* nil)
(defvar *pool-lock* (sb-thread:make-mutex :name "predator-pool-lock"))

(defvar *thread-local-arena* nil)
(defvar *thread-local-buffer* nil)

;; Trie node structure for non-allocating symbol lookup
(defstruct trie-node
  (value nil)
  (terminal-p nil :type boolean)
  (transitions nil :type list)) ; list of (byte . trie-node)

(defparameter *default-vocabulary*
  '(;; Constants & Booleans
    "T" "NIL" "PI"

    ;; Logic & Conditionals
    "AND" "OR" "NOT" "IF" "WHEN" "UNLESS" "COND" "CASE" "ECASE" "CCASE" "TYPECASE" "ETYPECASE"

    ;; Variables, Binding & Mutation
    "LET" "LET*" "PROG" "PROG*" "DEFVAR" "DEFPARAMETER" "SETQ" "SETF" "PSETQ" "PSETF"
    "ROTATEF" "SHIFTF" "MULTIPLE-VALUE-BIND" "MULTIPLE-VALUE-SETQ" "DESTRUCTURING-BIND"

    ;; Functions & Lambdas
    "LAMBDA" "DEFUN" "DEFMACRO" "FLET" "LABELS" "MACROLET" "SYMBOL-MACROLET" "FUNCTION" "QUOTE"

    ;; Sequence/List/Cons Manipulation
    "CAR" "CDR" "CONS" "LIST" "LIST*" "APPEND" "CONCATENATE" "LENGTH" "REVERSE" "NREVERSE"
    "MEMBER" "FIND" "FIND-IF" "POSITION" "POSITION-IF" "COUNT" "COUNT-IF" "ASSOC" "ASSOC-IF"
    "RASSOC" "MAPCAR" "MAP" "REDUCE" "SUBSEQ" "FIRST" "SECOND" "THIRD" "FOURTH" "FIFTH"
    "REST" "LAST" "NTH" "BUTLAST" "NBUTLAST" "MAKE-LIST" "COPY-LIST" "COPY-ALIST" "COPY-TREE"
    "ADJOIN" "UNION" "NUNION" "INTERSECTION" "NINTERSECTION" "SET-DIFFERENCE" "NSET-DIFFERENCE"
    "SET-EXCLUSIVE-OR" "NSET-EXCLUSIVE-OR" "SUBSETP"

    ;; Tree operations
    "CAAR" "CADR" "CDAR" "CDDR" "CAAAR" "CAADR" "CADAR" "CADDR" "CDAAR" "CDADR" "CDDAR" "CDDDR"
    "CAAAAR" "CAAADR" "CAADAR" "CAADDR" "CADAAR" "CADADR" "CADDAR" "CADDDR"
    "CDAAAR" "CDAADR" "CDADAR" "CDADDR" "CDDAAR" "CDDADR" "CDDDAR" "CDDDDR"

    ;; Equality & Comparison
    "EQ" "EQL" "EQUAL" "EQUALP" "=" "/=" "<" ">" "<=" ">=" "MIN" "MAX"

    ;; Math & Arithmetic
    "+" "-" "*" "/" "1+" "1-" "ABS" "MOD" "REM" "EXPT" "SQRT" "ISQRT" "GCD" "LCM"
    "FLOOR" "CEILING" "ROUND" "TRUNCATE" "SIGNUM" "SIN" "COS" "TAN" "ASIN" "ACOS" "ATAN"
    "SINH" "COSH" "TANH" "ASINH" "ACOSH" "ATANH" "EXP" "LOG"

    ;; Type Predicates & Coercion
    "ATOM" "CONSP" "LISTP" "SYMBOLP" "NUMBERP" "INTEGERP" "FLOATP" "STRINGP" "CHARACTERP"
    "FUNCTIONP" "NULL" "TYPEP" "SUBTYPEP" "COERCE"

    ;; Type Names & Declarations
    "DECLARE" "TYPE" "FTYPE" "OPTIMIZE" "SPEED" "SAFETY" "SPACE" "DEBUG"
    "FIXNUM" "BIGNUM" "FLOAT" "SINGLE-FLOAT" "DOUBLE-FLOAT" "RATIO" "REAL" "NUMBER"
    "SYMBOL" "STRING" "CHARACTER" "SEQUENCE" "BOOLEAN"

    ;; Strings & Characters
    "MAKE-STRING" "STRING" "CHAR" "SCHAR" "STRING=" "STRING/=" "STRING<" "STRING>"
    "STRING<=" "STRING>=" "STRING-EQUAL" "STRING-NOT-EQUAL" "STRING-LESSP"
    "STRING-GREATERP" "STRING-UPCASE" "STRING-DOWNCASE" "STRING-CAPITALIZE"
    "STRING-TRIM" "STRING-LEFT-TRIM" "STRING-RIGHT-TRIM" "CHAR=" "CHAR/=" "CHAR<"
    "CHAR>" "CHAR<=" "CHAR>=" "CHAR-EQUAL" "CHAR-CODE" "CODE-CHAR" "CHAR-NAME" "NAME-CHAR"

    ;; Arrays & Vectors
    "MAKE-ARRAY" "AREF" "SVREF" "VECTOR" "ARRAY"

    ;; Hash Tables
    "MAKE-HASH-TABLE" "GETHASH" "REMHASH" "CLRHASH" "HASH-TABLE-COUNT" "HASH-TABLE-SIZE" "MAPHASH"

    ;; Control Flow & Execution
    "PROGN" "PROG1" "PROG2" "BLOCK" "RETURN" "RETURN-FROM" "TAGBODY" "GO"
    "LOOP" "LOOP-FINISH" "DOLIST" "DOTIMES" "WITH-SIMPLE-RESTART" "HANDLER-CASE"
    "HANDLER-BIND" "IGNORE-ERRORS" "UNWIND-PROTECT" "APPLY" "FUNCALL" "VALUES" "NTH-VALUE"

    ;; Objects / CLOS
    "DEFCLASS" "DEFMETHOD" "DEFGENERIC" "CALL-NEXT-METHOD" "NEXT-METHOD-P" "SLOT-VALUE"
    "SLOT-BOUNDP" "SLOT-MAKUNBOUND" "SLOT-EXISTS-P" "INITIALIZE-INSTANCE" "MAKE-INSTANCE"

    ;; Symbols & Introspection
    "GENSYM" "MAKE-SYMBOL" "SYMBOL-NAME" "SYMBOL-VALUE" "SYMBOL-FUNCTION"
    "BOUNDP" "FBOUNDP" "MAKUNBOUND" "FMAKUNBOUND" "IDENTITY" "CONSTANTLY" "COMPLEMENT"

    ;; Formatted Printing
    "FORMAT" "PRINT" "PRIN1" "PRINC" "TERPRI" "FRESH-LINE" "WRITE-TO-STRING"

    ;; Application specific vocabulary
    "STATUS" "OK" "ERROR" "PREDATOR" "MACHINE" "ARENA"))

(defun trie-insert (root bytes value)
  (let ((curr root))
    (loop for b across bytes do
      (let ((assoc (assoc b (trie-node-transitions curr))))
        (if assoc
            (setf curr (cdr assoc))
            (let ((new-node (make-trie-node)))
              (push (cons b new-node) (trie-node-transitions curr))
              (setf curr new-node)))))
    (setf (trie-node-value curr) value)
    (setf (trie-node-terminal-p curr) t)
    root))

(defun build-default-trie ()
  (let ((root (make-trie-node)))
    (dolist (word *default-vocabulary*)
      (let* ((sym (intern word "GEMINI"))
             (bytes (map '(vector (unsigned-byte 8)) #'char-code word)))
        (trie-insert root bytes sym)))
    root))

(defvar *predator-trie-root* (build-default-trie))

;; Secure Pool Management
(defun checkout-arena-securely ()
  (sb-thread:with-mutex (*pool-lock*)
    (if *arena-pool*
        (pop *arena-pool*)
        (make-array *max-global-nodes* :initial-element nil :adjustable nil :fill-pointer nil))))

(defun return-arena-securely (arena)
  (declare (type simple-vector arena))
  (fill arena nil)
  (sb-thread:with-mutex (*pool-lock*)
    (push arena *arena-pool*)))

(defun checkout-buffer-securely ()
  (sb-thread:with-mutex (*pool-lock*)
    (if *buffer-pool*
        (pop *buffer-pool*)
        (make-array *buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))))

(defun return-buffer-securely (buf)
  (declare (type (simple-array (unsigned-byte 8) (*)) buf))
  (fill buf 0)
  (sb-thread:with-mutex (*pool-lock*)
    (push buf *buffer-pool*)))

(defun log-threat-safely (condition)
  (format *error-output* "~&[PREDATOR THREAT ELIMINATED: ~A]~%" condition))

(defun sever-socket-connection (stream)
  (when (streamp stream)
    (ignore-errors
     (close stream :abort t))))

;; SUPERVISOR TIER (Interrupt-Safe, Leak-Proof Matrix)
(defmacro with-predator-supervisor ((stream-var un-evaluated-stream) &body body)
  (let ((stream-sym (gensym "STREAM"))
        (arena-sym (gensym "ARENA"))
        (buffer-sym (gensym "BUFFER")))
    `(let ((,stream-sym ,un-evaluated-stream))
       ;; Invert the protection matrix: Disable interrupts BEFORE checkout
       (sb-sys:without-interrupts
         (let ((,arena-sym (checkout-arena-securely))
               (,buffer-sym (checkout-buffer-securely)))
           (unwind-protect
               ;; Re-enable interrupts only inside the safe execution block
               (sb-sys:allow-with-interrupts
                 (handler-case
                     (let ((,stream-var ,stream-sym)
                           (*thread-local-arena* ,arena-sym)
                           (*thread-local-buffer* ,buffer-sym))
                       ,@body)
                   ;; Catch SERIOUS-CONDITION, never root 'CONDITION'
                   (serious-condition (c)
                     (log-threat-safely c)
                     (sever-socket-connection ,stream-sym)
                     (values nil :threat-eliminated))))
             ;; Absolute Resource Scrubbing (Executed with interrupts disabled)
             (return-arena-securely ,arena-sym)
             (return-buffer-securely ,buffer-sym)))))))

;; Binary Input & Lookahead Loop
(defun next-byte (ctx &optional (eof-error-p t))
  (declare (optimize (speed 3) (safety 0)))
  (when (> (get-internal-real-time) (predator-context-deadline ctx))
    (error 'predator-terminal-condition :reason :deadline-exceeded))
  (let ((reg (predator-context-unread-register ctx)))
    (if (>= reg 0)
        (progn (setf (predator-context-unread-register ctx) -1) reg)
        (let ((b (read-byte (predator-context-stream ctx) nil :eof)))
          (when (eq b :eof)
            (if eof-error-p
                (error 'predator-terminal-condition :reason :unexpected-eof)
                (return-from next-byte :eof)))
          b))))

(defun peek-byte-ctx (ctx &optional (eof-error-p t))
  (let ((b (next-byte ctx eof-error-p)))
    (unless (eq b :eof)
      (setf (predator-context-unread-register ctx) b))
    b))

;; Arena operations
(defun arena-push (ctx val)
  (let ((ptr (predator-context-arena-ptr ctx)))
    (when (>= ptr (length (predator-context-arena ctx)))
      (error 'predator-terminal-condition :reason :arena-exhausted))
    (setf (svref (predator-context-arena ctx) ptr) val)
    (setf (predator-context-arena-ptr ctx) (1+ ptr))
    ptr))

;; Whitespace loop
(defun consume-whitespace (ctx)
  (declare (optimize (speed 3) (safety 0)))
  (loop
    (let ((b (peek-byte-ctx ctx nil)))
      (if (or (= b #x20) (= b #x09) (= b #x0A) (= b #x0D))
          (next-byte ctx nil)
          (return)))))

;; Number accumulation
(defun accumulate-digit (acc digit-val)
  (declare (type fixnum acc digit-val)
           (optimize (speed 3) (safety 0)))
  (let ((limit (floor most-positive-fixnum 10)))
    (if (> acc limit)
        (error 'predator-terminal-condition :reason :numeric-overflow)
        (let ((acc-times-10 (* acc 10)))
          (if (> digit-val (- most-positive-fixnum acc-times-10))
              (error 'predator-terminal-condition :reason :numeric-overflow)
              (+ acc-times-10 digit-val))))))

(defun parse-number (ctx sign)
  (declare (type (member -1 1) sign)
           (optimize (speed 3) (safety 0)))
  (let ((b (next-byte ctx t)))
    (declare (type fixnum b))
    (cond
      ;; Leading zero check
      ((= b #x30) ; '0'
       (let ((next-b (peek-byte-ctx ctx nil)))
         (if (and (not (eq next-b :eof)) (>= next-b #x30) (<= next-b #x39))
             (error 'predator-terminal-condition :reason :leading-zeroes)
             (arena-push ctx 0))))
      ;; Non-zero first digit
      ((and (>= b #x31) (<= b #x39))
       (let ((acc (- b #x30)))
         (declare (type fixnum acc))
         (loop
           (let ((next-b (peek-byte-ctx ctx nil)))
             (cond
               ((and (not (eq next-b :eof)) (>= next-b #x30) (<= next-b #x39))
                (next-byte ctx t) ; consume
                (setf acc (accumulate-digit acc (- next-b #x30))))
               ;; Reject exponent markers (e, E, d, D) or ratio (/) following/within a number
               ((and (not (eq next-b :eof))
                     (or (= next-b #x65) (= next-b #x45)   ; e, E
                         (= next-b #x64) (= next-b #x44)   ; d, D
                         (= next-b #x2F)))                 ; /
                (error 'predator-terminal-condition :reason :type-annihilation))
               ;; Delimiter check: number must be followed by a delimiter
               ((or (eq next-b :eof)
                    (= next-b #x20) (= next-b #x09) (= next-b #x0A) (= next-b #x0D)
                    (= next-b #x28) (= next-b #x29))
                (let ((val (* sign acc)))
                  (return (arena-push ctx val))))
               (t
                (error 'predator-terminal-condition :reason :invalid-token-delimiter)))))))
      (t
       (error 'predator-terminal-condition :reason :invalid-number-start)))))

(defun parse-symbol (ctx first-byte)
  (declare (type fixnum first-byte)
           (optimize (speed 3) (safety 0)))
  (let ((node *predator-trie-root*))
    ;; Move to the first byte's transition
    (let ((assoc (assoc first-byte (trie-node-transitions node))))
      (unless assoc
        (error 'predator-terminal-condition :reason :diverged-from-trie))
      (setf node (cdr assoc)))
    ;; Read subsequent bytes
    (loop
      (let ((next-b (peek-byte-ctx ctx nil)))
        (cond
          ;; Delimiter check: stop and check terminal node
          ((or (eq next-b :eof)
               (= next-b #x20) (= next-b #x09) (= next-b #x0A) (= next-b #x0D)
               (= next-b #x28) (= next-b #x29))
           (unless (trie-node-terminal-p node)
             (error 'predator-terminal-condition :reason :diverged-from-trie))
           (return (arena-push ctx (trie-node-value node))))
          (t
           (next-byte ctx t)
           (let ((assoc (assoc next-b (trie-node-transitions node))))
             (unless assoc
               (error 'predator-terminal-condition :reason :diverged-from-trie))
             (setf node (cdr assoc)))))))))

(defun parse-expression (ctx)
  (declare (optimize (speed 3) (safety 0)))
  (consume-whitespace ctx)
  (let ((b (next-byte ctx t)))
    (cond
      ;; Left parenthesis: start of list
      ((= b #x28) ; #\(
       (let ((start-ptr (arena-push ctx :list-start)))
         (let ((end-placeholder-ptr (arena-push ctx -1)))
           (loop
             (consume-whitespace ctx)
             (let ((next-b (peek-byte-ctx ctx t)))
               (cond
                 ((= next-b #x29) ; #\)
                  (next-byte ctx t) ; consume the #\)
                  (let ((end-ptr (arena-push ctx :list-end)))
                    (setf (svref (predator-context-arena ctx) end-placeholder-ptr) end-ptr)
                    (return-from parse-expression start-ptr)))
                 (t
                  (parse-expression ctx))))))))

      ;; Right parenthesis: unexpected closed parenthesis
      ((= b #x29) ; #\)
       (error 'predator-terminal-condition :reason :unexpected-closed-parenthesis))

      ;; Sign or digit: start of number
      ((or (= b #x2B) (= b #x2D) (and (>= b #x30) (<= b #x39))) ; #\+, #\-, or '0'-'9'
       (cond
         ((or (= b #x2B) (= b #x2D))
          (let ((next-b (peek-byte-ctx ctx t)))
            (if (and (>= next-b #x30) (<= next-b #x39))
                (parse-number ctx (if (= b #x2D) -1 1))
                (parse-symbol ctx b))))
         (t
          (setf (predator-context-unread-register ctx) b)
          (parse-number ctx 1))))

      ;; Otherwise, must be symbol
      (t
       (parse-symbol ctx b)))))

(defun reconstruct-ast (arena start-idx)
  "Reconstructs a Common Lisp S-expression from the flat ARENA starting at START-IDX."
  (let ((val (svref arena start-idx)))
    (cond
      ((eq val :list-start)
       (let ((end-idx (svref arena (1+ start-idx)))
             (elements nil))
         (let ((curr (+ start-idx 2)))
           (loop
             (when (>= curr end-idx)
               (return))
             (let ((el-val (svref arena curr)))
               (cond
                 ((eq el-val :list-start)
                  (let ((next-end-idx (svref arena (1+ curr))))
                    (push (reconstruct-ast arena curr) elements)
                    (setf curr (1+ next-end-idx))))
                 ((eq el-val :list-end)
                  (return))
                 (t
                  (push el-val elements)
                  (incf curr))))))
         (nreverse elements)))
      ((eq val :list-end)
       (error "Unexpected :list-end marker at reconstruct start index ~A" start-idx))
      (t
       val))))

(defun %predator-read-internal (stream &key (timeout-ms 500) (arena *thread-local-arena*) (buffer *thread-local-buffer*))
  "Direct internal read function without supervisor wrapper, throwing conditions."
  (let* ((deadline (+ (get-internal-real-time)
                      (floor (* timeout-ms internal-time-units-per-second) 1000)))
         (ctx (make-predator-context
               :stream stream
               :arena arena
               :buffer buffer
               :arena-ptr 0
               :unread-register -1
               :deadline deadline)))
    (consume-whitespace ctx)
    (let ((first-b (peek-byte-ctx ctx nil)))
      (if (eq first-b :eof)
          (values nil :eof)
          (let ((start-idx (parse-expression ctx)))
            (values (reconstruct-ast arena start-idx) :ok))))))

(defun predator-read (stream &key (timeout-ms 500))
  "Parses a single S-expression from STREAM using Predator Reader v4.0.
Returns reconstructed AST, or (values nil :threat-eliminated) if an attack/error was intercepted."
  (with-predator-supervisor (safe-stream stream)
    (%predator-read-internal safe-stream :timeout-ms timeout-ms)))
