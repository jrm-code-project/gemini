;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defvar *llm-returns-value-count* 0
  "Count of top-level LLM calls that successfully returned a value.")
(defvar *llm-returns-nothing-count* 0
  "Count of top-level LLM calls that completed but returned NIL or empty.")
(defvar *llm-abort-count* 0
  "Count of top-level LLM calls that aborted due to an error.")

(defun get-llm-stats ()
  "Returns the current counts for top-level LLM procedure outcomes as a plist."
  (list :returned-value *llm-returns-value-count*
        :returned-nothing *llm-returns-nothing-count*
        :aborted *llm-abort-count*))

(defun reset-llm-stats ()
  "Resets the top-level LLM procedure outcome counters to zero."
  (setf *llm-returns-value-count* 0)
  (setf *llm-returns-nothing-count* 0)
  (setf *llm-abort-count* 0)
  (values))

(defmacro with-llm-instrumentation (&body body)
  "Executes body, updating global counters based on whether it returns a true value, nil, or signals an error, without unwinding the stack prematurely."
  (let ((result-sym (gensym "RESULT")))
    `(handler-bind ((error (lambda (e)
                             (declare (ignore e))
                             (incf *llm-abort-count*)))) ; Just increment and decline to handle
       (let ((,result-sym (progn ,@body)))
         (if ,result-sym
             (incf *llm-returns-value-count*)
             (incf *llm-returns-nothing-count*))
         ,result-sym))))

(defun call-with-model-override (generator model thunk)
  "Executes THUNK, temporarily overriding the model of the given GENERATOR if MODEL is non-NIL."
  (if (null model)
      (funcall thunk)
      (let ((old-model (get-model generator)))
        (unwind-protect
             (progn
               (setf (get-model generator) model)
               (funcall thunk))
          (setf (get-model generator) old-model)))))

(defvar *content-generator-registry* (make-hash-table :test 'equal)
  "A thread-safe dynamic registry mapping content generator/model names to content-generator instances.")

(defvar *content-generator-registry-lock* (sb-thread:make-mutex :name "content-generator-registry-lock")
  "Mutex to protect the content generator registry under concurrent access.")

(defun normalize-registry-key (name)
  "Normalizes name (string, symbol, keyword) to a standard uppercase string key."
  (string-upcase (princ-to-string name)))

(defun register-content-generator (name content-generator)
  "Registers a CONTENT-GENERATOR under NAME (string, symbol, or keyword) in the registry."
  (sb-thread:with-mutex (*content-generator-registry-lock*)
    (setf (gethash (normalize-registry-key name) *content-generator-registry*) content-generator))
  content-generator)

(defun unregister-content-generator (name)
  "Removes a content generator from the registry by its NAME."
  (sb-thread:with-mutex (*content-generator-registry-lock*)
    (remhash (normalize-registry-key name) *content-generator-registry*)))

(defun find-content-generator (name)
  "Retrieves a registered content generator by NAME, returning the content-generator instance."
  (sb-thread:with-mutex (*content-generator-registry-lock*)
    (or (gethash (normalize-registry-key name) *content-generator-registry*)
        (let* ((name-str (princ-to-string name))
               (clean-name (if (and (str:starts-with? "*" name-str)
                                    (str:ends-with? "*" name-str))
                               (subseq name-str 1 (1- (length name-str)))
                               name-str))
               (star-sym (find-symbol (format nil "*~A*" (string-upcase clean-name)) "GEMINI"))
               (plain-sym (find-symbol (string-upcase clean-name) "GEMINI")))
          (or (when (and star-sym (boundp star-sym))
                (symbol-value star-sym))
              (when (and plain-sym (boundp plain-sym))
                (symbol-value plain-sym)))))))

(defun list-content-generators ()
  "Returns a sorted list of all registered content generator names as strings."
  (sb-thread:with-mutex (*content-generator-registry-lock*)
    (let ((keys '()))
      (maphash (lambda (k v)
                 (declare (ignore v))
                 (push k keys))
               *content-generator-registry*)
      (sort keys #'string<))))

(defun invoke-model-with-session (session model-name prompt &rest args &key context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout)
  "Invokes a content-generator model from the registry by its NAME with the given PROMPT and arguments, using SESSION."
  (declare (ignore context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout))
  (let ((generator (or (find-content-generator model-name)
                       (error "Model/Generator ~s is not registered." model-name))))
    (apply #'invoke-generator-with-session session generator prompt args)))

(defun invoke-model (model-name prompt &rest args &key context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout)
  "Invokes a content-generator model from the registry by its NAME with the given PROMPT and arguments."
  (declare (ignore context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout))
  (apply #'invoke-model-with-session (ensure-runtime-session) model-name prompt args))

;; Populate the default generators on system load
(eval-when (:load-toplevel :execute)
  (register-content-generator "invoke-gemini" *default-content-generator*)
  (register-content-generator "gemini" *default-content-generator*)
  (register-content-generator "gemini-flash-lite" *gemini-flash-lite*)
  (register-content-generator "gemini-uncensored" *gemini-uncensored*)
  (register-content-generator "gemini-flash" *gemini-flash*)
  (register-content-generator "gemini-pro" *gemini-pro*)
  (register-content-generator "qwen" *qwen*))

(defun invoke-generator-with-session (session content-generator prompt
                                     &key context mood parts files file system-instruction
                                       model tools tool-config read-timeout connect-timeout)
  "Invokes CONTENT-GENERATOR with SESSION installed as the active runtime session."
  (let ((files (if file (list file) files)))
    (let ((session* (ensure-runtime-session session)))
      (let ((*default-repl-session* session*))
        (with-llm-instrumentation
          (call-with-model-override content-generator model
            (lambda ()
              (generate-content content-generator context mood prompt parts files system-instruction
                               :tools tools
                               :tool-config tool-config
                               :read-timeout read-timeout
                               :connect-timeout connect-timeout))))))))

(defun content->text (content)
  (if (null content)
      ""
      (reduce (lambda (l r)
                (concatenate 'string l (string #\Newline) r))
              (remove nil (map 'list (lambda (part)
                                       (when (text-part? part)
                                         (get-text part)))
                               (get-parts content))))))

(defun content->sexp (content)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string (content->text content)))))

(macrolet ((define-entry-point (name registry-key)
             (let ((with-session-name (intern (format nil "~A-WITH-SESSION" (symbol-name name))
                                              (symbol-package name))))
               `(progn
                  (defun ,with-session-name (session prompt &key context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout)
                    (let ((generator (or (find-content-generator ',registry-key)
                                         (error "Model/Generator ~s not found in registry." ',registry-key))))
                      (invoke-generator-with-session session generator prompt
                                                     :context context
                                                     :mood mood
                                                     :parts parts
                                                     :files files
                                                     :file file
                                                     :system-instruction system-instruction
                                                     :model model
                                                     :tools tools
                                                     :tool-config tool-config
                                                     :read-timeout read-timeout
                                                     :connect-timeout connect-timeout)))
                  (defun ,name (prompt &key context mood parts files file system-instruction model tools tool-config read-timeout connect-timeout)
                    (,with-session-name (ensure-runtime-session) prompt
                                        :context context
                                        :mood mood
                                        :parts parts
                                        :files files
                                        :file file
                                        :system-instruction system-instruction
                                        :model model
                                        :tools tools
                                        :tool-config tool-config
                                        :read-timeout read-timeout
                                        :connect-timeout connect-timeout))))))
  (define-entry-point invoke-gemini "gemini")
  (define-entry-point gemini-flash-lite "gemini-flash-lite")
  (define-entry-point gemini-uncensored "gemini-uncensored")
  (define-entry-point gemini-flash "gemini-flash")
  (define-entry-point gemini-pro "gemini-pro")
  (define-entry-point qwen "qwen"))

;; (defun invoke-gemini (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
;;   "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
;;   (with-llm-instrumentation
;;     (call-with-model-override *default-content-generator* model
;;       (lambda ()
;;         (generate-content *default-content-generator* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

;; (defun gemini-flash-lite (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
;;   (with-llm-instrumentation
;;     (call-with-model-override *gemini-flash-lite* model
;;       (lambda ()
;;         (generate-content *gemini-flash-lite* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

;; (defun gemini-uncensored (prompt &key context mood parts files system-instruction model (connect-timeout 60) (read-timeout 300))
;;   (with-llm-instrumentation
;;     (call-with-model-override *gemini-uncensored* model
;;       (lambda ()
;;         (generate-content *gemini-uncensored* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout)))))

;; (defun gemini-flash (prompt &key context mood parts files system-instruction model (read-timeout 300) (connect-timeout 60))
;;   "Fixed typo in lambda list to ensure timeouts actually pass through."
;;   (with-llm-instrumentation
;;     (call-with-model-override *gemini-flash* model
;;       (lambda ()
;;         (generate-content *gemini-flash* context mood prompt parts files system-instruction 
;;                           :read-timeout read-timeout :connect-timeout connect-timeout)))))

;; (defun gemini-pro (prompt &key context mood parts files system-instruction model)
;;   (with-llm-instrumentation
;;     (call-with-model-override *gemini-pro* model
;;       (lambda ()
;;         (generate-content *gemini-pro* context mood prompt parts files system-instruction)))))

(defun prompt-predicate-with-session (session prompt &key context mood parts files file system-instruction)
  (let ((files (if file (list file) files)))
    (let* ((session* (ensure-runtime-session session))
           (ans (let ((*default-repl-session* session*))
                  (content->text
                   (without-personality
                     (generate-content *gemini-flash-lite* context mood
                                       (format nil "~a~%Answer with `T` if true, `NIL` if false." prompt)
                                       parts files system-instruction))))))
      (cond ((search "T" (string-upcase ans)) t)
            ((search "NIL" (string-upcase ans)) nil)
            (t nil)))))

(defun prompt-predicate (prompt &key context mood parts files file system-instruction)
  (prompt-predicate-with-session (ensure-runtime-session) prompt
                                 :context context
                                 :mood mood
                                 :parts parts
                                 :files files
                                 :file file
                                 :system-instruction system-instruction))

(defun flash-compress-with-session (session text)
  (let ((session* (ensure-runtime-session session)))
    (let ((*default-repl-session* session*))
      (content->text
       (without-personality
         (generate-content *gemini-flash* nil nil (format nil "Condense the following text for reduced tokens.  Retain all meaning:~%~%~a" text) nil nil
 "Task: Lossless token compression.
 Constraint: Preserve 100% of facts and meaning.
 Output: Raw condensed text only. No preamble. No chatter."))))))

(defun flash-compress (text)
  (flash-compress-with-session (ensure-runtime-session) text))

(defparameter *compare-files-prompt*
  "Analyze the two provided files and determine their semantic similarity on a scale from 0.0 to 1.0, where 0.0 indicates the files are entirely unrelated and 1.0 indicates they are semantically identical. Output the score as a single real number, excluding all other text, explanations, or commentary.")

(defun similarity (f1 f2)
  (without-personality
    (parse-float-safely
     (content->text
      (gemini-flash-lite *compare-files-prompt* :files (list f1 f2))))))

(defun multiprompt (generator prompt-list &optional last-part)
  (if (null prompt-list)
      last-part
      (let ((response (if last-part
                          (funcall generator (list (car prompt-list) last-part))
                          (funcall generator (car prompt-list)))))
        (multiprompt generator (cdr prompt-list) (content->text response)))))

(defun resolve-model-string (model)
  (typecase model
    (null nil)
    (string model)
    (symbol
     (let* ((name (string-downcase (symbol-name model)))
            (clean-name (if (and (> (length name) 7) (string= (subseq name 0 7) "gemini-"))
                            name
                            name)))
       ;; check if registered
       (let ((m (find-model clean-name)))
         (if m
             (get-model-id m)
             (let ((m2 (find-model (concatenate 'string "models/" clean-name))))
               (if m2
                   (get-model-id m2)
                   (concatenate 'string "models/" clean-name)))))))))

(defun invoke-interaction-with-session (session prompt &key (model :gemini-3.5-flash) agent background tool-configs receiver)
  "Sends a stateful prompt using the Interactions API.
   Returns (values steps raw-response)."
(let* ((session* (ensure-runtime-session session))
       (backend (make-instance 'interactions-backend))
       (payload (make-hash-table :test 'equal)))
  (assert (or model agent) nil "Must specify either model or agent.")
  (if agent
      (setf (gethash "agent" payload) (string-downcase (string-upcase (princ-to-string agent))))
      (setf (gethash "model" payload) (resolve-model-string model)))

  (setf (gethash "input" payload) (build-interactions-input prompt))

  (when background
    (setf (gethash "background" payload) t))
  (when tool-configs
    (setf (gethash "tools" payload) tool-configs))
  (let ((*default-repl-session* session*))
    (invoke-backend backend (or agent model) payload :receiver receiver))))

(defun invoke-interaction (prompt &key (model :gemini-3.5-flash) agent background tool-configs receiver)
  "Sends a stateful prompt using the Interactions API.
   Returns (values steps raw-response)."
  (invoke-interaction-with-session (ensure-runtime-session)
                                   prompt
                                   :model model
                                   :agent agent
                                   :background background
                                   :tool-configs tool-configs
                                   :receiver receiver))
