;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defvar *default-content-generator*)
(defvar *gemini-pro*)
(defvar *gemini-flash*)
(defvar *gemini-flash-lite*)
(defvar *gemini-uncensored*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*default-content-generator* 'variable) "The default content generator instance.")
  (setf (documentation '*gemini-flash* 'variable) "The content generator for Gemini flash.")
  (setf (documentation '*gemini-flash-lite* 'variable) "The content generator for Gemini flash lite.")
  (setf (documentation '*gemini-pro* 'variable) "The content generator for Gemini Pro.")
  (setf (documentation '*gemini-uncensored* 'variable) "The content generator for Gemini Uncensored."))

(eval-when (:load-toplevel :execute)
  (setq *default-content-generator* (persona-name->content-generator "Default"))
  (setq *gemini-flash* (persona-name->content-generator "GeminiFlash"))
  (setq *gemini-flash-lite* (persona-name->content-generator "GeminiFlashLite"))
  (setq *gemini-pro* (persona-name->content-generator "GeminiPro"))
  (setq *gemini-uncensored* (persona-name->content-generator "Gemma4E4bUncensored"))
  )

(defun content->text (content)
  (reduce (lambda (l r)
            (concatenate 'string l (string #\Newline) r))
          (remove nil (map 'list (lambda (part)
                                   (when (text-part? part)
                                     (get-text part)))
                           (get-parts content)))))

(defun content->sexp (content)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read-from-string (content->text content)))))

(defun generate-content (content-generator context mood prompt parts files system-instruction &key (read-timeout 300) (connect-timeout 60))
  "Evaluates a prompt, manages turbo/model state, builds the payload, and executes the reinvoke loop.

  CONTENT-GENERATOR: The content generator object containing configuration and state.
  CONTEXT: A list of content objects representing the conversation history or context.
  PROMPT: The new prompt to evaluate, which can be a string, content object, or list of content.
  PARTS: Optional list of parts to include in the prompt.  Not concatenated to the context.
  FILES: Optional list of file specifications to include in the prompt.  Not concatenated to the context.
  SYSTEM-INSTRUCTION: Optional system instruction content to guide the model's response.
"
  (cond ((and (consp prompt) (eq (car prompt) :set-model!))
         (setf (get-model content-generator) (cadr prompt)))
        ((turbo-prompt? prompt)
         (%generate-content content-generator
                            context mood (subseq prompt 1) parts files system-instruction (char prompt 0) 0
                            :read-timeout read-timeout :connect-timeout connect-timeout))
        (t
         (%generate-content content-generator
                            context mood prompt parts files system-instruction nil 0
                                                        :read-timeout read-timeout :connect-timeout connect-timeout))))

(defun %generate-content (content-generator context mood prompt parts files system-instruction turbo depth &key (read-timeout 300) (connect-timeout 60))

  (when (= (mod depth 16) 15)
    (cerror "Continue content generation" "Possible infinite loop in content generation."))

  (multiple-value-bind (payload prompt-contents-and-context)
      (build-gemini-payload content-generator context mood prompt turbo parts files system-instruction)
          
    (let ((current-model (or (and turbo
                                  (cdr (assoc turbo +turbo-mapping+)))
                             (get-model content-generator)
                             +default-model+)))
      (restart-case

          (multiple-value-bind (response* usage-metadata)
              (handler-bind ((dexador.error:http-request-service-unavailable
                               (lambda (c)
                                 (declare (ignore c))
                                 (let ((restart (find-restart 'use-weaker-model)))
                                   (when (and restart (not (equal current-model +default-model+)))
                                     (invoke-restart restart))))))
                (%invoke-gemini content-generator current-model payload :read-timeout read-timeout :connect-timeout connect-timeout))
            (let* ((candidates (get-candidates response*))
                   (first-candidate (typecase candidates
                                      (cons (car candidates))
                                      (vector (when (plusp (length candidates))
                                                (aref candidates 0))))))
              (when usage-metadata

                (when (and (> (or (get-thoughts-token-count usage-metadata) 0) 10)
                           (< (or (get-thoughts-token-count usage-metadata) 0) 10000)
                           (< (or (get-candidates-token-count usage-metadata) 0) 2)
                           (< depth 5))
                  (format *trace-output* "~&;; Response has lots of thoughts (~a tokens) but very little content (~a token~:*~P). Continuing prompt.~%" (get-thoughts-token-count usage-metadata)
                          (get-candidates-token-count usage-metadata))
                  (return-from %generate-content
                    (let* ((content (or (get-content first-candidate)
                                        (content :parts (list (part "[Empty Response]"))))))
                      (%generate-content content-generator
                                         (append prompt-contents-and-context (list content))
                                         mood
                                         prompt
                                         parts
                                         files
                                         system-instruction
                                         turbo
                                         (1+ depth)))))


                (unless (> (or (get-candidates-token-count usage-metadata) 0) 0)
                  (format *trace-output* "~&;; Response too thin, retrying with stronger model.~%")
                  (return-from %generate-content
                    (let* ((content (or (get-content first-candidate)
                                        (content :parts (list (part "[Empty Response]"))))))
                      (%generate-content content-generator
                                         (append prompt-contents-and-context (list content))
                                         mood
                                         (continue-prompt)
                                         parts
                                         files
                                         system-instruction
                                         (elt "$*%" (random (length "$*%")))
                                         (1+ depth))))))
                            
              (when *echo-result*
                (print-text (get-bowdlerize content-generator) response*))
                              
              (let ((function-calls (extract-function-calls-from-results response*)))
                (cond (function-calls
                       (let ((function-results
                               (map 'list (compose (default-process-function-call content-generator)
                                                   #'get-function-call)
                                    function-calls)))
                         (assert (list-of-parts? function-results) ()
                                 "Expected function-results to be a list of parts.")
                         ;; FIX: Append the function response to the context 
                         ;; and recurse with the ORIGINAL prompt.
                         (let ((function-content (content :parts function-results :role "function")))
                           (%generate-content content-generator
                                              ;; Append original prompt turn AND function result to context
                                              (append prompt-contents-and-context (list function-content))
                                              mood
                                              prompt ;; use original prompt to keep goal active
                                              parts
                                              files
                                              system-instruction
                                              turbo
                                              (1+ depth)))))
                      ;; note, we can return NIL here if the first candidate has no content.
                      (first-candidate (get-content first-candidate))))))
        (use-weaker-model ()
          :report (lambda (s)
                    (format s "Switch from ~a to ~a and retry." current-model +default-model+))
          (%generate-content content-generator context mood prompt parts files system-instruction nil (1+ depth)
                             :read-timeout read-timeout
                             :connect-timeout connect-timeout))))))

(defun invoke-gemini (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
  (generate-content *default-content-generator* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout))

(defun gemini-flash-lite (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  (generate-content *gemini-flash-lite* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout))

(defun gemini-uncensored (prompt &key context mood parts files system-instruction (connect-timeout 60) (read-timeout 300))
  (generate-content *gemini-uncensored* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout))

(defun prompt-predicate (prompt &key context mood parts files system-instruction)
  (let ((ans (content->text
              (without-personality
                (generate-content *gemini-flash-lite* context mood
                                  (format nil "~a~%Answer with `T` if true, `NIL` if false." prompt)
                                  parts files system-instruction)))))
    (cond ((search "T" (string-upcase ans)) t)
          ((search "NIL" (string-upcase ans)) nil)
          (t nil))))

(defun gemini-flash (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  "Fixed typo in lambda list to ensure timeouts actually pass through."
  (generate-content *gemini-flash* context mood prompt parts files system-instruction 
                    :read-timeout read-timeout :connect-timeout connect-timeout))

(defun flash-compress (text)
  (content->text
   (without-personality
     (generate-content *gemini-flash* nil nil (format nil "Condense the following text for reduced tokens.  Retain all meaning:~%~%~a" text) nil nil 
 "Task: Lossless token compression.
 Constraint: Preserve 100% of facts and meaning.
 Output: Raw condensed text only. No preamble. No chatter."))))

(defun gemini-pro (prompt &key context mood parts files system-instruction)
  (generate-content *gemini-pro* context mood prompt parts files system-instruction))

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
