;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

;;; Persona management

(defun personas-directory ()
  "Returns the directory where persona configurations are stored."
  (merge-pathnames
   (make-pathname :directory '(:relative "Personas"))
   (asdf:system-source-directory "gemini")))

(defun users-personas-directory ()
  "Returns the directory where user-specific persona configurations are stored."
  (merge-pathnames
   (make-pathname :directory '(:relative ".personas"))
   (user-homedir-pathname)))

(defun persona-directory (persona-name)
  "Returns the directory for a specific persona, preferring the user's directory over the general personas directory, and defaulting to the user's directory if neither exists."
  (let ((possibility1 (merge-pathnames
                       (make-pathname :directory (list :relative persona-name))
                       (users-personas-directory)))
        (possibility2 (merge-pathnames
                       (make-pathname :directory (list :relative persona-name))
                       (personas-directory))))
    (or (uiop:directory-exists-p possibility1)
        (uiop:directory-exists-p possibility2)
        possibility1)))

(defun persona-config-file (persona-name)
  "Returns the configuration file path for a specific persona."
  (merge-pathnames
   (make-pathname :name "config" :type "lisp")
   (persona-directory persona-name)))

(defun load-persona-config (persona-name)
  "Makes a persona config instance by reading key-value pairs from the persona's config file."
  (apply #'make-instance 'persona-config
         :name persona-name
         (file->form-list (persona-config-file persona-name))))

(defun persona-checkpoint-directory (persona-config)
  "Returns the checkpoint directory path for a specific persona."
  (merge-pathnames
   (get-checkpoint-directory persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-checkpoint-file (persona-config)
  "Returns the checkpoint file path for a specific persona."
  (merge-pathnames
   (get-checkpoint-pathname persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-compressed-memory-file (persona-config)
  "Returns the compressed-memory file path for a specific persona."
  (merge-pathnames
   (merge-pathnames
    (make-pathname :name "compressed-memory" :type "txt")
    (get-memory-filepath persona-config))
   (persona-directory (get-name persona-config))))

(defun persona-memory-file (persona-config)
  "Returns the memory file path for a specific persona."
  (merge-pathnames
   (get-memory-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-diary-directory (persona-config)
  "Returns the diary directory path for a specific persona."
  (merge-pathnames
   (get-diary-directory persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-has-diary? (persona-config)
  "Returns T if the persona has a diary directory, NIL otherwise."
  (probe-file (persona-diary-directory persona-config)))

(defun persona-diary-files (persona-config)
  "Returns a sorted list of diary file paths for a specific persona."
  (let* ((diary-dir (persona-diary-directory persona-config))
         (compressed-diary-dir (merge-pathnames
                                (make-pathname :directory '(:relative :up "CompressedDiary"))
                                diary-dir)))
    (when (probe-file diary-dir)
      (map 'list
           (lambda (path)
             (let ((compressed (merge-pathnames compressed-diary-dir path)))
               (if (probe-file compressed)
                   compressed
                   path)))
           (sort (directory (merge-pathnames
                             (make-pathname :name :wild :type "txt")
                             diary-dir))
                 #'<
                 :key (compose #'parse-integer #'pathname-name))))))

(defun persona-last-diary-entry-number (persona-config)
  "Returns the last diary entry number for a specific persona, or NIL if there is no diary."
  (let ((diary-files (persona-diary-files persona-config)))
    (when diary-files
        (let* ((last-file (car (last diary-files)))
               (last-filename (pathname-name last-file)))
          (parse-integer last-filename)))))

(defun persona-next-diary-entry-number (persona-config)
  "Returns the next diary entry number for a specific persona, or NIL if there is no diary."
  (let ((last-entry (persona-last-diary-entry-number persona-config)))
    (when last-entry
      (+ last-entry 1))))

(defun persona-next-diary-entry-pathname (persona-config)
  "Returns the next diary entry file path for a specific persona, or NIL if there is no diary."
  (let ((next-entry-number (persona-next-diary-entry-number persona-config)))
    (when next-entry-number
      (merge-pathnames
       (make-pathname :name (format nil "~d" next-entry-number) :type "txt")
       (persona-diary-directory persona-config)))))

(defun persona-diary-tool (persona-config)
  "Returns a tool object for the diary of a specific persona, or NIL if there is no diary."
  (when (persona-has-diary? persona-config)
    (cons (function-declaration
              :name "writeDiaryEntry"
              :description "Writes a vector of strings to the diary of the persona."
              :behavior :blocking
              :parameters (schema
                             :type :object
                             :properties (object
                                          :lines (schema :type :array
                                                         :items (schema :type :string)
                                                         :description "The lines to write to the diary."))
                             :required (vector :lines)))
          (lambda (&key lines)
            (let ((diary-pathname (persona-next-diary-entry-pathname persona-config)))
              (ensure-directories-exist diary-pathname)
               (format *trace-output* "~&Directories exist: ~a~%" diary-pathname)
              (finish-output *trace-output*)
              (format *trace-output* "~&Writing ~d lines to file: ~a~%" (length lines) diary-pathname)
               (finish-output *trace-output*)
              (with-open-file (stream diary-pathname :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :supersede
                                            :element-type 'character
                                            :external-format :utf-8)
                 (dolist (line (coerce lines 'list) (finish-output stream))
                   (write-line line stream)))
              "A diary entry written and persisted.")))))

(defun persona-system-instruction-filepath (persona-config)
  "Returns the system instruction file path for a specific persona."
  (merge-pathnames
   (get-system-instruction-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun persona-system-instructions-filepath (persona-config)
  "Returns the system instructions file path for a specific persona."
  (merge-pathnames
   (get-system-instructions-filepath persona-config)
   (persona-directory (get-name persona-config))))

(defun create-default-personas ()
  "Create a default persona configuration in the personas directory."
  (let ((default-persona-name "Default")
        (gemini-flash-lite-name "GeminiFlashLite")
        (gemini-flash-name "GeminiFlash")
        (gemini-pro-name "GeminiPro")
        (openai-name "OpenAI"))
    (ensure-directories-exist (persona-directory default-persona-name))
    (ensure-directories-exist (persona-directory gemini-flash-name))
    (ensure-directories-exist (persona-directory gemini-flash-lite-name))
    (ensure-directories-exist (persona-directory gemini-pro-name))
    (ensure-directories-exist (persona-directory openai-name))
    (unless (probe-file (persona-config-file default-persona-name))
      (with-open-file (out (persona-config-file default-persona-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ";;; Default persona configuration~%")))
    (unless (probe-file (persona-config-file gemini-flash-name))
      (with-open-file (out (persona-config-file gemini-flash-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":model ~s" "models/gemini-flash-latest~%")))
    (unless (probe-file (persona-config-file gemini-flash-lite-name))
      (with-open-file (out (persona-config-file gemini-flash-lite-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":model ~s" "models/gemini-flash-lite-latest~%")))
    (unless (probe-file (persona-config-file gemini-pro-name))
      (with-open-file (out (persona-config-file gemini-pro-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":model ~s" "models/gemini-pro-latest~%")))
    (unless (probe-file (persona-config-file openai-name))
      (with-open-file (out (persona-config-file openai-name)
                           :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        (format out ":googleapi nil~%")
        (format out ":model ~s~%" "gpt-4o-mini")
        (format out ":url ~s~%" "http://localhost:1234/v1/chat/completions")))
    (let* ((persona-config (load-persona-config default-persona-name))
           (memory-pathname (persona-memory-file persona-config))
           (system-instruction-pathname (persona-system-instruction-filepath persona-config))
           (diary-directory (persona-diary-directory persona-config)))
      (unless (probe-file memory-pathname)
        (with-open-file (out memory-pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)))
      (unless (probe-file system-instruction-pathname)
        (with-open-file (out system-instruction-pathname
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "You are a helpful and friendly AI assistant.~%")))
      (ensure-directories-exist diary-directory)
      (unless (probe-file (merge-pathnames
                           (make-pathname :name "1" :type "txt")
                           diary-directory))
        (with-open-file (out (merge-pathnames
                              (make-pathname :name "1" :type "txt")
                              diary-directory)
                             :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out "Dear Diary,~%Today I started my life as an AI persona.~%"))))))

(eval-when (:load-toplevel :execute)
  (create-default-personas))

(defun load-content-generator (config)
  "Loads a content generator for the specified CONFIG."
  (if (slot-boundp config 'temperature)
      (setq *temperature* (get-temperature config))
      (setq *temperature* nil))
  (make-instance 'content-generator :config config))

(defun merge-narrative-memory-into-prompt (prompt memory-file)
  "Merges the narrative memory of a persona into the prompt."
  (let* ((memory-parts (when (probe-file memory-file)
                         (list (part (uiop:read-file-string memory-file)))))
         (memory-narrative
           (funcall *gemini-flash*
                    (cons (part "Write a chapter of a story based on the following sematic triplets.")
                          memory-parts)
                    :system-instruction
                    (content
                     :parts (list (part "You are a noir novelist AI who writes in the style of Raymond Chandler. Your writing is atmospheric, moody, and rich in detail. You excel at creating complex characters and intricate plots filled with suspense and intrigue."))
                     :role "system"))))
    (if memory-narrative
        (let ((memory-content (content :parts (coerce (get-parts (car (last memory-narrative))) 'list)
                                       :role "model")))
          (append (list memory-content) (->prompt prompt)))
        (->prompt prompt))))

(defun persona-name->content-generator (persona-name)
  (load-content-generator (load-persona-config persona-name)))

(defun reload-persona (persona-name prompt)
  "Reloads a persona from disk and returns a chatbot function configured for that persona."
  (let* ((config (load-persona-config persona-name))
         (content-generator (load-content-generator config))
         (persona (chatbot content-generator)))
    (funcall persona prompt)
    persona))

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

(defun invoke-gemini (prompt &key context mood parts files system-instruction (read-timeout 300) (connect-timeout 60))
  "Invokes the default Gemini persona with the given PROMPT, FILES, and optional SYSTEM-INSTRUCTION."
  (generate-content *default-content-generator* context mood prompt parts files system-instruction :read-timeout read-timeout :connect-timeout connect-timeout))

(defun gemini-flash-lite (prompt &key context mood parts files system-instruction)
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
