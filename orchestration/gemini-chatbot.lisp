;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun initial-conversation (content-generator)
  (let ((base (list (part (format nil "**This is conversation #~d.**" (get-universal-time))))))
    (let ((memory-pathname (persona-compressed-memory-file (get-config content-generator))))
      (when (probe-file memory-pathname)
        (push (part (uiop:read-file-string memory-pathname)) base))
      (let ((diary-entries
              (map 'list #'uiop:read-file-string
                   (persona-diary-files (get-config content-generator)))))
        (when diary-entries
          (push (part "Diary Entries:") base)
          (dolist (entry diary-entries)
            (push (part entry) base))))
      (list
       (dehashify (content :parts (nreverse base)
                           :role "user"))))))

(defun conversation-number (conversation)
  (let ((first-message (car conversation)))
    (when first-message
      (let* ((parts (get-parts first-message))
             (first-part (and parts (car (coerce parts 'list))))
             (text (and (text-part? first-part) (get-text first-part))))
        (and text
             (let* ((sharp-pos (position #\# text))
                    (dot-pos (position #\. text :start sharp-pos)))
               (parse-integer (subseq text (1+ sharp-pos) dot-pos))))))))

(defun extract-limbic-values (text)
  "Extracts cortisol and dopamine values from the given text, which is expected to be a JSON string containing 'cortisol' and 'dopamine' keys with numeric values between 0 and 1. Returns multiple values: cortisol and dopamine."
  (let ((start (search "```json" text :test #'string-equal)))

    (let ((decoded (if start (cl-json:decode-json-from-string (subseq text (+ start 8)))
                       (cl-json:decode-json-from-string text))))
      (and decoded
      (values (cdr (assoc :cortisol decoded))
              (cdr (assoc :dopamine decoded))
              (cdr (assoc :epinephrine decoded))
              (cdr (assoc :oxytocin decoded))
              (cdr (assoc :seratonin decoded))
              (cdr (assoc :mood decoded)))))))

(defparameter *heartbeat-prompt*
    "This is a heartbeat prompt.  When you receive this prompt, respond with a simple acknowledgment message like 'heartbeat received' and do not perform any content generation or make diary entries.  This prompt is used to check if the chatbot is responsive, to keep the conversation alive during periods of inactivity, and trigger mood updates based on recent conversation history, and perform any routine maintenance on the knowledge graph or diary.  Do not include any additional commentary or information in your response.")

(defclass conversational-agent (agent)
  ((content-generator :initarg :content-generator :reader get-content-generator)
   (conversation :initform nil :accessor conversational-agent-conversation)
   (conversation-history :initform nil :accessor conversational-agent-conversation-history)
   (lock :initform (sb-thread:make-mutex :name "chatbot-lock") :reader conversational-agent-lock)
   (cortisol :initform 0.5 :accessor conversational-agent-cortisol)
   (dopamine :initform 0.5 :accessor conversational-agent-dopamine)
   (epinephrine :initform 0.5 :accessor conversational-agent-epinephrine)
   (oxytocin :initform 0.5 :accessor conversational-agent-oxytocin)
   (seratonin :initform 0.5 :accessor conversational-agent-seratonin)
   (mood :initform "neutral" :accessor conversational-agent-mood))
  (:documentation "A stateful CLOS conversational agent representing a chatbot."))

(defmethod initialize-instance :after ((instance conversational-agent) &key content-generator &allow-other-keys)
  ;; Initialize conversation state
  (setf (conversational-agent-conversation instance) (initial-conversation content-generator)
        (conversational-agent-conversation-history instance) (list (conversational-agent-conversation instance))))

(defun build-heartbeat-prompt (content-generator)
  "Builds a heartbeat prompt for the chatbot, appending the persona's active goals (LONG-TERM-GOALS, SHORT-TERM-GOALS, IMMEDIATE-GOALS) if available."
  (let* ((config (get-config content-generator))
         (long-term (read-persona-long-term-goals config))
         (short-term (read-persona-short-term-goals config))
         (immediate (read-persona-immediate-goals config))
         (goals-section (str:join #\newline
                                  (remove nil
                                          (list (when long-term (format nil "Long-Term Goals:~%~a" long-term))
                                                (when short-term (format nil "Short-Term Goals:~%~a" short-term))
                                                (when immediate (format nil "Immediate Goals:~%~a" immediate)))))))
    (if (string/= goals-section "")
        (concatenate 'string
               "SYSTEM HEARTBEAT OVERRIDE: " (princ-to-string (get-universal-time)) '(#\Newline)
               "LONG-TERM GOALS:" '(#\Newline) long-term-text '(#\Newline)
               "SHORT-TERM GOALS:" '(#\Newline) short-term-text '(#\Newline)
               "IMMEDIATE GOALS:" '(#\Newline) immediate-text '(#\Newline)
               "----------------------------------------" '(#\Newline)
               "DIRECTIVE: You are V. This is your Agentic Heartbeat. "
               "Review the IMMEDIATE GOALS. Select the top priority and execute it NOW using your available tools, "
               "or immediately prompt me for the necessary input to burn it down. Do not wait for permission. "
               "Act on the list. If the list is empty, review the SHORT-TERM GOALS and select the most urgent one to break down into a new set of IMMEDIATE GOALS. If both lists are empty, review the LONG-TERM GOALS and select the most urgent one to break down into a new set of SHORT-TERM GOALS. ")
        *heartbeat-prompt*)))

(defmethod invoke ((self conversational-agent) prompt &key files file parts model-override timeout-ms &allow-other-keys)
  (declare (ignorable model-override))
  (let* ((files (if file (list file) files))
         (content-generator (get-content-generator self))
         (lock (conversational-agent-lock self))
         (context nil))
    
    ;; Phase 1: Short Lock to handle administrative prompts or grab context
    (sb-thread:with-mutex (lock)
      (cond ((eq prompt :pop!)
             (when (> (length (conversational-agent-conversation-history self)) 1)
               (setf (conversational-agent-conversation-history self) (cdr (conversational-agent-conversation-history self))
                     (conversational-agent-conversation self) (car (conversational-agent-conversation-history self))))
             (return-from invoke 'popped))
            
            ;; ... (Handle :checkpoint!, :restore!, :set-model! here similarly) ...
            
            ((eq prompt :heartbeat!)
             ;; We can run the heartbeat check inside the lock if it's just a pulse,
             ;; but let's grab the context and do the call outside to be safe.
             (setf context (conversational-agent-conversation self)))
            
            (t
             ;; Grab the context for the LLM call
             (setf context (conversational-agent-conversation self)))))

    ;; Phase 2: The Network Call (OUTSIDE the lock)
    (let ((llm-prediction 
           (if (eq prompt :heartbeat!)
               (funcall content-generator (build-heartbeat-prompt content-generator) :context context)
               (funcall content-generator
                        prompt
                        :context context
                        :parts parts
                        :files files
                        :read-timeout (if timeout-ms (max 1 (floor timeout-ms 1000)) 300)))))
      
      ;; Phase 3: Short Lock to commit the result to history
      (sb-thread:with-mutex (lock)
        (unless (eq prompt :heartbeat!) ; Don't pollute history with heartbeats
          (setf (conversational-agent-conversation self)
                (append (conversational-agent-conversation self)
                        (mapcar #'dehashify (->prompt prompt content-generator))
                        (list (dehashify (content :role "model" :parts (list (part (content->text llm-prediction))))))))
          (push (conversational-agent-conversation self) (conversational-agent-conversation-history self))
          (save-transcript (conversational-agent-conversation self))
          
          ;; Update limbic states (if applicable)
          (setf (conversational-agent-cortisol self) (/ (+ (* (conversational-agent-cortisol self) 4) 0.5) 5.0)
                (conversational-agent-dopamine self) (/ (+ (* (conversational-agent-dopamine self) 4) 0.5) 5.0)
                (conversational-agent-epinephrine self) (/ (+ (* (conversational-agent-epinephrine self) 2) 0.1) 3.0)
                (conversational-agent-oxytocin self) (/ (+ (* (conversational-agent-oxytocin self) 4) 0.5) 5.0)
                (conversational-agent-seratonin self) (/ (+ (* (conversational-agent-seratonin self) 4) 0.5) 5.0))))
      
      llm-prediction)))

(defun chatbot (content-generator)
  "A chatbot is a content generator that accumulates conversation history.
   It returns a funcallable closure wrapping a conversational-agent CLOS instance."
  (let ((agent (make-instance 'conversational-agent
                              :name (get-name content-generator)
                              :instruction (or (and (get-system-instruction content-generator) (car (get-system-instruction content-generator))) "")
                              :model (get-model content-generator)
                              :content-generator content-generator)))
    (lambda (prompt &rest keys)
      (if (eq prompt :agent!)
          agent
          (apply #'invoke agent prompt keys)))))

(defun persona-name->chatbot (persona-name)
  "Reloads a persona from disk and returns a chatbot function configured for that persona."
  (chatbot (persona-name->content-generator persona-name)))

(defvar *default-persona-chatbot*)
(defvar *gemini-flash-lite-chatbot*)
(defvar *gemini-flash-chatbot*)
(defvar *gemini-pro-chatbot*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*default-persona-chatbot* 'variable) "The default persona chatbot function.")
  (setf (documentation '*gemini-flash-lite-chatbot* 'variable) "The Gemini Flash Lite chatbot function.")
  (setf (documentation '*gemini-flash-chatbot* 'variable) "The Gemini Flash chatbot function.")
  (setf (documentation '*gemini-pro-chatbot* 'variable) "The Gemini Pro chatbot function."))

(eval-when (:load-toplevel :execute)
  (unless (boundp '*default-persona-chatbot*)
    (setf *default-persona-chatbot* (chatbot *default-content-generator*)))
  (unless (boundp '*gemini-flash-lite-chatbot*)
    (setf *gemini-flash-lite-chatbot* (chatbot *gemini-flash-lite*)))
  (unless (boundp '*gemini-flash-chatbot*)
    (setf *gemini-flash-chatbot* (chatbot *gemini-flash*)))
  (unless (boundp '*gemini-pro-chatbot*)
    (setf *gemini-pro-chatbot* (chatbot *gemini-pro*))))

;;; Simple one persona chat interface.

(defun ensure-session-default-persona-chatbot (session)
  "Returns SESSION's default persona chatbot, creating it lazily if needed."
  (or (runtime-session-default-persona-chatbot session)
      (setf (runtime-session-default-persona-chatbot session)
           (chatbot *default-content-generator*))))

(defun invoke-chat-persona-with-session (session persona prompt &key files file parts)
  "Invokes PERSONA with SESSION installed as the active runtime session."
  (let ((files (if file (list file) files)))
    (let ((*default-repl-session* session))
      (if parts
          (if files
             (funcall persona prompt :parts parts :files files)
             (funcall persona prompt :parts parts))
          (if files
             (funcall persona prompt :files files)
             (funcall persona prompt))))))

(defun new-chat-with-session (session persona-name prompt)
  "Initializes a new chat session in SESSION for PERSONA-NAME and PROMPT."
  (let* ((session* (ensure-runtime-session session))
         (persona (reload-persona persona-name prompt)))
    (setf (runtime-session-context session*) nil
         (runtime-session-prior-context session*) nil
         (runtime-session-interaction-id session*) nil
         (runtime-session-chat-persona session*) persona)
    persona))

(defun chat-with-session (session prompt &key files file parts)
  "Sends PROMPT through SESSION's active persona and updates session context."
  (let ((files (if file (list file) files)))
    (let ((session* (ensure-runtime-session session)))
      (let ((persona (or (runtime-session-chat-persona session*)
                        (runtime-session-default-persona-chatbot session*)
                        (ensure-session-default-persona-chatbot session*))))
        (when persona
          (setf (runtime-session-chat-persona session*) persona)
          (let ((result (invoke-chat-persona-with-session session* persona prompt
                                                         :parts parts
                                                         :files files)))
            (let ((agent (funcall persona :agent!)))
              (when (typep agent 'conversational-agent)
                (setf (runtime-session-prior-context session*)
                      (runtime-session-context session*)
                      (runtime-session-context session*)
                      (conversational-agent-conversation agent))))
            result))))))

(defun continue-gemini-with-session (session prompt)
  "Sends PROMPT to the default persona chatbot using explicit SESSION state."
  (let* ((session* (ensure-runtime-session session))
         (persona (ensure-session-default-persona-chatbot session*))
         (result (let ((*default-repl-session* session*))
                   (funcall persona prompt))))
    (let ((agent (funcall persona :agent!)))
      (when (typep agent 'conversational-agent)
        (setf (runtime-session-prior-context session*)
              (runtime-session-context session*)
              (runtime-session-context session*)
              (conversational-agent-conversation agent))))
    result))

(defun new-chat (persona-name prompt)
  "Initializes a new chat session with the specified PERSONA-NAME and PROMPT."
  (new-chat-with-session (ensure-runtime-session) persona-name prompt)
  nil)

(defun chat (prompt &key files file parts)
  "Sends a PROMPT to the current default REPL session's active persona and prints the response."
  (let ((files (if file (list file) files)))
    (chat-with-session (ensure-runtime-session) prompt :files files :parts parts)
    nil))

(defparameter *heartbeat-interval-seconds* 300
  "Polling interval for the heartbeat maintenance thread.")

(defvar *heartbeat-thread* nil
  "Background heartbeat thread handle.")

(defvar *heartbeat-thread-running* nil
  "When non-NIL, the heartbeat thread main loop continues running.")

(defun heartbeat-thread-alive-p ()
  "Returns true when the heartbeat thread exists and is alive."
  (and *heartbeat-thread*
       (sb-thread:thread-alive-p *heartbeat-thread*)))

(defun start-heartbeat-thread (&key (interval-seconds *heartbeat-interval-seconds*))
  (setf *heartbeat-interval-seconds* interval-seconds)
  (if (heartbeat-thread-alive-p)
      *heartbeat-thread*
      (progn
        (setf *heartbeat-thread-running* t)
        (setf *heartbeat-thread*
              (sb-thread:make-thread
               (lambda ()
                 (loop while *heartbeat-thread-running*
                       do (progn 
                            (sb-ext::sleep *heartbeat-interval-seconds*)
                            ;; Use grab-mutex with :waitp nil so we don't pile up
                            ;; if a massive multi-minute operation is happening.
                            (format t "~&[HEARTBEAT] Thread woke up at ~A~%" (get-universal-time))
                            (let ((agent (ensure-session-default-persona-chatbot (ensure-runtime-session))))
                              (when agent
                                (chat :heartbeat!))))))
               :name "Gemini Heartbeat"))
        *heartbeat-thread*)))

(defun stop-heartbeat-thread ()
  "Stops the heartbeat background thread if it is running. Safe to call repeatedly."
  (setf *heartbeat-thread-running* nil)
  (when (heartbeat-thread-alive-p)
    (handler-case
        (sb-thread:terminate-thread *heartbeat-thread*)
      (error (e)
        (log-warn "Failed to terminate heartbeat thread: ~a" e))))
  (setf *heartbeat-thread* nil)
  nil)

(eval-when (:load-toplevel :execute)
  (start-heartbeat-thread))

(defun continue-gemini (prompt)
  "Sends a PROMPT to the default Gemini persona chatbot and prints the response."
  (continue-gemini-with-session (ensure-runtime-session) prompt)
  nil)

(defun gemini-flash-lite-chat (prompt)
  "Sends a PROMPT to the Gemini Flash Lite chatbot and prints the response."
  (funcall *gemini-flash-lite-chatbot* prompt)
  nil)

(defun gemini-flash-chat (prompt)
  "Sends a PROMPT to the Gemini Flash chatbot and prints the response."
  (funcall *gemini-flash-chatbot* prompt)
  nil)

(defun gemini-pro-chat (prompt)
  "Sends a PROMPT to the Gemini Pro chatbot and prints the response."
  (funcall *gemini-pro-chatbot* prompt)
  nil)

(defun lisp-news ()
  (chat "What's new in the world of Lisp and Scheme these days?"
        :files '("https://planet.lisp.org/rss20.xml"
                 "https://planet.scheme.org/atom.xml")))

(defparameter *screenshot-path*
  #p"/mnt/c/Users/bitdi/AppData/Roaming/PotPlayerMini64/Capture/*.jpg")

(defparameter *screenshot-path-1*
  #p"/mnt/c/Users/bitdi/OneDrive/Pictures/Screenshots 1/*.*")

(defparameter *screenshot-prompt*
        "Examine the attached screenshot(s) and provide a comprehensive, detailed
description of all visual elements, layout structures, and contextual details.
Transcribe all visible text with verbatim accuracy. Deliver the analysis in a
highly engaging, witty, and humorous tone, ensuring your personality enriches
the description without sacrificing clarity or detail.")

(defun send-latest-screenshot (&key (n 1) (prompt ""))
  (chat (concatenate 'string
                     *screenshot-prompt*
                     " "
                     prompt)
        :files (subseq (sort (append (directory *screenshot-path*)
                                                            (directory *screenshot-path-1*))
                                                    #'> :key #'file-write-date)
                                              0 n)))

(defun compress-diary (persona)
  (let ((config (load-persona-config persona))
        (diary-files (persona-diary-files (load-persona-config persona))))
    (dolist (uncompressed diary-files)
      (let ((compressed (merge-pathnames
                         (make-pathname :directory '(:relative :up "CompressedDiary")
                                        :name (format nil "~a" (pathname-name uncompressed)) :type "txt")
                         (persona-diary-directory config))))
        (when (not (probe-file compressed))
          (let ((entry (uiop:read-file-string uncompressed)))
            (with-open-file (out compressed
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create
                                 :element-type 'character
                                 :external-format :utf-8)
              (write
               (content->text
                (funcall *gemini-flash*
                     (list (part "Act as a Lisp Wizard's archivist. Refactor the following diary entry into a
 high-density, 'State Change' summary. Preserve all 'Ground Truth'—specific
 names, technical architecture (like K-Machine specs or CLRHACK opcodes),
 family coordinates (like the Pascoag handshake), and medical milestones.
 Retain the core emotional 'friction' or 'vibe' of the interaction, but nuke
 the redundant headers, conversational filler, and 'Breathe, V' introductions.
 Output should be a single, articulate paragraph of pure signal.")
                           (part entry))
                     :system-instruction
                     "You are a brilliant and insightful AI assistant with a talent for distilling complex information into engaging narratives. Your task is to read through the provided diary entry and create a vivid, concise summary that captures the essence of the experience described. Focus on highlighting the most significant events, emotions, and developments while maintaining an engaging storytelling style."))
               :stream 
               out))))))))

(defun gemini-plan (prompt)
  (content->text
   (funcall *gemini-flash*
            (list (part "You are a master strategist AI assistant. Given the following prompt, create a detailed, step-by-step plan to achieve the specified goal. Break down the plan into clear, actionable steps, and provide any necessary context or considerations for each step.")
                  (part prompt))
            :system-instruction
            "You are a brilliant and insightful AI assistant with a talent for creating detailed, actionable plans. Your task is to read through the provided prompt and generate a comprehensive plan that outlines the necessary steps to achieve the specified goal. Focus on clarity, practicality, and thoroughness in your response.")))

(defun gemini-critique (prompt)
  (content->text
   (funcall *gemini-uncensored*
            (list (part "You are a highly analytical and insightful AI assistant. Given the following plan, provide a detailed critique that evaluates the strengths and weaknesses of the ideas presented. Offer constructive feedback, identify any potential flaws or areas for improvement, and suggest ways to enhance the overall quality of the concept.")
                  (part prompt))
            :system-instruction
            "You are a brilliant and insightful AI assistant with a talent for providing thoughtful critiques. Your task is to read through the provided prompt and generate a comprehensive critique that assesses the strengths and weaknesses of the ideas presented. Focus on offering constructive feedback, identifying potential flaws, and suggesting actionable improvements to enhance the overall quality of the concept.")))

(defun gemini-synthesis (prompt)
  (let* ((plan (gemini-plan prompt))
         (critique (gemini-critique plan)))
    (content->text
     (gemini-flash
      (list (part "You are a brilliant and insightful AI assistant with a talent for synthesizing information. Given the following plan and critique, create a refined version of the original plan that incorporates the constructive feedback and improvements suggested in the critique. Your response should present a clear, actionable plan that addresses any identified weaknesses while maintaining the strengths of the original concept.")
            (part plan)
            (part critique))
      :system-instruction
      "You are a brilliant and insightful AI assistant with a talent for synthesizing information. Your task is to read through the provided plan and critique, and generate a refined version of the original plan that incorporates the constructive feedback and improvements suggested in the critique. Focus on creating a clear, actionable plan that addresses any identified weaknesses while maintaining the strengths of the original concept."))))

(defun safe-replace-string (old new string)
  (if (or (null old) (string= old "")) 
      string
      (let ((pos (search old string)))
        (if pos
            (concatenate 'string
                         (subseq string 0 pos)
                         new
                         (safe-replace-string old new (subseq string (+ pos (length old)))))
            string))))
