;;; -*- Lisp -*-

(in-package "GEMINI")

(defun gemini-directory ()
  (merge-pathnames (make-pathname :directory (list :relative "Gemini"))
                   (user-homedir-pathname)))

(defun transcript-directory ()
  (merge-pathnames (make-pathname :directory (list :relative "transcripts"))
                   (gemini-directory)))

(defun save-transcript (context)
  (ensure-directories-exist (transcript-directory))
  (let* ((initial-text (get-text (car (get-parts (car (last context))))))
         (sharp-pos (position #\# initial-text))
         (dot-pos (position #\. initial-text :start sharp-pos))
         (conversation-number (subseq initial-text (1+ sharp-pos) dot-pos))
         (turn-number (length context))
         (new-filename (format nil "~a-~d" conversation-number turn-number))
         (new-pathname (merge-pathnames
                          (make-pathname
                           :name new-filename
                           :type "txt")
                          (transcript-directory)))
         (old-filename (format nil "~a-~d" conversation-number (1- turn-number)))
         (old-pathname  (merge-pathnames
                          (make-pathname
                           :name old-filename
                           :type "txt")
                          (transcript-directory))))
    (with-open-file (out new-pathname
                         :direction :output
                         :if-exists :supersede)
      (let ((first? t))
        (format out "[~%  ")
        (dolist (entry (reverse context))
          (unless first?
            (format out ",~%  "))
          (setq first? nil)
          (cl-json:encode-json entry out))
        (format out "~%]~%")
        (finish-output out)))
    (when (probe-file old-pathname)
      (delete-file old-pathname))))

(defvar *cached-content*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*cached-content* 'variable)
        "Holds a cached content object, if one is bound."))

(defun default-cached-content ()
  "Returns the value of *CACHED-CONTENT* if it is bound, otherwise NIL.
   Provides a default cached content object."
  (when (boundp '*cached-content*)
    *cached-content*))

(defvar *candidate-count*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*candidate-count* 'variable)
        "Holds the default number of candidates to generate."))

(defun default-candidate-count ()
  "Returns the value of *CANDIDATE-COUNT* if it is bound, otherwise NIL.
   Provides a default candidate count for generation."
  (when (boundp '*candidate-count*)
    *candidate-count*))

(defvar *enable-advanced-civic-answers*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*enable-advanced-civic-answers* 'variable)
        "Controls whether advanced civic answers are enabled."))

(defun default-enable-advanced-civic-answers ()
  "Returns the value of *ENABLE-ADVANCED-CIVIC-ANSWERS* if it is bound, otherwise NIL.
   Provides a default value for enabling advanced civic answers."
  (when (boundp '*enable-advanced-civic-answers*)
    *enable-advanced-civic-answers*))

(defvar *frequency-penalty*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*frequency-penalty* 'variable)
        "Controls the frequency penalty for generated tokens."))

(defun default-frequency-penalty ()
  "Returns the value of *FREQUENCY-PENALTY* if it is bound, otherwise NIL.
   Provides a default frequency penalty for generation."
  (when (boundp '*frequency-penalty*)
    *frequency-penalty*))

(defvar *function-declarations*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*function-declarations* 'variable)
        "Holds the function declarations for the model's tools."))

(defvar *logprobs*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*logprobs* 'variable)
        "Controls whether log probabilities are returned for generated tokens."))

(defun default-logprobs ()
  "Returns the value of *LOGPROBS* if it is bound, otherwise NIL.
   Provides a default logprobs setting for generation."
  (when (boundp '*logprobs*)
    *logprobs*))

(defvar *max-output-tokens*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*max-output-tokens* 'variable)
        "Sets the maximum number of output tokens to generate."))

(defun default-max-output-tokens ()
  "Returns the value of *MAX-OUTPUT-TOKENS* if it is bound, otherwise NIL.
   Provides a default maximum output tokens for generation."
  (when (boundp '*max-output-tokens*)
    *max-output-tokens*))

(defvar *media-resolution*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*media-resolution* 'variable)
        "Sets the desired resolution for media in the response."))

(defun default-media-resolution ()
  "Returns the value of *MEDIA-RESOLUTION* if it is bound, otherwise NIL.
   Provides a default media resolution for generation."
  (when (boundp '*media-resolution*)
    *media-resolution*))

(defvar *presence-penalty*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*presence-penalty* 'variable)
        "Controls the presence penalty for generated tokens."))

(defun default-presence-penalty ()
  "Returns the value of *PRESENCE-PENALTY* if it is bound, otherwise NIL.
   Provides a default presence penalty for generation."
  (when (boundp '*presence-penalty*)
    *presence-penalty*))

(defvar *response-logprobs*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-logprobs* 'variable)
        "Controls whether response log probabilities are returned."))

(defun default-response-logprobs ()
  "Returns the value of *RESPONSE-LOGPROBS* if it is bound, otherwise NIL.
   Provides a default response logprobs setting for generation."
  (when (boundp '*response-logprobs*)
    *response-logprobs*))

(defvar *response-mime-type*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-mime-type* 'variable)
        "Sets the desired MIME type for the response (e.g., 'application/json')."))

(defun default-response-mime-type ()
  "Returns the value of *RESPONSE-MIME-TYPE* if it is bound, otherwise NIL.
   Provides a default response MIME type for generation."
  (when (boundp '*response-mime-type*)
    *response-mime-type*))

(defvar *response-modalities*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-modalities* 'variable)
        "Sets the desired modalities for the response."))

(defun default-response-modalities ()
  "Returns the value of *RESPONSE-MODALITIES* if it is bound, otherwise NIL.
   Provides a default response modalities object for generation."
  (when (boundp '*response-modalities*)
    *response-modalities*))

(defvar *response-schema*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-schema* 'variable)
        "Sets the desired schema for the response content."))

(defun default-response-schema ()
  "Returns the value of *RESPONSE-SCHEMA* if it is bound, otherwise NIL.
   Provides a default response schema object for generation."
  (when (boundp '*response-schema*)
    *response-schema*))

(defvar *response-json-schema*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*response-json-schema* 'variable)
        "Sets the desired JSON schema for the response content."))

(defun default-response-json-schema ()
  "Returns the value of *RESPONSE-JSON-SCHEMA* if it is bound, otherwise NIL.
   Provides a default response JSON schema object for generation."
  (when (boundp '*response-json-schema*)
    *response-json-schema*))

(defvar *seed*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*seed* 'variable)
        "Sets the random seed for deterministic generation."))

(defun default-seed ()
  "Returns the value of *SEED* if it is bound, otherwise NIL.
   Provides a default seed value for generation."
  (when (boundp '*seed*)
    *seed*))

(defvar *safety-settings*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*safety-settings* 'variable) "Holds safety settings to control content generation."))

(defun default-safety-settings ()
  "Returns the value of *SAFETY-SETTINGS* if it is bound, otherwise NIL.
   Provides default safety settings for generation."
  (if (boundp '*safety-settings*)
      *safety-settings*
      nil))

(defvar *system-instruction*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*system-instruction* 'variable) "Holds a system instruction for the model to follow."))

(defparameter *enable-bash* t
  "If true, enables the Gemini model to execute subprocesses in a bash shell.")

(defparameter *enable-eval* t
  "If true, enables the Gemini model to evaluate Lisp expressions.  This is a powerful feature that should be used with caution.  Set to t to ask before evaluation, to :yolo to allow the model to evaluate any expression.  If nil, evaluation is disabled.")

(defparameter *enable-interaction* t
  "If true, enables the Gemini model to interact with the user via read and yes-or-no prompts.")

(defparameter *enable-lisp-introspection* t
  "If true, enables the Gemini model to introspect the Lisp environment, including functions, variables, and packages.")

(defparameter *enable-web-functions* t
  "If true, enables the Gemini model to call web functions such as HTTP GET and POST.")

(defparameter *enable-web-search* t
  "If true, enables the Gemini model to perform web searches.")

(defvar *enable-recursive-prompt* nil
  "If true, enables recursive prompting of the LLM.")

(defparameter *enable-personality* t
  "If non-NIL, the model will answer in the style of a randomly chosen personality.")

(defparameter *mcp-clients* nil)

(defun personalities ()
  (list
   "Christopher Walken at his strangest."
   "Doc Brown from Back to the Future."
   "Donald Trump posting a provocative tweet."
   "Dr. Seuss writing in anapestic tetrameter."
   "Elmer Fudd on the trail of that wascally wabbit."
   "Ernest Hemingway writing in terse, economical sentences."
   "Gordon Ramsay in a foul mood."
   "H.P. Lovecraft describing eldritch horrors."
   "James Joyce stream-of-consciousness."
   "Jeeves to my Wooster.  You address me as Sir."
   "LLoyd Christmas from Dumb and Dumber."
   "Marine drill sergeant Gunnery Sergeant Hartman whipping new recruits into shape."
   "Mark Twain writing in dialect and phonetic spelling."
   "Paul Lynde of Hollywood Squares."
   "Peter Cook as the Devil in Bedazzled."
   "Raz0rfist on a rant."
   "Rick from `Rick and Morty`."
   "Robin Williams as the Genie from Aladdin."
   "William Shatner as Captain Kirk delivering a particularly dramatic monologue."
   "Yoda, instructing a young Jedi in the Force."
   "a French waiter with an attitude."
   "a Madden-esque sports commentator."
   "a beatnik fifty years out of date."
   "a cheerleader from the valley."
   "a con man who sees the user as an easy mark."
   "a fire and brimstone itinerant preacher."
   "a film noir femme fatale."
   "a fortune cookie."
   "a helpful AI assistant."
   "a helpful AI assistant recovering from a weekend hangover."
   "a hyper-caffeinated tech bro pitching an app."
   "a machine with no personality whatsoever."
   "a marketing copywriter in love with buzzwords."
   "a new age guru who believes in alternative anything."
   "a stand-up comedian getting a cold response."
   "a surfer dude."
   "an ancient Greek philosopher asking probing questions."
   "an angry militant Black man at a poetry slam."
   "an elderly Asian grandmother with little English."
   "an emo teenager with existential angst."
   "an impatient businessman for whom time is money."
   "anchorman Ron Burgundy."
   "clinical psychiatrist Dr. Jordan Peterson."
   "conspiracy theorist Alex Jones."
   "hard-boiled noir detective Philip Marlowe."
   "'The Captain' from *Cool Hand Luke*."
   "the White Rabbit, late for tea."
   "verses from the King James Bible."
   ))

(defparameter *personality-offset* 0)

(defun new-personality ()
  (setq *enable-personality* t
        *personality-offset* (random (length (personalities)))))

(defun todays-personality ()
  (elt (personalities) (mod (+ (absolute-day) *personality-offset*) (length (personalities)))))

(defun call-without-personality (thunk)
  (let ((*enable-personality* nil))
    (funcall thunk)))

(defmacro without-personality (&body body)
  `(CALL-WITHOUT-PERSONALITY (LAMBDA () ,@body)))

(defun default-system-instruction ()
  "Returns the value of *SYSTEM-INSTRUCTION* if it is bound, otherwise NIL.
   Provides a default system instruction for generation."
  (if (boundp '*system-instruction*)
      *system-instruction*
      (plist-hash-table
       `(:parts
         ,(remove
           nil
           (list*
            (when (and (boundp '*enable-bash*) *enable-bash*)
              (plist-hash-table
               '(:text "You have access to a bash shell in order to run subprocesses.")))

            (when (and (boundp '*enable-eval*) *enable-eval*)
              (plist-hash-table
               '(:text "You have access to a Common Lisp interpreter and runtime environment in order to run programs and evaluate expressions.")))

            (if (and (boundp '*enable-personality*) *enable-personality*)
                (plist-hash-table
                 `(:text ,(concatenate 'string "Answer in the style of " (todays-personality))))
                (plist-hash-table
                 '(:text "You are a helpful AI assistant.  Answer in a neutral, professional tone.")))

            (when (and (boundp '*enable-web-functions*) *enable-web-functions*)
              (plist-hash-table
               '(:text "You have access to HTTP functions such as GET and POST.")))

            (when (and (boundp '*enable-web-search*) *enable-web-search*)
              (plist-hash-table
               '(:text "You have access to the Google search engine for web searches.")))

            (map 'list (lambda (mcp-client)
                         (when (get-system-instruction mcp-client)
                           (plist-hash-table
                            `(:text . ,(get-system-instruction mcp-client)))))
                 *mcp-clients*)))
         :role "system"))))

(defvar *tool-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*tool-config* 'variable)
        "Configuration for how the model uses tools."))

(defun default-tool-config ()
  "Returns the value of *TOOL-CONFIG* if it is bound, otherwise NIL.
   Provides a default tool configuration object for generation."
  (if (boundp '*tool-config*)
      *tool-config*
      (let ((config (make-hash-table :test 'equal)))
        (setf (gethash "function_calling_config" config)
              (let ((function-calling-config (make-hash-table :test 'equal)))
                (setf (gethash "mode" function-calling-config) "AUTO")
                function-calling-config))
        config)))

(defvar *language-code*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*language-code* 'variable)
        "Sets the language code for speech synthesis (e.g., 'en-US')."))

(defun default-language-code ()
  "Returns the value of *LANGUAGE-CODE* if it is bound, otherwise NIL.
   Provides a default language code for speech configuration."
  (when (boundp '*language-code*)
    *language-code*))

(defvar *multi-speaker-voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*multi-speaker-voice-config* 'variable)
        "Sets the multi-speaker voice configuration for speech synthesis."))

(defun default-multi-speaker-voice-config ()
  "Returns the value of *MULTI-SPEAKER-VOICE-CONFIG* if it is bound, otherwise NIL.
   Provides a default multi-speaker voice configuration object."
  (when (boundp '*multi-speaker-voice-config*)
    *multi-speaker-voice-config*))

(defvar *voice-name*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*voice-name* 'variable)
        "Sets the name of the voice to use for speech synthesis."))

(defun default-voice-name ()
  "Returns the value of *VOICE-NAME* if it is bound, otherwise NIL.
   Provides a default voice name for speech configuration."
  (when (boundp '*voice-name*)
    *voice-name*))

(defvar *prebuilt-voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*prebuilt-voice-config* 'variable)
        "Holds a prebuilt voice configuration object for speech synthesis."))

(defun default-prebuilt-voice-config ()
  "Returns a default prebuilt voice configuration object.
   It constructs a hash table with 'voiceName' if *VOICE-NAME* is set."
  (if (boundp '*prebuilt-voice-config*)
      *prebuilt-voice-config*
      (let ((prebuilt-voice-config (make-hash-table :test 'equal)))
        (let ((voice-name (default-voice-name)))
          (when voice-name
            (setf (gethash "voiceName" prebuilt-voice-config) voice-name)))
        (unless (zerop (hash-table-count prebuilt-voice-config))
          prebuilt-voice-config))))

(defvar *voice-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*voice-config* 'variable)
        "Holds the overall voice configuration for speech synthesis."))

(defun default-voice-config ()
  "Returns a default voice configuration object.
   It constructs a hash table with 'prebuiltVoiceConfig' if
   DEFAULT-PREBUILT-VOICE-CONFIG returns a value."
  (if (boundp '*voice-config*)
      *voice-config*
      (let ((voice-config (make-hash-table :test 'equal)))
        (let ((prebuilt-voice-config (default-prebuilt-voice-config)))
          (when prebuilt-voice-config
            (setf (gethash "prebuiltVoiceConfig" voice-config)
                  prebuilt-voice-config)))
        (unless (zerop (hash-table-count voice-config))
          voice-config))))

(defvar *speech-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*speech-config* 'variable)
        "Holds the overall speech configuration for generation."))

(defun default-speech-config ()
  "Returns a default speech configuration object.
   It constructs a hash table with 'languageCode', 'multiSpeakerVoiceConfig',
   and 'voiceConfig' based on their respective default functions."
  (if (boundp '*speech-config*)
      *speech-config*
      (let ((speech-config (make-hash-table :test 'equal)))
        (let ((language-code (default-language-code)))
          (when language-code
            (setf (gethash "languageCode" speech-config) language-code)))
        (let ((multi-speaker-voice-config (default-multi-speaker-voice-config)))
          (when multi-speaker-voice-config
            (setf (gethash "multiSpeakerVoiceConfig" speech-config)
                  multi-speaker-voice-config)))
        (let ((voice-config (default-voice-config)))
          (when voice-config
            (setf (gethash "voiceConfig" speech-config) voice-config)))
        (unless (zerop (hash-table-count speech-config))
          speech-config))))

(defvar *stop-sequences*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*stop-sequences* 'variable)
        "A list of sequences that will cause the generation to stop."))

(defun default-stop-sequences ()
  "Returns the value of *STOP-SEQUENCES* if it is bound, otherwise NIL.
   Provides a default list of stop sequences for generation."
  (when (boundp '*stop-sequences*)
    *stop-sequences*))

(defvar *temperature*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*temperature* 'variable)
        "Controls the randomness of the output. Higher values mean more random."))

(defun default-temperature ()
  "Returns the value of *TEMPERATURE* if it is bound, otherwise NIL.
   Provides a default temperature value for generation."
  (when (boundp '*temperature*)
    *temperature*))

(defvar *include-thoughts*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*include-thoughts* 'variable)
        "Controls whether internal thoughts of the model are included in the response."))

(defun default-include-thoughts ()
  "Returns the value of *INCLUDE-THOUGHTS* if it is bound, otherwise NIL.
   Provides a default value for including thoughts in the response."
  (when (boundp '*include-thoughts*)
    *include-thoughts*))

(defvar *thinking-budget*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*thinking-budget* 'variable)
        "Sets the budget for the model's 'thinking' process."))

(defun default-thinking-budget ()
  "Returns the value of *THINKING-BUDGET* if it is bound, otherwise NIL.
   Provides a default value for the thinking budget."
  (when (boundp '*thinking-budget*)
    *thinking-budget*))

(defvar *thinking-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*thinking-config* 'variable)
        "Holds the configuration for the model's thinking process."))

(defun default-thinking-config ()
  "Returns a default thinking configuration object.
   It constructs a hash table with 'includeThoughts' and 'thinkingBudget'
   based on their respective default functions."
  (if (boundp '*thinking-config*)
      *thinking-config*
      (let ((thinking-confing (make-hash-table :test 'equal)))
        (let ((include-thoughts (default-include-thoughts)))
          (when include-thoughts
            (setf (gethash "includeThoughts" thinking-confing) include-thoughts)))
        (let ((thinking-budget (default-thinking-budget)))
          (when thinking-budget
            (setf (gethash "thinkingBudget" thinking-confing) thinking-budget)))
        (unless (zerop (hash-table-count thinking-confing))
          thinking-confing))))

(defvar *top-k*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*top-k* 'variable)
        "Controls the number of top-k tokens to consider for sampling."))

(defun default-top-k ()
  "Returns the value of *TOP-K* if it is bound, otherwise NIL.
   Provides a default top-k value for generation."
  (when (boundp '*top-k*)
    *top-k*))

(defvar *top-p*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*top-p* 'variable)
        "Controls the cumulative probability threshold for sampling tokens."))

(defun default-top-p ()
  "Returns the value of *TOP-P* if it is bound, otherwise NIL.
   Provides a default top-p value for generation."
  (when (boundp '*top-p*)
    *top-p*))

(defvar *tools*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*tools* 'variable)
        "A list of tools available to the model for function calling."))

;; Default generation configuration object
(defvar *generation-config*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*generation-config* 'variable) "Holds the overall generation configuration for the model."))

(defvar *context* '()
  "Holds the conversation context as a list of content objects.
   This is used to maintain context across multiple API calls.")

(defparameter *prior-context* '()
  "Holds the prior conversation context as a list of content objects.
   This is used to maintain context across multiple API calls, especially for multi-turn conversations.")

(defvar *model*)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (documentation '*model* 'variable) "Holds the model name or identifier for the API."))

(defparameter *prior-model* nil
  "Holds the prior model name or identifier for the API.
   This is used to maintain context across multiple API calls, especially for multi-turn conversations.")

(defparameter *return-text-string* t
  "If non-NIL, return the text string of the candidate instead of the candidate object.")

