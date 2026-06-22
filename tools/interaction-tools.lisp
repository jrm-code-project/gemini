;;; -*- Lisp -*-

(in-package "GEMINI")

(defun interaction-tools-and-handlers ()
  "Return a list of interaction-related functions and their handlers."
  (list
   (cons
    (function-declaration
     :name "promptingRead"
     :description "Prompts the user for input and returns the response.  Do not hesitate to use this function to ask questions of the user or to get input from the user."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :user-prompt
                                             (schema :type :string
                                                     :description "The prompt to send to the user.  This should be a complete sentence or question."))
                         :required (vector :user-prompt))
     :response (schema :type :string
                       :description "The user's input response."))
    (lambda (&key user-prompt)
      (prompting-read user-prompt)))

   (cons
    (function-declaration
     :name "promptLLM"
     :description "Prompts the LLM with a string and returns the response.  Use this to ask questions of the LLM or to get input from the LLM."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :llm-prompt
                                             (schema :type :string
                                                     :description "The prompt to send to the LLM.  This should be a complete sentence or question."))
                         :required (vector :llm-prompt))
     :response (schema :type :string
                       :description "The LLM's response to the prompt."))
    (lambda (&key llm-prompt)
      (let ((*enable-recursive-prompt* nil))
        (continue-gemini llm-prompt))))

   (cons
    (function-declaration
     :name "yesOrNoP"
     :description "Asks a careful yes/no question and returns the response.  Use this for consequential questions that require a definitive yes or no answer."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :question
                                             (schema :type :string
                                                     :description "The question to ask the user."))
                         :required (vector :question))
     :response (schema :type :boolean
                       :description "Returns true or false based on user input."))
    (lambda (&key question)
      (if (yes-or-no-p question)
          jsonx:+json-true+
          jsonx:+json-false+)))

   (cons
    (function-declaration
     :name "yOrNP"
     :description "Asks a y/n question and returns the response.  Use this for simple yes/no questions that do not require a careful answer."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :question
                                             (schema :type :string
                                                     :description "The question to ask the user."))
                         :required (vector :question))
     :response (schema :type :boolean
                       :description "Returns true or false based on user input."))
    (lambda (&key question)
      (if (y-or-n-p question)
          jsonx:+json-true+
          jsonx:+json-false+)))
   ))
