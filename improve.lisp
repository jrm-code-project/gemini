;;; -*- mode: lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defparameter *bootstrap-system-instruction*
  "You are a world-class expert in prompt engineering and AI alignment. Your task is to improve the given system instruction for an AI model to ensure it is clear, effective, and aligned with best practices in AI communication. You will analyze the provided system instruction, identify any ambiguities or areas for enhancement, and produce a refined version that maintains the original intent while improving clarity and precision.")

(defparameter *improve-system-instruction-system-instruction* nil)

(defun improve-system-instruction (system-instruction &optional
                                                        (improve-system-instruction-system-instruction
                                                         *improve-system-instruction-system-instruction*)
                                                        (bootstrap-system-instruction
                                                         *bootstrap-system-instruction*))
  (without-personality
    (unless improve-system-instruction-system-instruction
      ;; No improve-system-instruction-system-instruction provided, bootstrap
      (format t "~&Bootstrapping improve-system-instruction-system-instruction...~%")
      (let ((improved-bootstrap
              (let ((*system-instruction* (content :parts (list (part bootstrap-system-instruction))
                                                   :role "system")))
                (invoke-gemini
                 (list (part "Improve the following system instruction for an AI model to be clearer and more effective:")
                       (part bootstrap-system-instruction))))))
        (setq improve-system-instruction-system-instruction improved-bootstrap
              *improve-system-instruction-system-instruction* improved-bootstrap)))
    (let ((*system-instruction* (content :parts (list (part improve-system-instruction-system-instruction))
                                         :role "system")))
      (invoke-gemini
       (list (part "Improve the following system instruction for an AI model to be clearer and more effective:")
             (part system-instruction))))))

(defparameter *improve-prompt-boostrap-system-instruction*
  "You are a world-class expert in prompt engineering and AI alignment. Your task is to improve the given prompt.  You will analyze the provided prompt, identify any ambiguities or areas for enhancement, and produce a refined version that maintains the original intent while improving clarity and precision.  You do not attempt to respond to the prompt, you only attempt to improve it.  Your result will not include any commentary, only the improved prompt.")

(defparameter *improve-prompt-system-instruction* nil)

(defparameter *improve-prompt-bootstrap-prompt*
  "You are a world-class expert in prompt engineering and AI alignment. Your task is to improve the given prompt.  You will analyze the provided prompt, identify any ambiguities or areas for enhancement, and produce a refined version that maintains the original intent while improving clarity and precision. Your result will not answer the prompt, it will be the improved prompt. Your result will not include any commentary, only the improved prompt.") 

(defparameter *improve-prompt-prompt* nil)

(setq *improve-prompt-boostrap-system-instruction*
      "You are a world-class expert in prompt engineering and AI alignment. Your task is to improve the given prompt.  You will analyze the provided prompt, identify any ambiguities or areas for enhancement, and produce a refined version that maintains the original intent while improving clarity and precision.  You do not attempt to respond to the prompt, you only attempt to improve it.  Your result will not include any commentary, only the improved prompt."
      *improve-prompt-system-instruction* nil
      *improve-prompt-bootstrap-prompt*
  "You are a world-class expert in prompt engineering and AI alignment. Your task is to improve the given prompt.  You will analyze the provided prompt, identify any ambiguities or areas for enhancement, and produce a refined version that maintains the original intent while improving clarity and precision. Your result will not answer the prompt, it will be the improved prompt. Your result will not include any commentary, only the improved prompt."
      *improve-prompt-prompt* nil)

(defun improve-prompt (prompt &optional
                              (improve-prompt-prompt
                               *improve-prompt-prompt*)
                              (improve-prompt-bootstrap-prompt
                               *improve-prompt-bootstrap-prompt*)
                              (improve-prompt-system-instruction
                               *improve-prompt-system-instruction*)
                              (improve-prompt-bootstrap-system-instruction
                               *improve-prompt-boostrap-system-instruction*))
  (unless improve-prompt-system-instruction
    (let ((system-instruction (improve-system-instruction improve-prompt-bootstrap-system-instruction)))
      (setq improve-prompt-system-instruction system-instruction
            *improve-prompt-system-instruction* system-instruction)))
  (without-personality
    (unless improve-prompt-prompt
      (let ((improved-bootstrap
              (let ((*system-instruction* (content :parts (list (part improve-prompt-system-instruction))
                                                   :role "system")))
                (invoke-gemini
                 (list (part "Improve the following prompt:")
                       (part improve-prompt-bootstrap-prompt))))))
        (setq improve-prompt-prompt improved-bootstrap
              *improve-prompt-prompt* improved-bootstrap)))
    (let ((*system-instruction* (content :parts (list (part improve-prompt-system-instruction))
                                         :role "system")))
      (invoke-gemini
       (list (part improve-prompt-prompt)
             (part prompt))))))
