;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(defun mock-successful-interaction-response (&key (id "mock_id")
                                                  (environment-id nil)
                                                  (text "ok"))
  (let ((response (gemini::object
                   :id id
                   :steps (vector
                           (gemini::object
                            :type "model_output"
                            :index 0
                            :content (gemini::object
                                      :parts (vector (gemini::object :text text))))))))
    (when environment-id
      (setf (gethash :environment-id response) environment-id)
      (setf (gethash :environmentId response) environment-id))
    response))
