;;; -*- Lisp -*-

(in-package "GEMINI")

(defun generate-lora-input ()
  (with-open-file (stream (merge-pathnames "lora-output.json")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (do-symbols (sym (find-package "CL"))
      (terpri stream)
      (cond ((fboundp sym)
             (dotimes (i 10)
               (write-string (content->text
                       (gemini-flash-lite (format nil "Write a question that does not mention ~a, but would elicit an ideomatic use of the symbol ~a in a respnose." sym sym)
                                 :read-timeout 100
                                 :connect-timeout 10
                                 :system-instruction (format nil "The response should be a question that would likely cause a human to use the symbol ~a in their answer, without mentioning the symbol itself. The question should be clear and concise, but may be unusual or absurd." sym)))
                             :stream stream)
               (terpri stream)
               (finish-output stream))
               (terpri stream))))))
