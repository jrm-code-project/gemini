;;;; gemini-tests.asd

(defsystem "gemini-tests"
  :description "Tests for the gemini system."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :license "MIT"
  :depends-on ("fiveam" "gemini")
  :components ((:module "tests"
                :components
                ((:file "main"    :depends-on ("package"))
                 (:file "package"))))
  :perform (test-op (op c) (symbol-call :gemini-tests '#:run!)))
