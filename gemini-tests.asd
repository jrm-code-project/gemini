;;;; gemini-tests.asd

(defsystem "gemini-tests"
  :description "Tests for the gemini system."
  :author "Joe Marshall <eval.apply@gmail.com>"
  :license "MIT"
  :depends-on ("fiveam" "gemini")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main" :depends-on ("package"))
                 (:file "suites" :depends-on ("main"))
                 (:file "misc" :depends-on ("suites"))
                 (:file "concurrency" :depends-on ("suites"))
                 (:file "predator" :depends-on ("suites"))
                 (:file "gemini-print" :depends-on ("suites"))
                 (:file "gemini-core" :depends-on ("suites"))
                 (:file "chatbot" :depends-on ("suites"))
                 (:file "interaction-support" :depends-on ("suites"))
                 (:file "interaction-payloads" :depends-on ("suites"))
                 (:file "interaction-backend" :depends-on ("interaction-support"))
                 (:file "interaction-live" :depends-on ("suites"))
                 (:file "lmstudio-backend" :depends-on ("suites"))
                 (:file "iridium" :depends-on ("suites"))
                 (:file "interaction-streams" :depends-on ("suites"))
                 (:file "sse" :depends-on ("suites")))))
  :perform (test-op (op c) (symbol-call :gemini-tests '#:run!)))
