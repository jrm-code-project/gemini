;;;; gemini.asd

(defsystem "gemini"
  :description "API to Google's Gemini LLM"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria"
               "asdf"
               "cl-json"
               "dexador"
               "fold"
               "function"
               "named-let"
               "trivial-backtrace"
               "uiop")
  :components ((:file "apikey"    :depends-on ("package"))
               (:file "asdfx"     :depends-on ("package"))
               (:file "debug"     :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "functions" :depends-on ("asdfx" "misc" "package" "object"))
               (:file "gemini"    :depends-on ("apikey" "functions" "misc" "object" "package" "vars"))
               (:file "llm-repl"  :depends-on ("functions" "gemini" "object" "package" "vars"))
               (:file "misc"      :depends-on ("package"))
               (:file "object"    :depends-on ("misc" "package" "vars"))
               (:file "package")
               (:file "vars"      :depends-on ("package"))))
