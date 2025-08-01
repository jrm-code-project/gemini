;;;; gemini.asd

(defsystem "gemini"
  :description "API to Google's Gemini LLM"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria" "cl-json" "dexador" "fold" "function" "named-let" "uiop")
  :components ((:file "apikey" :depends-on ("package"))
               (:file "gemini" :depends-on ("misc" "object" "package" "vars"))
               (:file "misc"   :depends-on ("package"))
               (:file "object" :depends-on ("misc" "package" "vars"))
               (:file "package")
               (:file "vars"   :depends-on ("package"))
               ))
