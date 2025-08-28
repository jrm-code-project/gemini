;;;; gemini.asd

(defsystem "gemini"
  :description "API to Google's Gemini LLM"
  :author "Joe Marshall"
  :license "MIT"
  :depends-on ("alexandria"
               "asdf"
               "chanl"
               "cl-base64"
               "cl-json"
               "dexador"
               "fold"
               "function"
               "named-let"
               "promise"
               "series"
               "str"
               "trivial-backtrace"
               "uiop")
  :components ((:file "apikey"    :depends-on ("package"))
               (:file "asdfx"     :depends-on ("package"))
               (:file "blogger"   :depends-on ("gemini" "json" "package" "vars"))
               (:file "debug"     :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "functions" :depends-on ("asdfx" "misc" "mcp" "package" "object"))
               (:file "gemini"    :depends-on ("apikey" "functions" "json" "mcp" "misc" "object" "package" "vars"))
               (:file "json"      :depends-on ("package"))
               (:file "jsonrpc"   :depends-on ("json" "misc" "object" "package"))
               (:file "llm-repl"  :depends-on ("functions" "gemini" "object" "package" "vars"))
               (:file "mcp"       :depends-on ("jsonrpc" "misc" "object" "package" "vars"))
               (:file "misc"      :depends-on ("json" "package"))
               (:file "object"    :depends-on ("misc" "package" "vars"))
               (:file "package")
               (:file "vars"      :depends-on ("package"))))
