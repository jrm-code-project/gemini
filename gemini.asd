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
               "cl-ppcre"
               "dexador"
               "fold"
               "function"
               "google"
               "jsonx"
               "named-let"
               "promise"
               "series"
               "str"
               "trivial-backtrace"
               "uiop")
  :components ((:file "asdfx"     :depends-on ("package"))
               (:file "blogger"   :depends-on ("gemini" "package" "vars"))
               (:file "debug"     :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "functions" :depends-on ("asdfx" "mcp" "misc" "package" "object"))
               (:file "gemini"    :depends-on ("functions" "mcp" "misc" "object" "package" "vars"))
               (:file "jsonrpc"   :depends-on ("misc" "object" "package"))
               (:file "llm-repl"  :depends-on ("functions" "gemini" "object" "package" "vars"))
               (:file "mcp"       :depends-on ("jsonrpc" "misc" "object" "package" "vars"))
               (:file "misc"      :depends-on ("package"))
               (:file "object"    :depends-on ("misc" "package" "vars"))
               (:file "package")
               (:file "vars"      :depends-on ("package"))))
