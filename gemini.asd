;;;; gemini.asd

(defsystem "gemini"
  :description "API to Google's Gemini LLM"
  :author "Joe Marshall <eval.apply@gmail.com>"
  :license "MIT"
  :defsystem-depends-on ("fiveam")
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
               "trivial-timeout"
               "uiop")
  :components ((:file "analyze"   :depends-on ("gemini" "object" "package" "vars"))
               (:file "asdfx"     :depends-on ("package"))
               (:file "blogger"   :depends-on ("gemini" "object" "package" "vars"))
               ;; loads last
               (:file "config"    :depends-on ("analyze"
                                               "asdfx"
                                               "blogger"
                                               "debug"
                                               "evolution-tools"
                                               "filesystem-tools"
                                               "functions"
                                               "gemini"
                                               "git-tools"
                                               "interaction-tools"
                                               "lisp-introspection-tools"
                                               "misc-tools"
                                               "shell-tools"
                                               "web-tools"
                                               "improve"
                                               "jsonrpc"
                                               "llm-repl"
                                               "mcp"
                                               "misc"
                                               "object"
                                               "package"
                                               "system"
                                               "vars"))
               (:file "debug"     :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "echo"      :depends-on ("gemini" "object" "package" "vars"))
               (:file "evolution-tools" :depends-on ("package" "vars"))
               (:file "filesystem-tools" :depends-on ("package" "vars"))
               (:file "functions" :depends-on ("asdfx"
                                               "evolution-tools"
                                               "filesystem-tools"
                                               "git-tools"
                                               "interaction-tools"
                                               "lisp-introspection-tools"
                                               "mcp"
                                               "misc"
                                               "misc-tools"
                                               "object"
                                               "package"
                                               "shell-tools"
                                               "vars"
                                               "web-tools"))
               (:file "gemini"    :depends-on ("functions" "macros" "mcp" "misc" "object" "package" "vars"))
               (:file "git-tools" :depends-on ("package" "vars"))
               (:file "improve"   :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "interaction-tools" :depends-on ("package" "vars"))
               (:file "jsonrpc"   :depends-on ("misc" "object" "package"))
               (:file "lisp-introspection-tools" :depends-on ("package" "vars"))
               (:file "llm-repl"  :depends-on ("functions" "gemini" "object" "package" "vars"))
               (:file "macros"    :depends-on ("package"))
               (:file "mcp"       :depends-on ("jsonrpc" "misc" "object" "package" "vars"))
               (:file "meta"      :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "misc"      :depends-on ("package"))
               (:file "misc-tools" :depends-on ("package" "vars"))
               (:file "object"    :depends-on ("misc" "package" "vars"))
               (:file "package")
               (:file "parse"     :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "shell-tools" :depends-on ("package" "vars"))
               (:file "specimen"  :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "system"    :depends-on ("gemini" "misc" "object" "package" "vars"))
               (:file "vars"      :depends-on ("package"))
               (:file "web-tools" :depends-on ("package" "vars"))))
