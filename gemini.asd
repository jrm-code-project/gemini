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
               "sb-introspect"
               "series"
               "str"
               "trivial-backtrace"
               "trivial-timeout"
               "uiop")
  :components
  ((:module "infrastructure"
    :components
    ((:file "package")
     (:file "vars"      :depends-on ("package"))
     (:file "misc"      :depends-on ("package"))
     (:file "macros"    :depends-on ("package"))
     (:file "predator"  :depends-on ("package"))
     (:file "object"    :depends-on ("misc" "package" "vars"))))

   (:module "transport"
    :depends-on ("infrastructure")
    :components
    ((:file "asdfx")
     (:file "jsonrpc")
     (:file "mcp"       :depends-on ("jsonrpc"))))

   (:module "adapters"
    :depends-on ("infrastructure")
    :components
    ((:file "adapter")
     (:file "gemini-print")
     (:file "gemini-openai" :depends-on ("adapter"))))

   (:module "tools"
    :depends-on ("infrastructure" "transport")
    :components
    ((:file "git-tools")
     (:file "shell-tools")
     (:file "web-tools")
     (:file "filesystem-tools")
     (:file "interaction-tools")
     (:file "lisp-introspection-tools")
     (:file "evolution-tools")
     (:file "misc-tools")
     (:file "functions" :depends-on ("git-tools"
                                     "shell-tools"
                                     "web-tools"
                                     "filesystem-tools"
                                     "interaction-tools"
                                     "lisp-introspection-tools"
                                     "evolution-tools"
                                     "misc-tools"))))

   (:module "orchestration"
    :depends-on ("infrastructure" "transport" "adapters" "tools")
    :components
    ((:file "gemini-core")
     (:file "lmstudio-tool-bridge" :depends-on ("gemini-core"))
     (:file "interaction-session" :depends-on ("gemini-core"))
     (:file "interaction" :depends-on ("gemini-core" "lmstudio-tool-bridge" "interaction-session"))
     (:file "interaction-events" :depends-on ("interaction" "interaction-session"))
     (:file "interaction-payloads" :depends-on ("interaction" "lmstudio-tool-bridge"))
     (:file "interaction-transport" :depends-on ("interaction" "interaction-session" "interaction-events" "interaction-payloads"))
     (:file "gemini-personas")
     (:file "gemini-chatbot" :depends-on ("gemini-personas"))
     (:file "gemini-iridium")
     (:file "uroboros"       :depends-on ("gemini-core"))
     (:file "gemini"        :depends-on ("gemini-core"
                                         "gemini-personas"
                                         "gemini-chatbot"
                                         "gemini-iridium"
                                         "uroboros"
                                         "interaction"))))

   (:module "apps"
    :depends-on ("orchestration" "tools")
    :components
    ((:file "analyze")
     (:file "blogger")
     (:file "debug")
     (:file "echo")
     (:file "improve")
     (:file "llm-repl")
     (:file "meta")
     (:file "parse")
     (:file "specimen")
     (:file "system")
     (:file "config"    :depends-on ("analyze"
                                     "blogger"
                                     "debug"
                                     "echo"
                                     "improve"
                                     "llm-repl"
                                     "meta"
                                     "parse"
                                     "specimen"
                                     "system"))))))
