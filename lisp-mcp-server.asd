;;;; lisp-mcp-server.asd

(asdf:defsystem "lisp-mcp-server"
  :description "Model Context Protocol server for Common Lisp (MVP skeleton)"
  :author ""
  :license "MIT"
  :version "0.1.0"
  :depends-on (
    :alexandria
    :yason
    )
  :serial t
  :components (
    (:module "src"
     :components (
       (:file "package")
       (:file "core")
       (:file "run")))
    ))

(asdf:defsystem "lisp-mcp-server/tests"
  :description "Tests for lisp-mcp-server"
  :author ""
  :license "MIT"
  :depends-on ("lisp-mcp-server" :rove)
  :serial t
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "core-test"))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :rove :run c)))

