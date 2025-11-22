;;;; tests/package.lisp
(defpackage :lisp-mcp-server/tests
  (:use :cl :rove)
  (:import-from :lisp-mcp-server
                #:version
                #:repl-eval
                #:code-find-definition
                #:code-describe-symbol))
(in-package :lisp-mcp-server/tests)
