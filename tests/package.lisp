;;;; tests/package.lisp
(defpackage :lisp-mcp-server/tests
  (:use :cl :rove)
  (:import-from :lisp-mcp-server
                #:version
                #:repl-eval
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory
                #:code-find-definition
                #:code-describe-symbol))
(in-package :lisp-mcp-server/tests)
