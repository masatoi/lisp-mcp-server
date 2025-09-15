;;;; tests/package.lisp
(defpackage :lisp-mcp-server/tests
  (:use :cl :rove)
  (:import-from :lisp-mcp-server #:version #:repl-eval))
(in-package :lisp-mcp-server/tests)
