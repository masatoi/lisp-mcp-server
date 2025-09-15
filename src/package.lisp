;;;; src/package.lisp
(defpackage :lisp-mcp-server
  (:use :cl)
  (:nicknames :mcp)
  (:export
   ;; public API (MVP skeleton)
   #:run
   #:version
   ;; REPL interfaces
   #:repl-eval))
(in-package :lisp-mcp-server)
