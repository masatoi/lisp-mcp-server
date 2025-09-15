;;;; src/package.lisp
(defpackage :lisp-mcp-server
  (:use :cl)
  (:nicknames :mcp)
  (:export
   ;; public API (MVP skeleton)
   #:run
   #:version
   ;; REPL interfaces
   #:repl-eval
   ;; Protocol helpers (for tests and custom transports)
   #:process-json-line))
(in-package :lisp-mcp-server)
