;;;; src/package.lisp
(defpackage :lisp-mcp-server
  (:use :cl)
  (:nicknames :mcp)
  (:export
   ;; public API (MVP skeleton)
   #:run
   #:version
   ;; Logging controls
    #:set-log-level-from-env
   ;; REPL interfaces
   #:repl-eval
   ;; Protocol helpers (for tests and custom transports)
   #:process-json-line
   ;; TCP server
   #:serve-tcp))
(in-package :lisp-mcp-server)
