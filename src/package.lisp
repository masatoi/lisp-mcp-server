;;;; src/package.lisp
(defpackage :lisp-mcp-server
  (:use :cl)
  (:nicknames :mcp)
  (:export
   ;; public API (MVP skeleton)
   #:run
   #:version
   ;; Code intelligence
   #:code-find-definition
   #:code-describe-symbol
   ;; Logging controls
    #:set-log-level-from-env
   ;; REPL interfaces
   #:repl-eval
   ;; Protocol helpers (for tests and custom transports)
   #:process-json-line
   ;; TCP server
   #:serve-tcp
   #:*tcp-server-thread*
   #:*tcp-server-port*
   #:tcp-server-running-p
   #:start-tcp-server-thread
   #:ensure-tcp-server-thread
   #:stop-tcp-server-thread))
(in-package :lisp-mcp-server)
