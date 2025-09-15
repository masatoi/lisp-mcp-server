;;;; src/package.lisp
(defpackage :lisp-mcp-server
  (:use :cl)
  (:nicknames :mcp)
  (:export
   ;; public API (MVP skeleton)
   #:run
   #:version))
(in-package :lisp-mcp-server)

