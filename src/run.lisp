;;;; src/run.lisp
(in-package :lisp-mcp-server)

;; MVP placeholder: provide a minimal RUN entry point signature only.
;; Real transport/protocol handling will be implemented TDD-first later.

(declaim (ftype (function (&key (:transport (member :stdio :tcp))) (values boolean &optional)) run))
(defun run (&key (transport :stdio))
  "Start the MCP server (skeleton). Returns T when the skeleton is ready.

TRANSPORT is reserved for future use. Only validates the option for now."
  (declare (ignorable transport))
  t)

