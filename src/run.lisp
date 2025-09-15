;;;; src/run.lisp
(in-package :lisp-mcp-server)

;; MVP placeholder: provide a minimal RUN entry point signature only.
;; Real transport/protocol handling will be implemented TDD-first later.

(declaim (ftype (function (&key (:transport (member :stdio :tcp)) (:in stream) (:out stream))
                          (values boolean &optional))
                run))
(defun run (&key (transport :stdio) (in *standard-input*) (out *standard-output*))
  "Start the MCP server loop. For :stdio, reads newline-delimited JSON from IN
and writes responses to OUT. Returns T when input is exhausted (EOF).

This is a minimal loop for E2E bring-up; full transport features come later."
  (ecase transport
    (:stdio
     (let ((state (make-state)))
       (loop for line = (read-line in nil :eof)
             until (eq line :eof)
             do (let ((resp (process-json-line line state)))
                  (when resp
                    (write-line resp out)
                    (force-output out))))
       t))
    (:tcp
     ;; not implemented yet
     t)))
