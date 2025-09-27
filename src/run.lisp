;;;; src/run.lisp
(in-package :lisp-mcp-server)

(defun %stdio-server-loop (in out)
  (let ((state (make-state)))
    (log-event :info "stdio.start")
    (unwind-protect
         (loop for line = (read-line in nil :eof)
               until (eq line :eof)
               do (let ((resp (process-json-line line state)))
                    (when resp
                      (write-line resp out)
                      (force-output out))))
      (log-event :info "stdio.stop"))
    t))

(defun %tcp-server-loop (host port accept-once on-listening)
  (log-event :info "tcp.start" "host" host "port" port)
  (unwind-protect
       (serve-tcp :host host :port port
                  :accept-once accept-once
                  :on-listening on-listening)
    (log-event :info "tcp.stop" "host" host "port" port)))

(declaim (ftype (function (&key (:transport (member :stdio :tcp))
                                (:in stream) (:out stream)
                                (:host string) (:port (or integer null))
                                (:accept-once t) (:on-listening function))
                          (values boolean &optional))
                run))
(defun run (&key (transport :stdio) (in *standard-input*) (out *standard-output*)
                 (host "127.0.0.1") (port 0) (accept-once t) on-listening)
  "Start the MCP server loop. For :stdio, reads newline-delimited JSON from IN
and writes responses to OUT. Returns T when input is exhausted (EOF).

This loop now spins up dedicated worker threads for both the transport
processing and evaluation to avoid blocking the main starter thread."
  (ensure-eval-manager)
  (ecase transport
    (:stdio
     (let ((thread (bordeaux-threads:make-thread
                    (lambda () (%stdio-server-loop in out))
                    :name "mcp-stdio-loop")))
       (bordeaux-threads:join-thread thread)))
    (:tcp
     (let ((thread (bordeaux-threads:make-thread
                    (lambda () (%tcp-server-loop host port accept-once on-listening))
                    :name "mcp-tcp-loop")))
       (bordeaux-threads:join-thread thread)))))
