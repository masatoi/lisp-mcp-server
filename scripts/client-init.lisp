#!/usr/bin/env sbcl --script
;; Simple TCP client to send MCP initialize and print the response line.

(require :asdf)
(ignore-errors (ql:quickload :usocket))

(defun arg (name &optional default)
  (let* ((argv (or (ignore-errors sb-ext:*posix-argv*) (list)))
         (pos (position name argv :test #'string=)))
    (or (and pos (let ((idx (1+ pos))) (when (< idx (length argv)) (nth idx argv))))
        default)))

(let* ((host (or (arg "--host") (or (uiop:getenv "MCP_HOST") "127.0.0.1")))
       (port (parse-integer (or (arg "--port") (or (uiop:getenv "MCP_PORT") "12345"))))
       (sock nil)
       (stream nil))
  (handler-case
      (progn
        (setf sock (usocket:socket-connect host port :element-type 'character))
        (setf stream (usocket:socket-stream sock))
        (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
        (finish-output stream)
        (let ((line (read-line stream nil nil)))
          (when line (format t "~A~%" line)))
        (uiop:quit 0))
    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (uiop:quit 2)))

