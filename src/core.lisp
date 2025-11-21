;;;; src/core.lisp
(in-package :lisp-mcp-server)

(defparameter +server-version+
  "0.1.0"
  "Semantic version of lisp-mcp-server.")

(declaim (ftype (function () simple-string) version))
(defun version ()
  "Return server version string."
  +server-version+)
