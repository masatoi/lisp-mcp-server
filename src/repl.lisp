;;;; src/repl.lisp

(defpackage #:lisp-mcp-server/src/repl
  (:use #:cl)
  (:import-from #:uiop #:print-backtrace)
  (:export #:repl-eval #:*default-eval-package*))

(in-package #:lisp-mcp-server/src/repl)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(declaim (inline %read-all))
(defun %read-all (string)
  "Read all top-level forms from STRING and return them as a list."
  (let ((*readtable* (copy-readtable nil))
        (*read-eval* t))
    (with-input-from-string (in string)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(declaim (ftype (function (string &key (:package (or package symbol string))
                                  (:print-level (or null (integer 0)))
                                  (:print-length (or null (integer 0))))
                          (values string t string string &optional))
                repl-eval))
(defun repl-eval (input &key (package *default-eval-package*)
                             (print-level nil) (print-length nil))
  "Evaluate INPUT (a string of one or more s-expressions) in PACKAGE.

Forms are read as provided and evaluated sequentially; the last value is
returned as a printed string per `prin1-to-string`. The second return value is
the raw last value for callers that want it. The third and fourth values capture
stdout and stderr produced during evaluation."
  (let ((last-value nil)
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream)))
    (handler-bind ((error (lambda (e)
                            (setf last-value
                                  (with-output-to-string (out)
                                    (format out "~A~%" e)
                                    (uiop:print-backtrace :stream out :condition e)))
                            (return-from repl-eval
                              (values last-value
                                      last-value
                                      (get-output-stream-string stdout)
                                      (progn (write-string "" stderr)
                                             (get-output-stream-string stderr)))))))
      ;; Resolve package (may signal error)
      (let* ((pkg (etypecase package
                    (package package)
                    (symbol (find-package package))
                    (string (find-package package)))))
        (unless pkg
          (error "Package ~S does not exist" package))
        (let ((*package* pkg)
              (*read-eval* t)
              (*print-readably* t))
          ;; Read forms (may signal error)
          (let ((forms (%read-all input)))
            ;; Evaluate forms (may signal error)
            (let ((*standard-output* stdout)
                  (*error-output* stderr))
              (dolist (form forms)
                (setf last-value (eval form)))))))
      ;; Normal completion: format result
      (let ((*print-level* print-level)
            (*print-length* print-length))
        (values (prin1-to-string last-value)
                last-value
                (get-output-stream-string stdout)
                (get-output-stream-string stderr))))))
