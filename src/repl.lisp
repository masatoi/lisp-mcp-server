;;;; src/repl.lisp
(in-package :lisp-mcp-server)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(declaim (inline %read-all))
(defun %read-all (string)
  "Read all top-level forms from STRING and return them as a list."
  (let ((*readtable* (copy-readtable nil)))
    (with-input-from-string (in string)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(declaim (ftype (function (string &key (:package (or package symbol string))
                                         (:print-level (or null (integer 0)))
                                         (:print-length (or null (integer 0))))
                        (values string t &optional))
                repl-eval))
(defun repl-eval (input &key (package *default-eval-package*)
                              (print-level nil) (print-length nil))
  "Evaluate INPUT (a string of one or more s-expressions) in PACKAGE.

Forms are read as provided and evaluated sequentially; the last value is
returned as a printed string per `prin1-to-string`. The second return value is
the raw last value for callers that want it."
  (let* ((pkg (etypecase package
                (package package)
                (symbol (find-package package))
                (string (find-package package))))
         (*package* pkg)
         (forms (%read-all input))
         (last-value nil))
    (block eval-forms
      (handler-bind ((error (lambda (e)
                              (setf last-value
                                    (with-output-to-string (out)
                                      (format out "~A~%" e)
                                      (uiop:print-backtrace :stream out :condition e)))
                              (return-from eval-forms))))
        (dolist (form forms)
          (setf last-value (eval form)))))
    (let ((*print-level* print-level)
          (*print-length* print-length))
      (values (prin1-to-string last-value) last-value))))
