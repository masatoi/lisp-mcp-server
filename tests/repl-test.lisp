;;;; tests/repl-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest repl-eval-simple
  (testing "(+ 1 2) returns 3 as a string"
    (multiple-value-bind (printed value)
        (repl-eval "(+ 1 2)")
      (ok (string= printed "3"))
      (ok (= value 3)))))

(deftest repl-eval-multiple-forms
  (testing "evaluates forms sequentially and returns last value"
    (let ((input "(defparameter *x* 10) (incf *x* 2) (* *x* 2)"))
      (multiple-value-bind (printed value)
          (repl-eval input)
        (ok (string= printed "24"))
        (ok (= value 24))))))

(deftest repl-eval-read-eval-enabled
  (testing "#.(...) is evaluated at read time"
    (multiple-value-bind (printed value)
        (repl-eval "#.(+ 1 2)")
      (ok (string= printed "3"))
      (ok (= value 3)))))
