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

(deftest repl-eval-timeout
  (testing "long-running forms trigger evaluation-timeout"
    (let ((mcp::*default-eval-timeout* 0.1))
      (let ((caught nil)
            (start (get-internal-real-time))
            (units internal-time-units-per-second))
        (handler-case
            (repl-eval "(progn (sleep 1) :done)")
          (mcp:evaluation-timeout (c)
            (setf caught c)))
        (ok caught)
        (ok (typep caught 'mcp:evaluation-timeout))
        (ok (>= (mcp:evaluation-timeout-seconds caught) 0.1))
        (let ((elapsed (/ (- (get-internal-real-time) start) units)))
          (ok (< elapsed 1.0)))))))

(deftest repl-eval-captures-output
  (testing "captures stdout and stderr text"
    (multiple-value-bind (printed value stdout stderr)
        (repl-eval "(progn (format t \"hello~%\") (format *error-output* \"warn~%\") :done)")
      (ok (string= printed ":DONE"))
      (ok (eq value :done))
      (ok (search "hello" stdout))
      (ok (search "warn" stderr)))))
