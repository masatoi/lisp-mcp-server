;;;; tests/core-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest version-available
  (testing "version returns a non-empty string"
    (ok (stringp (version)))
    (ok (> (length (version)) 0))))

(deftest run-skeleton
  (testing "run returns T for skeleton"
    (ok (eq t (run :transport :stdio)))))

