;;;; tests/code-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest code-find-definition-returns-path-and-line
  (testing "code.find-definition returns relative path and positive line"
    (multiple-value-bind (path line)
        (code-find-definition "lisp-mcp-server:version")
      (ok (string= path "src/core.lisp"))
      (ok (integerp line))
      (ok (> line 0)))))

(deftest code-describe-symbol-returns-doc
  (testing "code.describe-symbol returns type, arglist, and documentation"
    (multiple-value-bind (name type arglist doc)
        (code-describe-symbol "lisp-mcp-server:version")
      (ok (stringp name))
      (ok (string= type "function"))
      (ok (stringp arglist))
      (ok (stringp doc)))))
