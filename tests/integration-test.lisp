;;;; tests/integration-test.lisp

(defpackage #:lisp-mcp-server/tests/integration-test
  (:use #:cl #:rove)
  (:import-from #:lisp-mcp-server/src/protocol #:process-json-line)
  (:import-from #:yason #:parse))

(in-package #:lisp-mcp-server/tests/integration-test)

(deftest repl-eval-debug-flow
  (testing "agent fixes wrong arity using code-find and fs-read-file"
    ;; 1) Intentionally call version with a wrong argument to get an error.
    (let* ((req1 "{\"jsonrpc\":\"2.0\",\"id\":100,\"method\":\"tools/call\",\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":\"(lisp-mcp-server:version :extra)\"}}}")
           (resp1 (process-json-line req1))
           (obj1 (parse resp1))
           (result1 (gethash "result" obj1))
           (content1 (and result1 (gethash "content" result1)))
           (first1 (and (arrayp content1) (> (length content1) 0) (aref content1 0)))
           (text1 (and first1 (gethash "text" first1))))
      (ok (string= (gethash "jsonrpc" obj1) "2.0"))
      ;; repl-eval returns the error message as text content, not as JSON-RPC error.
      (ok (stringp text1))
      (ok (search "argument" text1)))
    ;; 2) Locate the definition of version.
    (let* ((req2 "{\"jsonrpc\":\"2.0\",\"id\":101,\"method\":\"tools/call\",\"params\":{\"name\":\"code-find\",\"arguments\":{\"symbol\":\"lisp-mcp-server:version\"}}}")
           (resp2 (process-json-line req2))
           (obj2 (parse resp2))
           (result2 (gethash "result" obj2)))
      (ok (string= (gethash "jsonrpc" obj2) "2.0"))
      (ok (string= (gethash "path" result2) "src/core.lisp"))
      (ok (integerp (gethash "line" result2))))
    ;; 3) Read the file and confirm the definition has no arguments.
    (let* ((req3 "{\"jsonrpc\":\"2.0\",\"id\":102,\"method\":\"tools/call\",\"params\":{\"name\":\"fs-read-file\",\"arguments\":{\"path\":\"src/core.lisp\",\"limit\":400}}}")
           (resp3 (process-json-line req3))
           (obj3 (parse resp3))
           (result3 (gethash "result" obj3))
           (content3 (gethash "content" result3))
           (first3 (and (arrayp content3) (> (length content3) 0) (aref content3 0)))
           (text3 (and first3 (gethash "text" first3))))
      (ok (string= (gethash "jsonrpc" obj3) "2.0"))
      (ok (stringp text3))
      (ok (search "(defun version" text3)))
    ;; 4) Call version correctly (no arguments) and succeed.
    (let* ((req4 "{\"jsonrpc\":\"2.0\",\"id\":103,\"method\":\"tools/call\",\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":\"(lisp-mcp-server:version)\"}}}")
           (resp4 (process-json-line req4))
           (obj4 (parse resp4))
           (result4 (gethash "result" obj4))
           (content4 (and result4 (gethash "content" result4)))
           (first4 (and (arrayp content4) (> (length content4) 0) (aref content4 0)))
           (text4 (and first4 (gethash "text" first4))))
      (ok (string= (gethash "jsonrpc" obj4) "2.0"))
      (ok (stringp text4))
      (ok (search "0.1.0" text4)))))
