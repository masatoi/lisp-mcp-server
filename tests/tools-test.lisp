;;;; tests/tools-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest tools-list-includes-repl-eval
  (testing "tools/list returns repl.eval with input schema"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}")
           (resp (mcp:process-json-line req))
           (obj (yason:parse resp))
           (result (gethash "result" obj))
           (tools (gethash "tools" result))
           (repl (find-if (lambda (tool) (string= (gethash "name" tool) "repl-eval")) tools))
           (code-find (find-if (lambda (tool) (string= (gethash "name" tool) "code.find")) tools))
           (code-describe (find-if (lambda (tool) (string= (gethash "name" tool) "code.describe")) tools)))
      (ok (stringp resp))
      (ok tools)
      (ok repl)
      (ok code-find)
      (ok code-describe)
      (ok (stringp (gethash "description" repl)))
      (let* ((schema (gethash "inputSchema" repl))
             (code (gethash "code" (gethash "properties" schema))))
        (ok (string= (gethash "type" schema) "object"))
        (ok (string= (gethash "type" code) "string"))))))

(deftest tools-call-code-find
  (testing "tools/call code.find returns path and line"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"code.find\",\"arguments\":{\"symbol\":\"lisp-mcp-server:version\"}}}"))
      (let* ((resp (mcp:process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "path" result) "src/core.lisp"))
        (ok (integerp (gethash "line" result)))))))

(deftest tools-call-code-describe
  (testing "tools/call code.describe returns symbol metadata"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"code.describe\",\"arguments\":{\"symbol\":\"lisp-mcp-server:version\"}}}"))
      (let* ((resp (mcp:process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (string= (gethash "type" result) "function"))
        (ok (stringp (gethash "arglist" result)))
        (ok (stringp (gethash "documentation" result)))))))

(deftest tools-call-repl-eval
  (testing "tools/call executes repl.eval and returns text content"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":\"(+ 1 2)\"}}}"))
      (let* ((resp (mcp:process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 2))
        (ok (arrayp content))
        (ok (string= (gethash "type" first) "text"))
        (ok (string= (gethash "text" first) "3"))))))

(deftest tools-call-namespaced-name
  (testing "namespaced tool name like lisp_mcp.repl-eval is accepted"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"tools/call\",\"params\":{\"name\":\"lisp_mcp.repl-eval\",\"arguments\":{\"code\":\"(+ 2 3)\"}}}"))
      (let* ((resp (mcp:process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (content (and result (gethash "content" result)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 5))
        (ok (arrayp content))
        (ok (string= (gethash "type" first) "text"))
        (ok (string= (gethash "text" first) "5"))))))

;; tools/call test to be added after tools/list passes
