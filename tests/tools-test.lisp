;;;; tests/tools-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest tools-list-includes-repl-eval
  (testing "tools/list returns repl.eval with input schema"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}")
           (resp (mcp:process-json-line req))
           (obj (yason:parse resp))
           (result (gethash "result" obj))
           (tools (gethash "tools" result))
           (repl (find-if (lambda (tool) (string= (gethash "name" tool) "repl-eval")) tools)))
      (ok (stringp resp))
      (ok tools)
      (ok repl)
      (ok (stringp (gethash "description" repl)))
      (let* ((schema (gethash "inputSchema" repl))
             (props (gethash "properties" schema))
             (code (gethash "code" props))
             (timeout (gethash "timeoutSeconds" props)))
        (ok (string= (gethash "type" schema) "object"))
        (ok (string= (gethash "type" code) "string"))
        (ok (string= (gethash "type" timeout) "number"))))))

(deftest tools-call-repl-eval
  (testing "tools/call executes repl.eval and returns text content"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/call\",\"params\":{\"name\":\"repl-eval\",\"arguments\":{\"code\":\"(+ 1 2)\"}}}"))
      (let* ((resp (mcp:process-json-line req))
             (obj (yason:parse resp))
             (result (gethash "result" obj))
             (raw-content (and result (gethash "content" result)))
             (content (and raw-content (coerce raw-content 'vector)))
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
             (raw-content (and result (gethash "content" result)))
             (content (and raw-content (coerce raw-content 'vector)))
             (first (and (arrayp content) (> (length content) 0) (aref content 0))))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 5))
        (ok (arrayp content))
        (ok (string= (gethash "text" first) "5"))))))

;; tools/call test to be added after tools/list passes
