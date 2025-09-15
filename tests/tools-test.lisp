;;;; tests/tools-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest tools-list-includes-repl-eval
  (testing "tools/list returns repl.eval with input schema"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"tools/list\",\"params\":{}}")
           (resp (mcp:process-json-line req)))
      (ok (stringp resp))
      (let* ((obj (yason:parse resp))
             (result (gethash "result" obj))
             (tools (gethash "tools" result)))
        (ok tools)
        (let ((found nil))
          (map nil (lambda (t)
                     (when (string= (gethash "name" t) "repl.eval")
                       (setf found t)))
               tools)
          (ok found)
          (ok (stringp (gethash "description" found)))
          (let* ((schema (gethash "inputSchema" found))
                 (props (and schema (gethash "properties" schema)))
                 (code (and props (gethash "code" props))))
            (ok (string= (gethash "type" schema) "object"))
            (ok (string= (gethash "type" code) "string")))))))

;; tools/call test to be added after tools/list passes
