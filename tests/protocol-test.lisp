;;;; tests/protocol-test.lisp
(in-package :lisp-mcp-server/tests)

(defparameter *init-req*
  "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.1\"}}}")

(deftest initialize-handshake
  (testing "process-json-line responds with serverInfo and capabilities"
    (let ((resp (mcp:process-json-line *init-req*)))
      (ok (stringp resp))
      (let ((obj (yason:parse resp)))
        (ok (string= (gethash "jsonrpc" obj) "2.0"))
        (ok (eql (gethash "id" obj) 1))
        (ok (gethash "result" obj))
        (let* ((result (gethash "result" obj))
               (server (gethash "serverInfo" result))
               (caps (gethash "capabilities" result)))
          (ok (stringp (gethash "name" server)))
          (ok (stringp (gethash "version" server)))
          (ok (gethash "tools" caps)))))))

(deftest initialized-notification
  (testing "notifications/initialized returns no response"
    (let* ((line "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\",\"params\":{\"protocolVersion\":\"2025-06-18\"}}"))
      (ok (null (mcp:process-json-line line))))))
