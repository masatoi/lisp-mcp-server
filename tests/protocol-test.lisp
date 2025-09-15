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

(deftest initialize-echo-version
  (testing "initialize echoes client protocolVersion when supported"
    (let* ((line "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2024-11-05\"}}")
           (resp (mcp:process-json-line line))
           (obj (yason:parse resp))
           (result (gethash "result" obj)))
      (ok (string= (gethash "protocolVersion" result) "2024-11-05")))))

(deftest ping-returns-empty
  (testing "ping returns empty result object"
    (let* ((resp (mcp:process-json-line "{\"jsonrpc\":\"2.0\",\"id\":42,\"method\":\"ping\"}"))
           (obj (yason:parse resp))
           (result (gethash "result" obj)))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0)))))
