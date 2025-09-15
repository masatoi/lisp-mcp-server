;;;; tests/protocol-test.lisp
(in-package :lisp-mcp-server/tests)

(defparameter *init-req*
  (with-output-to-string (s)
    (write-string
     (yason:with-output-to-string* ()
       (yason:encode
        '((jsonrpc . "2.0")
          (id . 1)
          (method . "initialize")
          (params . ((clientInfo . ((name . "test-client") (version . "0.1")))))))))
     s)))

(deftest initialize-handshake
  (testing "process-json-line responds with serverInfo and capabilities"
    (let ((resp (mcp:process-json-line *init-req*)))
      (ok (stringp resp))
      (let ((obj (yason:parse resp)))
        (ok (string= (getf obj :jsonrpc) "2.0"))
        (ok (eql (getf obj :id) 1))
        (ok (getf obj :result))
        (let* ((result (getf obj :result))
               (server (getf result :serverInfo))
               (caps (getf result :capabilities)))
          (ok (stringp (getf server :name)))
          (ok (stringp (getf server :version)))
          (ok (getf caps :tools)))))))

(deftest initialized-notification
  (testing "notifications/initialized returns no response"
    (let* ((line (yason:with-output-to-string* ()
                   (yason:encode '((jsonrpc . "2.0")
                                   (method . "notifications/initialized")
                                   (params . ((protocolVersion . "2025-06-18"))))))))
      (ok (null (mcp:process-json-line line))))))

