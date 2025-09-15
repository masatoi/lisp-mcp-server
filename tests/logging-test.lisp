;;;; tests/logging-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest logging-rpc-dispatch
  (testing "process-json-line emits debug logs when level=debug"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
           (sink (make-string-output-stream)))
      (let ((mcp::*log-level* :debug)
            (mcp::*log-stream* sink))
        (ok (stringp (mcp:process-json-line req)))
        (let* ((s (get-output-stream-string sink)))
          (ok (> (length s) 0))
          (ok (search "\"event\":\"rpc.dispatch\"" s))
          (ok (search "\"event\":\"rpc.result\"" s)))))))

