;;;; tests/core-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest version-available
  (testing "version returns a non-empty string"
    (ok (stringp (version)))
    (ok (> (length (version)) 0))))

(deftest run-skeleton
  (testing "run returns T for skeleton"
    (ok (eq t (mcp:run :transport :stdio)))))

(deftest stdio-one-message
  (testing "run processes one line from :in and writes to :out"
    (let* ((req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n")
           (in (make-string-input-stream req))
           (out (make-string-output-stream)))
      (ok (eq t (mcp:run :transport :stdio :in in :out out)))
      (let* ((s (get-output-stream-string out))
             (lines (cl-ppcre:split "\n" s :limit 0))
             (first (first lines)))
        (ok (> (length s) 0))
        (ok (search "\"result\"" first))))))
