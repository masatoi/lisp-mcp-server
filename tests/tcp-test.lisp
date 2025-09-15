;;;; tests/tcp-test.lisp
(in-package :lisp-mcp-server/tests)

(deftest tcp-serve-initialize
  (testing "serve-tcp accepts a connection and responds to initialize"
    (let ((port-var nil))
      (let ((thr (bordeaux-threads:make-thread
                  (lambda ()
                    (mcp:serve-tcp :host "127.0.0.1" :port 0
                                   :on-listening (lambda (p) (setf port-var p))
                                   :accept-once t))
                  :name "tcp-test-server")))
        (loop repeat 200 until port-var do (sleep 0.01))
        (ok port-var)
        (let* ((sock (usocket:socket-connect "127.0.0.1" port-var
                                             :element-type 'character))
               (stream (usocket:socket-stream sock)))
          (unwind-protect
               (progn
                 (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
                 (finish-output stream)
                 (let ((line (read-line stream)))
                   (ok (search "\"result\"" line))))
            (ignore-errors (close stream))
            (ignore-errors (usocket:socket-close sock))))
        (bordeaux-threads:join-thread thr)))))

