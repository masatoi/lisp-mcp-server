;;;; tests/tcp-test.lisp

(defpackage #:lisp-mcp-server/tests/tcp-test
  (:use #:cl #:rove)
  (:import-from #:lisp-mcp-server/src/tcp
                #:serve-tcp
                #:start-tcp-server-thread
                #:ensure-tcp-server-thread
                #:stop-tcp-server-thread
                #:tcp-server-running-p)
  (:import-from #:bordeaux-threads #:make-thread #:join-thread)
  (:import-from #:usocket))

(in-package #:lisp-mcp-server/tests/tcp-test)

(deftest tcp-serve-initialize
  (testing "serve-tcp accepts a connection and responds to initialize"
    (let ((port-var nil))
      (let ((thr (make-thread
                  (lambda ()
                    (serve-tcp :host "127.0.0.1" :port 0
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
                 ;; Half-close our write side so the server sees EOF and exits
                 ;; the read loop even if the client crashes before closing.
                 (ignore-errors (usocket:socket-shutdown sock :output))
                 (let ((line (read-line stream)))
                   (ok (search "\"result\"" line))))
            (ignore-errors (close stream))
            (ignore-errors (usocket:socket-close sock))))
        (join-thread thr)))))

#+(or)
(deftest tcp-thread-helper-lifecycle
  (testing "start/ensure/stop manage tcp server thread"
    (unwind-protect
         (progn
           (multiple-value-bind (thr port)
               (start-tcp-server-thread :host "127.0.0.1" :port 0 :accept-once nil)
             (ok thr)
             (ok (tcp-server-running-p))
             (ok (integerp port))
             (let* ((sock (usocket:socket-connect "127.0.0.1" port
                                                  :element-type 'character))
                    (stream (usocket:socket-stream sock)))
               (unwind-protect
                    (progn
                      (write-string "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}\n" stream)
                      (finish-output stream)
                      ;; Signal EOF to server read loop after sending request.
                      (ignore-errors (usocket:socket-shutdown sock :output))
                      (let ((line (read-line stream)))
                        (ok (search "\"result\"" line))))
                 (ignore-errors (close stream))
                 (ignore-errors (usocket:socket-close sock))))
             (ok (eq :already-running
                     (ensure-tcp-server-thread :host "127.0.0.1" :port port
                                               :accept-once nil)))))
      (stop-tcp-server-thread))
    (ok (not (tcp-server-running-p)))))
