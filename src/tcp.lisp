;;;; src/tcp.lisp

(defpackage #:lisp-mcp-server/src/tcp
  (:use #:cl)
  (:import-from #:lisp-mcp-server/src/log #:log-event)
  (:import-from #:lisp-mcp-server/src/protocol #:make-state #:process-json-line)
  (:import-from #:bordeaux-threads #:thread-alive-p #:make-thread #:destroy-thread)
  (:import-from #:usocket)
  (:export
   #:*tcp-server-thread*
   #:*tcp-server-port*
   #:tcp-server-running-p
   #:start-tcp-server-thread
   #:ensure-tcp-server-thread
   #:stop-tcp-server-thread
   #:serve-tcp))

(in-package #:lisp-mcp-server/src/tcp)

(defparameter *tcp-server-thread* nil
  "Background TCP server thread created by START-TCP-SERVER-THREAD.")

(defparameter *tcp-server-port* nil
  "Port number of the currently running background TCP server.")

(defun tcp-server-running-p ()
  "Return T when a background TCP server thread is alive."
  (and *tcp-server-thread*
       (bordeaux-threads:thread-alive-p *tcp-server-thread*)))

(declaim (ftype (function (&key (:host string) (:port (or integer null))
                                   (:accept-once t) (:on-listening (or null function)))
                          (values t (or integer null) &optional))
                start-tcp-server-thread))
(defun start-tcp-server-thread (&key (host "127.0.0.1") (port 0)
                                     (accept-once nil) on-listening)
  "Start the TCP MCP server on a dedicated thread.
Returns the thread object and the bound PORT once the listener is up."
  (when (and *tcp-server-thread*
             (not (bordeaux-threads:thread-alive-p *tcp-server-thread*)))
    (setf *tcp-server-thread* nil
          *tcp-server-port* nil))
  (when (tcp-server-running-p)
    (log-event :info "tcp.thread.already-running" "port" *tcp-server-port*)
    (return-from start-tcp-server-thread (values *tcp-server-thread* *tcp-server-port*)))
  (let ((actual-port nil))
    (log-event :info "tcp.thread.start" "host" host "port" port "accept-once" accept-once)
    (setf *tcp-server-thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (serve-tcp :host host
                        :port port
                        :accept-once accept-once
                        :on-listening (lambda (p)
                                        (setf actual-port p)
                                        (setf *tcp-server-port* p)
                                        (when on-listening (funcall on-listening p))))
             (log-event :info "tcp.thread.exit"))
           :name "mcp-tcp-server"))
    (loop repeat 200
          until (or actual-port (not (tcp-server-running-p)))
          do (sleep 0.01))
    (values *tcp-server-thread* *tcp-server-port*)))

(declaim (ftype (function (&key (:host string) (:port (or integer null))
                                   (:accept-once t) (:on-listening (or null function)))
                          (values (member :already-running :started) &optional))
                ensure-tcp-server-thread))
(defun ensure-tcp-server-thread (&key (host "127.0.0.1") (port 0)
                                      (accept-once nil) on-listening)
  "Ensure a background TCP server thread is running.
Starts one if needed and returns :started; otherwise returns :already-running."
  (if (tcp-server-running-p)
      (progn
        (when (and on-listening *tcp-server-port*)
          (funcall on-listening *tcp-server-port*))
        :already-running)
      (progn
        (multiple-value-bind (thr started-port)
            (start-tcp-server-thread :host host :port port
                                     :accept-once accept-once
                                     :on-listening on-listening)
          (declare (ignore thr))
          (when started-port :started)))))

(declaim (ftype (function () (values (member nil :stopped) &optional))
                stop-tcp-server-thread))
(defun stop-tcp-server-thread ()
  "Stop the background TCP server thread if it is running."
  (when (tcp-server-running-p)
    (log-event :info "tcp.thread.stop" "port" *tcp-server-port*)
    (bordeaux-threads:destroy-thread *tcp-server-thread*)
    (setf *tcp-server-thread* nil
          *tcp-server-port* nil)
    :stopped))

(defun %process-stream (stream)
  (let ((state (make-state)))
    (loop for line = (read-line stream nil :eof)
          until (eq line :eof)
          do (progn
               (log-event :debug "tcp.read" "line" line)
               (let ((resp (process-json-line line state)))
                 (log-event :debug "tcp.response" "resp" resp "resp-nil" (null resp))
                 (when resp
                   (log-event :debug "tcp.write" "resp" resp)
                   (write-line resp stream)
                   (finish-output stream)
                   (log-event :debug "tcp.flushed")))))))

(defun serve-tcp (&key (host "127.0.0.1") (port 0) (accept-once t) on-listening)
  "Serve MCP over TCP. If PORT is 0, an ephemeral port is chosen.
Calls ON-LISTENING with the actual port when ready. If ACCEPT-ONCE is T,
accepts a single connection and returns T after the client closes."
  (let ((listener nil))
    (unwind-protect
         (progn
           (setf listener (usocket:socket-listen host port :reuse-address t
                                                :element-type 'character))
           (let ((actual (usocket:get-local-port listener)))
             (when on-listening (funcall on-listening actual)))
           (labels ((handle-one ()
                      (let ((client nil)
                            (stream nil))
                        (unwind-protect
                             (progn
                               (setf client (usocket:socket-accept listener))
                               (log-event :info "tcp.accept" "remote" (ignore-errors (usocket:get-peer-address client)))
                               (setf stream (usocket:socket-stream client))
                               (%process-stream stream)
                               t)
                          (when stream (ignore-errors (close stream)))
                          (when client (ignore-errors (usocket:socket-close client)))))))
             (if accept-once
                 (handle-one)
                 (loop do (handle-one)))))
      (when listener (ignore-errors (usocket:socket-close listener))))))
