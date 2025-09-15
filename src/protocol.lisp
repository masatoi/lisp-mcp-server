;;;; src/protocol.lisp
(in-package :lisp-mcp-server)

(defparameter +protocol-version+ "2025-06-18")

(defclass server-state ()
  ((initialized-p :initform nil :accessor initialized-p)
   (client-info  :initform nil :accessor client-info)))

(defun make-state () (make-instance 'server-state))

(defun %decode-json (line)
  (yason:parse line))

(defun %encode-json (obj)
  (yason:with-output-to-string* ()
    (yason:encode obj)))

(defun %make-ht (&rest kvs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on kvs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun %result (id payload)
  (%make-ht "jsonrpc" "2.0" "id" id "result" payload))

(defun %error (id code message &optional data)
  (let* ((err (%make-ht "code" code "message" message))
         (obj (%make-ht "jsonrpc" "2.0" "id" id "error" err)))
    (when data (setf (gethash "data" err) data))
    obj))

(defun handle-initialize (state id params)
  (declare (ignore params))
  (setf (initialized-p state) t)
  (%result id
           (%make-ht
            "protocolVersion" +protocol-version+
            "serverInfo" (%make-ht "name" "lisp-mcp-server" "version" (version))
            "capabilities" (%make-ht "tools" (%make-ht "listChanged" t)))))

(defun handle-notification (state method params)
  (declare (ignore state params))
  (when (string= method "notifications/initialized")
    (return-from handle-notification nil))
  nil)

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize")
     (handle-initialize state id params))

    ((string= method "tools/list")
     (let* ((repl-tool (%make-ht
                        "name" "repl.eval"
                        "description" "Evaluate Common Lisp forms with read-time eval disabled; returns last value as printed text."
                        "inputSchema" (%make-ht
                                       "type" "object"
                                       "properties"
                                       (let ((p (make-hash-table :test #'equal)))
                                         (setf (gethash "code" p)
                                               (%make-ht "type" "string" "description" "Code string of one or more forms"))
                                         (setf (gethash "package" p) (%make-ht "type" "string"))
                                         (setf (gethash "printLevel" p) (%make-ht "type" "integer"))
                                         (setf (gethash "printLength" p) (%make-ht "type" "integer"))
                                         p)))))
            (tools (vector repl-tool)))
       (%result id (%make-ht "tools" tools))))

    ((string= method "tools/call")
     (let* ((name (and params (gethash "name" params)))
            (args (and params (gethash "arguments" params))))
       (cond
         ((string= name "repl.eval")
          (let* ((code (and args (gethash "code" args)))
                 (pkg  (and args (gethash "package" args)))
                 (pl   (and args (gethash "printLevel" args)))
                 (plen (and args (gethash "printLength" args))))
            (multiple-value-bind (printed _)
                (repl-eval (or code "")
                           :package (or pkg *package*)
                           :print-level pl
                           :print-length plen)
              (declare (ignore _))
              (let* ((item (%make-ht "type" "text" "text" printed))
                     (content (make-array 1 :initial-contents (list item))))
                (%result id (%make-ht "content" content))))))
         (t
          (%error id -32601 (format nil "Tool ~A not found" name))))))

    (t
     (%error id -32601 (format nil "Method ~A not found" method)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let* ((msg (%decode-json line))
         (jsonrpc (gethash "jsonrpc" msg))
         (id (gethash "id" msg))
         (method (gethash "method" msg))
         (params (gethash "params" msg)))
    (log-event :debug "rpc.dispatch" "id" id "method" method)
    (unless (and (stringp jsonrpc) (string= jsonrpc "2.0"))
      (let ((resp (%encode-json (%error id -32600 "Invalid Request"))))
        (log-event :warn "rpc.invalid" "reason" "bad jsonrpc version")
        (return-from process-json-line resp)))
    (if method
        (if id
            (let ((r (handle-request state id method params)))
              (log-event :debug "rpc.result" "id" id "method" method)
              (%encode-json r))
            ;; notification
            (progn
              (handle-notification state method params)
              (log-event :debug "rpc.notify" "method" method)
              nil))
        (let ((resp (%encode-json (%error id -32600 "Invalid Request"))))
          (log-event :warn "rpc.invalid" "reason" "missing method")
          resp))))
