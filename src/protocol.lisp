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

(defun %result (id payload)
  (list :jsonrpc "2.0" :id id :result payload))

(defun %error (id code message &optional data)
  (list :jsonrpc "2.0" :id id :error (append (list :code code :message message)
                                              (when data (list :data data)))))

(defun handle-initialize (state id params)
  (declare (ignore params))
  (setf (initialized-p state) t)
  (%result id
           (list :protocolVersion +protocol-version+
                 :serverInfo (list :name "lisp-mcp-server" :version (version))
                 :capabilities (list :tools (list :listChanged t)))))

(defun handle-notification (state method params)
  (declare (ignore state params))
  (ecase method
    ("notifications/initialized" nil)))

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize") (handle-initialize state id params))
    (t (%error id -32601 (format nil "Method ~A not found" method)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let* ((msg (%decode-json line))
         (jsonrpc (gethash "jsonrpc" msg))
         (id (gethash "id" msg))
         (method (gethash "method" msg))
         (params (gethash "params" msg)))
    (unless (and (stringp jsonrpc) (string= jsonrpc "2.0"))
      (return-from process-json-line (%encode-json (%error id -32600 "Invalid Request"))))
    (if method
        (if id
            (%encode-json (handle-request state id method params))
            ;; notification
            (progn (handle-notification state method params) nil))
        (%encode-json (%error id -32600 "Invalid Request")))))
