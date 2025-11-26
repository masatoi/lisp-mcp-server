;;;; src/protocol.lisp
(in-package :lisp-mcp-server)

(defparameter +protocol-version+ "2025-06-18")
(defparameter +supported-protocol-versions+
  '("2025-06-18" "2025-03-26" "2024-11-05")
  "Supported MCP protocol versions, ordered by preference.")

(defclass server-state ()
  ((initialized-p :initform nil :accessor initialized-p)
   (client-info  :initform nil :accessor client-info)))

(defun make-state () (make-instance 'server-state))

(defun %decode-json (line)
  (yason:parse line))

(defun %encode-json (obj)
  (with-output-to-string (stream)
    (yason:encode obj stream)))

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
  (declare (ignore state))
  (let* ((client-ver (and params (gethash "protocolVersion" params)))
         (chosen (if (and client-ver (find client-ver +supported-protocol-versions+
                                           :test #'string=))
                     client-ver
                     (first +supported-protocol-versions+)))
         (caps (%make-ht
                "tools" (%make-ht "listChanged" t))))
    (%result id
             (%make-ht
              "protocolVersion" chosen
              "serverInfo" (%make-ht "name" "lisp-mcp-server" "version" (version))
              "capabilities" caps))))

(defun handle-notification (state method params)
  (declare (ignore state params))
  (when (string= method "notifications/initialized")
    (return-from handle-notification nil))
  nil)

(defun tools-descriptor-repl ()
  (%make-ht
   "name" "repl-eval"
   "description" "Evaluate Common Lisp forms and return the last value as printed text."
   "inputSchema" (%make-ht
                  "type" "object"
                  "properties" (let ((p (make-hash-table :test #'equal)))
                                  (setf (gethash "code" p)
                                        (%make-ht "type" "string" "description" "Code string of one or more forms"))
                                  (setf (gethash "package" p) (%make-ht "type" "string"))
                                  (setf (gethash "printLevel" p) (%make-ht "type" "integer"))
                                  (setf (gethash "printLength" p) (%make-ht "type" "integer"))
                                  p))))

(defun tools-descriptor-fs-read ()
  (%make-ht
   "name" "fs-read-file"
   "description" "Read a text file with optional offset and limit."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p) (%make-ht "type" "string"))
                   (setf (gethash "offset" p) (%make-ht "type" "integer"))
                   (setf (gethash "limit" p) (%make-ht "type" "integer"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))

(defun tools-descriptor-fs-write ()
  (%make-ht
   "name" "fs-write-file"
   "description" "Write text content to a file relative to project root."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p) (%make-ht "type" "string"))
                   (setf (gethash "content" p) (%make-ht "type" "string"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path" "content")))))

(defun tools-descriptor-fs-list ()
  (%make-ht
   "name" "fs-list-directory"
   "description" "List entries in a directory, filtering hidden and build artifacts."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "path" p) (%make-ht "type" "string"))
                   (%make-ht "type" "object" "properties" p "required" (vector "path")))))

(defun tools-descriptor-code-find ()
  (%make-ht
   "name" "code-find"
   "description" "Locate the definition of a symbol (path and line) using sb-introspect."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "symbol" p)
                         (%make-ht "type" "string" "description" "Symbol name like \"cl:mapcar\""))
                   (setf (gethash "package" p)
                         (%make-ht "type" "string" "description" "Optional package used when SYMBOL is unqualified"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "symbol")))))

(defun tools-descriptor-code-describe ()
  (%make-ht
   "name" "code-describe"
   "description" "Describe a symbol: type, arglist, and documentation."
   "inputSchema" (let ((p (make-hash-table :test #'equal)))
                   (setf (gethash "symbol" p)
                         (%make-ht "type" "string" "description" "Symbol name like \"cl:mapcar\""))
                   (setf (gethash "package" p)
                         (%make-ht "type" "string" "description" "Optional package used when SYMBOL is unqualified"))
                   (%make-ht "type" "object"
                             "properties" p
                             "required" (vector "symbol")))))

(defun handle-tools-list (id)
  (let* ((tools (vector (tools-descriptor-repl)
                        (tools-descriptor-fs-read)
                        (tools-descriptor-fs-write)
                        (tools-descriptor-fs-list)
                        (tools-descriptor-code-find)
                        (tools-descriptor-code-describe))))
    (%result id (%make-ht "tools" tools))))

(defun %normalize-tool-name (name)
  "Normalize a tool NAME possibly namespaced like 'ns.tool' or 'ns/tool'.
Returns a downcased local tool name (string)."
  (let* ((s (string-downcase name))
         (dot (position #\. s :from-end t))
         (sl (position #\/ s :from-end t))
         (idx (max (or dot -1) (or sl -1))))
    (subseq s (1+ idx))))

(defun handle-tools-call (id params)
  (let* ((name (and params (gethash "name" params)))
         (args (and params (gethash "arguments" params)))
         (local (and name (%normalize-tool-name name))))
    (when name (log-event :debug "tools.call" "name" name "local" local))
    (cond
      ((member local '("repl-eval" "repl.eval" "repl_eval") :test #'string=)
       (handler-case
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
                 (%result id (%make-ht "content" content)))))
        (error (e)
          (%error id -32603
                  (format nil "Internal error during REPL evaluation: ~A" e)))))
      ((member local '("fs-read-file" "fs_read_file" "read_file" "read") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (offset (and args (gethash "offset" args)))
                  (limit (and args (gethash "limit" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
             (let ((content (fs-read-file path :offset offset :limit limit)))
               (%result id (%make-ht "content" content))))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs-read-file: ~A" e)))))
      ((member local '("fs-write-file" "fs_write_file" "write_file" "write") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args)))
                  (content (and args (gethash "content" args))))
             (unless (and (stringp path) (stringp content))
               (return-from handle-tools-call
                 (%error id -32602 "path and content must be strings")))
              (fs-write-file path content)
              (%result id (%make-ht "success" t)))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs_write_file: ~A" e)))))
      ((member local '("fs-list-directory" "fs_list_directory" "list_directory"
                       "list" "ls") :test #'string=)
       (handler-case
           (let* ((path (and args (gethash "path" args))))
             (unless (stringp path)
               (return-from handle-tools-call
                 (%error id -32602 "path must be a string")))
              (let ((entries (fs-list-directory path)))
                (%result id (%make-ht "entries" entries))))
         (error (e)
           (%error id -32603 (format nil "Internal error during fs-list-directory: ~A" e)))))
      ((member local '("code-find" "code_find" "find" "find_definition") :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (multiple-value-bind (path line)
                 (code-find-definition symbol :package pkg)
                (if path
                    (%result id (%make-ht "path" path "line" line))
                    (%error id -32004 (format nil "Definition not found for ~A" symbol)))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during code-find: ~A" e)))))
      ((member local '("code-describe" "code_describe" "describe" "describe_symbol") :test #'string=)
       (handler-case
           (let* ((symbol (and args (gethash "symbol" args)))
                  (pkg (and args (gethash "package" args))))
             (unless (stringp symbol)
               (return-from handle-tools-call
                 (%error id -32602 "symbol must be a string")))
             (multiple-value-bind (name type arglist doc)
                 (code-describe-symbol symbol :package pkg)
                (%result id (%make-ht "name" name
                                      "type" type
                                      "arglist" arglist
                                      "documentation" doc))))
         (error (e)
           (%error id -32603
                   (format nil "Internal error during code-describe: ~A" e)))))
      (t
       (%error id -32601 (format nil "Tool ~A not found" name))))))

(defun handle-request (state id method params)
  (cond
    ((string= method "initialize") (handle-initialize state id params))
    ((string= method "tools/list") (handle-tools-list id))
    ((string= method "tools/call") (handle-tools-call id params))
    ((string= method "ping") (%result id (%make-ht)))
    (t (%error id -32601 (format nil "Method ~A not found" method)))))

(defun process-json-line (line &optional (state (make-state)))
  "Process one JSON-RPC line and return a JSON line to send, or NIL for notifications."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) line)))
    (when (string= trimmed "")
      (log-event :debug "rpc.skip-empty")
      (return-from process-json-line nil))
    (let* ((msg (%decode-json trimmed))
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
            resp)))))
