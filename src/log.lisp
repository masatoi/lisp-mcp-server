;;;; src/log.lisp
(in-package :lisp-mcp-server)

(defparameter *log-level* :debug)
(defparameter *log-stream* *error-output*)
(defparameter *error-detail* :backtrace)

(defun %level->int (level)
  (ecase level
    (:debug 10)
    (:info 20)
    (:warn 30)
    (:error 40)))

(defun %parse-level (s)
  (cond
    ((null s) nil)
    ((string= s "debug") :debug)
    ((string= s "info") :info)
    ((string= s "warn") :warn)
    ((string= s "warning") :warn)
    ((string= s "error") :error)
    (t nil)))

(defun set-log-level-from-env ()
  (let* ((env (uiop:getenv "MCP_LOG_LEVEL"))
         (lvl (%parse-level (and env (string-downcase env)))))
    (when lvl (setf *log-level* lvl))
    *log-level*))

(defun %parse-error-detail (s)
  (cond
    ((null s) nil)
    ((string= s "none") :none)
    ((string= s "message") :message)
    ((string= s "backtrace") :backtrace)
    (t nil)))

(defun set-error-detail-from-env ()
  (let* ((env (uiop:getenv "MCP_ERROR_DETAIL"))
         (val (%parse-error-detail (and env (string-downcase env)))))
    (when val (setf *error-detail* val))
    *error-detail*))

(defun %ts-iso8601 ()
  (multiple-value-bind (sec min hour day mon year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year mon day hour min sec)))

(defun should-log-p (level)
  (<= (%level->int level) (%level->int *log-level*)))

(defun log-event (level event &rest kvs)
  "Emit a JSON log line to *log-stream* with LEVEL and EVENT.
Additional key-values KV can be provided as alternating strings and values."
  (when (should-log-p level)
    (let ((obj (make-hash-table :test #'equal)))
      (setf (gethash "ts" obj) (%ts-iso8601))
      (setf (gethash "level" obj) (string-downcase (symbol-name level)))
      (setf (gethash "event" obj) event)
      (loop for (k v) on kvs by #'cddr
            when k do (setf (gethash k obj) v))
      (yason:encode obj *log-stream*)
      (terpri *log-stream*)
      (finish-output *log-stream*))))

;; initialize level from env at load
(set-log-level-from-env)

;; initialize error detail policy from env at load
(set-error-detail-from-env)

(defun %backtrace-string (e)
  (when (eql *error-detail* :backtrace)
    (handler-case
        (with-output-to-string (s)
          (trivial-backtrace:print-backtrace e :output s))
      (t () nil))))

(defun condition-type-string (c)
  (let ((nm (ignore-errors (class-name (class-of c)))))
    (cond
      ((symbolp nm) (string-downcase (symbol-name nm)))
      ((stringp nm) (string-downcase nm))
      (t (string-downcase (princ-to-string (type-of c)))))))
