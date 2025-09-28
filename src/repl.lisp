;;;; src/repl.lisp
(in-package :lisp-mcp-server)

(defparameter *default-eval-package* (find-package :cl-user)
  "Default package in which `repl-eval` evaluates forms.")

(defparameter *default-eval-timeout* 10.0d0
  "Default evaluation timeout in seconds for REPL jobs.")

(defparameter *warned-timeout-unsupported* nil
  "Internal flag ensuring the timeout warning is logged once per image.")

(define-condition evaluation-timeout (error)
  ((seconds :initarg :seconds :reader evaluation-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Evaluation exceeded ~,3F seconds." (evaluation-timeout-seconds condition)))))

(defun set-default-eval-timeout (seconds)
  "Set the default REPL evaluation timeout to SECONDS seconds.
SECONDS must be a positive real number. The new value applies to future
evaluations."
  (check-type seconds (real (0) *))
  (setf *default-eval-timeout* (coerce seconds 'double-float)))

(defun %read-all (string)
  "Read all top-level forms from STRING and return them as a list."
  (let ((*readtable* (copy-readtable nil)))
    (with-input-from-string (in string)
      (loop for form = (read in nil :eof)
            until (eq form :eof)
            collect form))))

(defstruct (eval-job
            (:constructor %make-eval-job (code package print-level print-length timeout)))
  code
  package
  print-level
  print-length
  timeout
  printed
  value
  stdout
  stderr
  error
  (done-p nil)
  (lock (bordeaux-threads:make-lock))
  (condition (bordeaux-threads:make-condition-variable)))

(defstruct (eval-manager (:constructor %make-eval-manager ()))
  (lock (bordeaux-threads:make-lock))
  (condition (bordeaux-threads:make-condition-variable))
  (queue '())
  (thread nil)
  (shutdown-p nil))

(defvar *eval-manager* nil
  "Singleton evaluation manager coordinating the worker thread.")

(defmacro %with-eval-timeout ((seconds) &body body)
  "Execute BODY, aborting with `evaluation-timeout` if SECONDS elapse.
SECONDS may be NIL to disable the timeout."
  (let ((timeout-var (gensym "TIMEOUT")))
    `(let ((,timeout-var ,seconds))
       (if (and ,timeout-var (> ,timeout-var 0))
           #+sbcl
           (handler-case
               (sb-ext:with-timeout ,timeout-var
                 ,@body)
             (sb-ext:timeout ()
               (error 'evaluation-timeout :seconds ,timeout-var)))
           #-sbcl
           (progn
             (unless *warned-timeout-unsupported*
               (setf *warned-timeout-unsupported* t)
               (log-event :warn "repl.timeout-unsupported"
                          "implementation" (lisp-implementation-type)))
             ,@body)
           (progn ,@body)))))

(defun %normalize-timeout (timeout)
  (cond
    ((null timeout) (coerce *default-eval-timeout* 'double-float))
    ((typep timeout '(real (0) *)) (coerce timeout 'double-float))
    (t (error 'type-error :datum timeout :expected-type '(real (0) *)))))

(defun %complete-job (job &key printed value stdout stderr error)
  (bordeaux-threads:with-lock-held ((eval-job-lock job))
    (setf (eval-job-printed job) printed
          (eval-job-value job) value
          (eval-job-stdout job) stdout
          (eval-job-stderr job) stderr
          (eval-job-error job) error
          (eval-job-done-p job) t)
    (bordeaux-threads:condition-notify (eval-job-condition job))))

(defun %wait-for-job (job)
  (bordeaux-threads:with-lock-held ((eval-job-lock job))
    (loop until (eval-job-done-p job)
          do (bordeaux-threads:condition-wait (eval-job-condition job)
                                              (eval-job-lock job)))
    (if (eval-job-error job)
        (error (eval-job-error job))
        (values (eval-job-printed job)
                (eval-job-value job)
                (eval-job-stdout job)
                (eval-job-stderr job)))))

(defun %push-job (manager job)
  (bordeaux-threads:with-lock-held ((eval-manager-lock manager))
    (setf (eval-manager-queue manager)
          (nconc (eval-manager-queue manager) (list job)))
    (bordeaux-threads:condition-notify (eval-manager-condition manager)))
  job)

(defun %pop-job (manager)
  (bordeaux-threads:with-lock-held ((eval-manager-lock manager))
    (loop
      (when (eval-manager-shutdown-p manager)
        (return :shutdown))
      (let ((queue (eval-manager-queue manager)))
        (when queue
          (let ((job (first queue)))
            (setf (eval-manager-queue manager) (rest queue))
            (return job))))
      (bordeaux-threads:condition-wait (eval-manager-condition manager)
                                       (eval-manager-lock manager)))))

(defun %execute-job (job)
  (handler-case
      (multiple-value-bind (printed value stdout stderr)
          (%with-eval-timeout ((eval-job-timeout job))
            (%evaluate-job (eval-job-code job)
                           (eval-job-package job)
                           (eval-job-print-level job)
                           (eval-job-print-length job)))
        (%complete-job job :printed printed :value value :stdout stdout :stderr stderr))
    (condition (c)
      (%complete-job job :error c))))

(defun %worker-loop (manager)
  (loop for job = (%pop-job manager)
        until (eq job :shutdown)
        do (%execute-job job)))

(defun make-eval-manager ()
  (let ((manager (%make-eval-manager)))
    (setf (eval-manager-thread manager)
          (bordeaux-threads:make-thread (lambda () (%worker-loop manager))
                                        :name "mcp-eval-worker"))
    manager))

(defun ensure-eval-manager ()
  (let ((manager *eval-manager*))
    (if (and manager
             (eval-manager-thread manager)
             (bordeaux-threads:thread-alive-p (eval-manager-thread manager)))
        manager
        (setf *eval-manager* (make-eval-manager)))))

(defun %evaluate-job (code package print-level print-length)
  (let ((*package* package))
    (let* ((forms (%read-all code))
           (last-value nil)
           (stdout-stream (make-string-output-stream))
           (stderr-stream (make-string-output-stream))
           (orig-out *standard-output*)
           (orig-err *error-output*))
      (let ((*standard-output* (if orig-out
                                   (make-broadcast-stream stdout-stream orig-out)
                                   stdout-stream))
            (*error-output* (if orig-err
                                (make-broadcast-stream stderr-stream orig-err)
                                stderr-stream)))
        (dolist (form forms)
          (setf last-value (eval form)))
        (let ((*print-level* print-level)
              (*print-length* print-length))
          (values (prin1-to-string last-value)
                  last-value
                  (get-output-stream-string stdout-stream)
                  (get-output-stream-string stderr-stream)))))))

(declaim (ftype (function (string &key (:package (or package symbol string))
                                  (:print-level (or null (integer 0)))
                                  (:print-length (or null (integer 0)))
                                  (:timeout (or null (real (0) *))))
                          (values string t &optional string string))
                repl-eval))

(defun repl-eval (input &key (package *default-eval-package*)
                             (print-level nil) (print-length nil)
                             timeout)
  "Evaluate INPUT (a string of one or more s-expressions) in PACKAGE.

Forms are read and evaluated sequentially in a dedicated evaluation worker
thread. The last value is returned both as a printed string and as the raw
value, alongside any text captured from standard output and error streams.
Evaluation aborts with `evaluation-timeout` when the specified timeout elapses."
  (check-type input string)
  (let* ((pkg (etypecase package
                (package package)
                (symbol (or (find-package package)
                            (error 'package-error :package package)))
                (string (or (find-package package)
                            (error 'package-error :package package)))))
         (deadline (%normalize-timeout (or timeout *default-eval-timeout*)))
         (manager (ensure-eval-manager))
         (job (%make-eval-job input pkg print-level print-length deadline)))
    (%push-job manager job)
    (%wait-for-job job)))
