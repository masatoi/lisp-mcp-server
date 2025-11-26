;;;; tests/fs-test.lisp

(defpackage #:lisp-mcp-server/tests/fs-test
  (:use #:cl #:rove)
  (:import-from #:lisp-mcp-server/src/fs
                #:fs-read-file
                #:fs-write-file
                #:fs-list-directory))

(in-package #:lisp-mcp-server/tests/fs-test)

(deftest fs-read-file-project
  (testing "fs-read-file reads project file with content"
    (let ((txt (fs-read-file "src/core.lisp" :offset 0 :limit 40)))
      (ok (stringp txt))
      (ok (> (length txt) 0)))))

(deftest fs-write-file-project
  (testing "fs-write-file writes under project root"
    (let* ((rel "tests/tmp-fs-write.txt")
           (content "hello world\n"))
      (unwind-protect
           (progn
             (ok (fs-write-file rel content))
             (let ((read (fs-read-file rel)))
               (ok (string= read content))))
        (ignore-errors (delete-file rel))))))

(deftest fs-list-directory-project
  (testing "fs-list-directory lists entries and filters hidden"
    (let* ((entries (fs-list-directory "."))
           (names (map 'list (lambda (h) (gethash "name" h)) entries)))
      (ok (find "src" names :test #'string=))
      (ok (not (find ".git" names :test #'string=))))))
