;;;; src/fs.lisp
(in-package :lisp-mcp-server)

(defparameter *project-root*
  (or (ignore-errors
        (uiop:ensure-directory-pathname
         (asdf:system-source-directory "lisp-mcp-server")))
      (uiop:ensure-directory-pathname (uiop:getcwd)))
  "Absolute pathname of the project root. Prefer the ASDF system source
directory; fall back to the current working directory at load time.")

(defparameter *hidden-prefixes* '("." ".git" ".hg" ".svn" ".cache" ".fasl"))
(defparameter *skip-extensions* '("fasl" "ufasl" "x86f" "cfasl"))

(defun %path-inside-p (child parent)
  "Return T when CHILD pathname is a subpath of directory PARENT."
  (uiop:subpathp child parent))

(defun %canonical-path (path &key relative-to)
  "Turn PATH designator into a physical absolute pathname.
If RELATIVE-TO is provided and PATH is relative, merge it with RELATIVE-TO."
  (let* ((pn (uiop:ensure-pathname path :want-relative nil
                                   :ensure-directory nil :ensure-absolute nil))
         (abs (if (uiop:absolute-pathname-p pn)
                  pn
                  (uiop:merge-pathnames* pn (or relative-to *project-root*)))))
    (uiop:ensure-pathname abs :want-relative nil)))

(defun %allowed-read-path-p (pn)
  "Return PN if readable per policy, else NIL.
Allows project-root subpaths and source dirs of registered ASDF systems."
  (let* ((abs (%canonical-path pn))
         (project-ok (%path-inside-p abs (uiop:ensure-directory-pathname *project-root*))))
    (when project-ok (return-from %allowed-read-path-p abs))
    ;; absolute path allowed only inside system-source-directory of registered systems
    (let ((systems (asdf:registered-systems)))
      (dolist (name systems)
        (let ((dir (ignore-errors (asdf:system-source-directory name))))
          (when (and dir (%path-inside-p abs dir))
            (return-from %allowed-read-path-p abs)))))
    nil))

(defun %ensure-write-path (path)
  "Ensure PATH is relative to project root and return absolute pathname.
Signals an error if outside project root or absolute."
  (let* ((pn (uiop:ensure-pathname path :want-relative t))
         (abs (%canonical-path pn :relative-to *project-root*)))
    (unless (%path-inside-p abs (uiop:ensure-directory-pathname *project-root*))
      (error "Write path ~A is outside project root" path))
    abs))

(defun %read-file-string (pn offset limit)
  "Read file PN honoring OFFSET and LIMIT (both may be NIL)."
  (let* ((content (uiop:read-file-string pn))
         (start (max 0 (or offset 0)))
         (end (if (and limit (>= limit 0))
                  (min (length content) (+ start limit))
                  (length content))))
    (subseq content start end)))

(defun fs-read-file (path &key offset limit)
  "Read text file PATH with optional OFFSET and LIMIT.
Returns the content string."
  (when (and offset (not (integerp offset)))
    (error "offset must be an integer"))
  (when (and limit (not (integerp limit)))
    (error "limit must be an integer"))
  (let ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (%read-file-string pn offset limit)))

(defun %write-string-to-file (pn content)
  (uiop/filesystem::ensure-directories-exist pn)
  (with-open-file (out pn :direction :output :if-exists :supersede :if-does-not-exist :create
                       :element-type 'character)
    (write-string content out)
    (finish-output out))
  t)

(defun fs-write-file (path content)
  "Write CONTENT to PATH relative to project root.
Returns T on success."
  (let ((pn (%ensure-write-path path)))
    (%write-string-to-file pn content)))

(defun %should-skip-entry-p (path)
  (let* ((name (pathname-name path))
         (type (pathname-type path))
         (str (and name (string name))))
    (or (null str)
        (some (lambda (pref) (uiop:string-prefix-p pref str)) *hidden-prefixes*)
        (and type (member (string-downcase type) *skip-extensions* :test #'string=)))))

(defun fs-list-directory (path)
  "List directory entries at PATH respecting read allow-list.
Returns a vector of hash-tables with keys \"name\" and \"type\" (file|directory)."
  (let* ((pn (%allowed-read-path-p path)))
    (unless pn
      (error "Read not permitted for path ~A" path))
    (unless (uiop:directory-exists-p pn)
      (error "Directory ~A does not exist or is not readable" path))
    (let* ((pattern (uiop:merge-pathnames* #P"*" pn))
           (entries (directory pattern))
           (results '()))
      (dolist (p entries)
        (unless (%should-skip-entry-p p)
          (let ((h (make-hash-table :test #'equal)))
            (setf (gethash "name" h) (file-namestring p)
                  (gethash "type" h) (if (uiop:directory-pathname-p p) "directory" "file"))
            (push h results))))
      (coerce (nreverse results) 'vector))))
