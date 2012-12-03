(in-package #:quickdist)

(defparameter *distinfo-template*
  "name: {name}
version: {version}
distinfo-subscription-url: {base-url}/{name}.txt
release-index-url: {base-url}/{name}/{version}/releases.txt
system-index-url: {base-url}/{name}/{version}/systems.txt
")
(defparameter *distinfo-file-template* "{dists-dir}/{name}.txt")
(defparameter *dist-dir-template*      "{dists-dir}/{name}/{version}")
(defparameter *archive-dir-template*   "{dists-dir}/{name}/archive")
(defparameter *archive-url-template*   "{base-url}/{name}/archive")

(defparameter *md5sum-command* (list "/usr/bin/md5sum" '()))
(defparameter *sha1sum-command* (list "/usr/bin/sha1sum" '()))
(defparameter *tar-command* (list "/bin/tar" '()))

(defvar *template-readtable*
  (let ((readtable (copy-readtable)))
    (set-syntax-from-char #\} #\) readtable)
    readtable))

(defun read-template-form (stream)
  (let ((*readtable* *template-readtable*)
        (*package* (symbol-package :keyword)))
    (read-delimited-list #\} stream)))

(defmacro do-character-stream ((var stream &optional result) &body body)
  `(loop for ,var = (read-char ,stream nil)
         while ,var do ,@body
         finally (return ,result)))

(defun render-template (template data)
  (with-output-to-string (out)
    (with-input-from-string (in template)
      (do-character-stream (c in)
        (if (not (char= c #\{))
            (write-char c out)
            (let ((form (read-template-form in)))
              (princ (or (getf data (car form))
                         (error "The value of {~a} is undefined." (car form)))
                     out)))))))

(defun effective-mtime (path)
  (if (not (fad:directory-pathname-p path))
      (file-write-date path)
      (apply #'max 0 (mapcar #'effective-mtime (fad:list-directory path)))))

(defun format-date (universal-time)
  (let* ((time (multiple-value-list (decode-universal-time universal-time)))
         (date (reverse (subseq time 3 6))))
    (format nil "~{~2,'0d~}" date)))

(defun external-program-word (&rest run-args)
  (let* ((s (with-output-to-string (out)
              (apply #'external-program:run (append run-args `(:output ,out))))))
    (subseq s 0 (position #\Space s))))

(defun md5sum (path)
  (external-program-word (first *md5sum-command*)
                         (append (second *md5sum-command*)
                                 (list (princ-to-string path)))))

(defun tar-content-sha1 (path)
  (let ((tar (external-program:start (first *tar-command*)
                                     (append (second *tar-command*)
                                             (list "-xOf" path)) :output :stream)))
    (external-program-word (first *sha1sum-command*)
                           (second *sha1sum-command*)
                           :input (external-program:process-output-stream tar))))

(defun last-directory (path)
  (first (last (pathname-directory path))))

(defun archive (destdir-path source-path)
  (let* ((mtime (format-date (effective-mtime source-path)))
         (name (format nil "~a-~a" (last-directory source-path) mtime))
         (out-path (make-pathname :name name :type "tgz" :defaults (truename destdir-path))))
    (external-program:run (first *tar-command*)
                          (append (second *tar-command*)
                                  (list "-C" (princ-to-string source-path) "."
                                        "-czf" (princ-to-string out-path)
                                        "--transform" (format nil "s#^.#~a#" name)))
                          :output *standard-output* :error *error-output*)
    out-path))

(defun find-system-files (path)
  (let ((system-files nil))
    (flet ((add-system-file (path) (push path system-files))
           (asd-file-p (path) (string-equal "asd" (pathname-type path))))
      (fad:walk-directory path #'add-system-file :test #'asd-file-p))
    (sort system-files #'string< :key #'pathname-name)))

(defun asdf-dependency-name (form)
  (cond
    ((and (listp form) (eq :version (first form)))
     (second form))
    (t form)))

(defun get-systems (asd-path)
  (with-open-file (s asd-path)
    (let* ((package (make-package (symbol-name (gensym "TMPPKG")) :use '(:cl :asdf)))
           (*package* package))
      (unwind-protect
           (sort
            (loop for form = (quickdist-reader:safe-read s nil)
                  while form
                  when (and (symbolp (car form))
                            (equalp "defsystem" (symbol-name (car form))))
                  collect (list* (cadr form)
                                 (sort (mapcar #'asdf-dependency-name
                                               (append (getf form :defsystem-depends-on)
                                                       (getf form :depends-on)))
                                       #'string-lessp)))
            #'string-lessp :key #'first)
        (delete-package package)))))

(defun unix-filename (path)
  (format nil "~a.~a" (pathname-name path) (pathname-type path)))

(defun unix-filename-relative-to (base path)
  (let ((base-name (princ-to-string (truename base)))
        (path-name (princ-to-string (truename path))))
    (subseq path-name (mismatch base-name path-name))))

(defun create-dist (projects-path dist-path archive-path archive-url)
  (with-open-file (release-index (make-pathname :name "releases" :type "txt" :defaults dist-path)
                                 :direction :output :if-exists :supersede)
    (write-line "# project url size file-md5 content-sha1 prefix [system-file1..system-fileN]" release-index)
    (with-open-file (system-index (make-pathname :name "systems" :type "txt" :defaults dist-path)
                                  :direction :output :if-exists :supersede)
      (write-line "# project system-file system-name [dependency1..dependencyN]" system-index)
      (dolist (project-path (fad:list-directory projects-path))
        (when (fad:directory-pathname-p project-path)
          (let ((system-files (find-system-files project-path)))
            (if (not system-files)
                (warn "No .asd files found in ~a, skipping." project-path)
                (with-simple-restart (skip-project "Skip this project, continue with the next.")
                  (let* ((tgz-path (archive archive-path project-path))
                         (project-name (last-directory project-path))
                         (project-prefix (pathname-name tgz-path))
                         (project-url (format nil "~a/~a" archive-url (unix-filename tgz-path))))
                    (format *error-output* "Processing ~a...~%" project-name)
                    (format release-index "~a ~a ~a ~a ~a ~a~{ ~a~}~%"
                            project-name project-url (file-size tgz-path) (md5sum tgz-path) (tar-content-sha1 tgz-path) project-prefix
                            (mapcar (curry #'unix-filename-relative-to project-path) system-files))
                    (dolist (system-file system-files)
                      (dolist (name-and-dependencies (get-systems system-file))
                        (let ((*print-case* :downcase))
                          (format system-index "~a ~a ~a~{ ~a~}~%"
                                  project-name (pathname-name system-file) (first name-and-dependencies)
                                  (rest name-and-dependencies))))))))))))))

(defun quickdist (&key name (version :today) base-url projects-dir dists-dir)
  (let* ((version (if (not (eq version :today)) version (format-date (get-universal-time))))
         (projects-path (fad:pathname-as-directory projects-dir))
         (template-data (list :name name :version version
                              :base-url (string-right-trim "/" base-url)
                              :dists-dir (string-right-trim "/" (princ-to-string dists-dir))))
         (distinfo-path (fad:pathname-as-file (render-template *distinfo-file-template* template-data)))
         (dist-path (fad:pathname-as-directory (render-template *dist-dir-template* template-data)))
         (archive-path (fad:pathname-as-directory (render-template *archive-dir-template* template-data)))
         (archive-url (render-template *archive-url-template* template-data)))
    (assert (fad:directory-exists-p projects-path))
    (ensure-directories-exist dist-path :verbose t)
    (ensure-directories-exist archive-path :verbose t)
    (create-dist projects-path dist-path archive-path archive-url)
    (let ((distinfo (render-template *distinfo-template* template-data)))
      (dolist (path (list (make-pathname :name "distinfo" :type "txt" :defaults dist-path)
                          distinfo-path))
        (write-string-into-file distinfo path :if-exists :supersede)))))
