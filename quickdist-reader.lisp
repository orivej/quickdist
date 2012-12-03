(in-package #:quickdist-reader)

(defvar *safe-readtable*
  (let ((readtable (copy-readtable *readtable*)))
    (flet ((read* (stream &rest ignore)
             (declare (ignore ignore))
             (safe-read stream nil (values) t)))
      (set-dispatch-macro-character #\# #\. #'read* readtable))
    readtable))

(defun safe-read (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
  (handler-bind ((symbol-in-missing-package-error (lambda (c) (declare (ignore c)) (invoke-restart 'intern-here)))
                 (symbol-missing-in-package-error (lambda (c) (declare (ignore c)) (invoke-restart 'make-symbol))))
    (let ((*readtable* *safe-readtable*))
      (read stream eof-error-p eof-value recursive-p))))
