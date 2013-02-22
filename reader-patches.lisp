;;; The code in this file is a modified copy of PJB's reader,
;;; which is licensed under AGPL3; whatever this means.

(in-package #:com.informatimago.common-lisp.lisp-reader.reader)

;;; Work around rational in base inside a disabled feature,
;;; such as #+xxx #o0
(defun read-rational-in-base (stream arg sub-char *read-base*)
  "
DO:      Read a rational number in the base specified.
RETURN:  The rational read.
"
  (when arg (serror 'simple-reader-error stream "no number allowed between # and ~A" sub-char))
  (let ((value (let ((*read-suppress* nil)) (read stream t nil t))))
    (if (rationalp value)
        value
        (serror 'simple-reader-error stream
                "token \"~A\" after #~A is not a rational number in base ~D"
                value sub-char *read-base*))))
