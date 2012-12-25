(defpackage #:quickdist-reader
  (:use #:cl)
  (:shadowing-import-from #:com.informatimago.common-lisp.lisp-reader.reader
                          #:*readtable* #:copy-readtable
                          #:set-dispatch-macro-character
                          #:read
                          #:symbol-in-missing-package-error #:intern-here
                          #:symbol-missing-in-package-error #:make-symbol)
  (:export #:safe-read))

(defpackage #:quickdist
  (:use #:cl #:alexandria)
  (:import-from #:quicklisp
                #:file-size)
  (:export #:quickdist
           #:*distinfo-template*
           #:*distinfo-file-template*
           #:*dist-dir-template*
           #:*archive-dir-template*
           #:*archive-url-template*
           #:*gnutar*))
