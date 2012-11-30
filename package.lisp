(defpackage #:quickdist
  (:use #:cl #:alexandria)
  (:import-from #:quicklisp
                #:file-size)
  (:import-from #:asdf
                #:load-sysdef)
  (:export #:quickdist
           #:*distinfo-template*
           #:*distinfo-file-template*
           #:*dist-dir-template*
           #:*archive-dir-template*
           #:*archive-url-template*))
