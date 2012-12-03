(asdf:defsystem quickdist
  :depends-on (alexandria
               cl-fad
               external-program
               quicklisp
               com.informatimago.common-lisp.lisp-reader)
  :serial t
  :components ((:file "package")
               (:file "quickdist-reader")
               (:file "quickdist")))
