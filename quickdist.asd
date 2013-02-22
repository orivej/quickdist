(asdf:defsystem quickdist
  :description "Create a Quicklisp distribution from a directory of local projects."
  :author "Orivej Desh <orivej@gmx.fr>"
  :licence "Unlicense <http://unlicense.org/UNLICENSE>"
  :depends-on (alexandria
               cl-fad
               external-program babel-streams ironclad
               quicklisp
               com.informatimago.common-lisp.lisp-reader)
  :serial t
  :components ((:file "package")
               (:file "quickdist-reader")
               (:file "quickdist")
               (:file "reader-patches")))
