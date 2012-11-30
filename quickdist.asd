(asdf:defsystem quickdist
  :depends-on (alexandria
               cl-fad
               external-program
               quicklisp)
  :serial t
  :components ((:file "package")
               (:file "quickdist")))
