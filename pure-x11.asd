#+nil
(push :generate-pure-x11-doc cl:*features*)
(asdf:defsystem pure-x11
  :version "0"
  :description "Very simple socket based interface to X11"
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:sb-bsd-sockets #+generate-pure-x11-doc :mgl-pax)
  ;; note the dependency of mgl-pax is only for creating the
  ;; documentation.  if an appropriate defpackage call is placed in
  ;; package.lisp and a few of its definitions removed, this
  ;; dependency can be ommited (it depends on quite a few things
  ;; itself).
  :components ((:file "package")
               (:file "x" :depends-on ("package"))
               (:file "shm" :depends-on ("package" "x"))))
