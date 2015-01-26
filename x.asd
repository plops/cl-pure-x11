(asdf:defsystem pylon
  :version "0"
  :description "Very simple socket based interface to X11"
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:sb-bsd-sockets)
  :components ((:file "package")
               (:file "x" :depends-on ("package"))
               (:file "shm" :depends-on ("package" "x"))))
