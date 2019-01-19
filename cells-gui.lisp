(ql:quickload "cells")
(ql:quickload "pure-x11")
(ql:quickload "sb-concurrency")
(ql:quickload "spatial-trees")
(defpackage :g-cells
  (:use :cl :pure-x11 :cells))
(in-package :g-cells)
;; https://github.com/plops/cl-learn-cells
;; https://github.com/kennytilton/celtk
;; http://www.x.org/archive/X11R7.5/doc/x11proto/proto.pdf

;; possible spatial datastructures:
;; http://norvig.com/ltd/test/kd.lisp
;; https://github.com/nikodemus/raylisp/blob/master/kernel/kd-tree.lisp
;; https://github.com/rpav/spatial-trees


;; during development connect using localhost:6000 so that I can see
;; the packages in wireshark

;; if connection fails with reason 'no protocol specified' call 'xhost +'

(handler-case 
    (connect)
  (sb-bsd-sockets:connection-refused-error ()
    (sb-ext:run-program "/usr/bin/socat" `("-d" "-d" "TCP-LISTEN:6000,fork,bind=localhost"
						,(format nil "UNIX-CONNECT:~a"
							 (first (directory "/tmp/.X11-unix/X*"))))
			:wait nil)
    (connect)))

(connect)


(progn ;; open a window and draw a line
  (make-window)
  (draw-window 0 0 100 100))

(pure-x11::clear-area)

(query-pointer)
(force-output pure-x11::*s*)



(loop for i below 100 do
     (format t "~a~%" (multiple-value-list (query-pointer)))
     (sleep .1))
