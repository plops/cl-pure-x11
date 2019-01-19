(ql:quickload "cells")
(ql:quickload "pure-x11")


(defpackage :g-cells
  (:use :cl :pure-x11 :cells))
(in-package :g-cells)
;; https://github.com/plops/cl-learn-cells
;; https://github.com/kennytilton/celtk

(handler-case 
    (connect)
  (sb-bsd-sockets:connection-refused-error ()
    (sb-ext:run-program "/usr/bin/socat" `("-d" "-d" "TCP-LISTEN:6000,fork,bind=localhost"
						,(format nil "UNIX-CONNECT:~a"
							 (first (directory "/tmp/.X11-unix/X*"))))
			:wait nil)
    (connect)))




(progn ;; open a window and draw a line
  (connect)
  (make-window)
  (draw-window 0 0 100 100))
