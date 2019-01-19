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



(progn ;; open a window and draw a line
  (make-window)
  (draw-window 0 0 100 100))

(pure-x11::clear-area)

(query-pointer)
(force-output pure-x11::*s*)



(loop for i below 100 do
     (format t "~a~%" (multiple-value-list (query-pointer)))
     (sleep .1))




(defparameter *mailbox-rx* (sb-concurrency:make-mailbox :name 'rx))
(sb-concurrency:list-mailbox-messages *mailbox-rx*)

(defun handle-x11-event (msg)
  "0 error
1 reply
4 button press
5 button release
6 pointer moved
12 expose"
  (let ((type (aref msg 0)))
   (cond
     ((eq type 0) (format t "error ~{~2x ~}~%" (loop for e across msg
						  collect e)))
     ((eq type 1) (format t "reply ~{~2x ~}~%" (loop for e across msg
						  collect e)))
     ((member type '(4 5 6))
      (multiple-value-bind (event-x event-y state timestamp)
	  (pure-x11::parse-motion-notify msg)
	(let ((button (pure-x11::key-button-r state)))
	  (format t "pointer ~a~%" `(:msg ,msg
					  :pos (,event-x ,event-y)
					  :state ,state
					  :ts ,timestamp
					  :button ,button)))))
     ((eq type 12)
      (format t "expose~%")
      )
     (t ;; some other event
      (format t "event ~{~2x ~}~%" (loop for e across msg
				      collect e))))))

(sb-thread:make-thread
 #'(lambda ()
     (loop while t do
	  (let ((msg (pure-x11::read-reply-wait)))
	    (handle-x11-event msg))))
 :name "x11-event-handler-thread")
