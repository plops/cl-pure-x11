(ql:quickload "cells")
(ql:quickload "pure-x11")
(ql:quickload "sb-concurrency")
(ql:quickload "spatial-trees")
(ql:quickload "cl-containers")
(ql:quickload "alexandria")
(defpackage :g-cells
  (:use :cl :pure-x11 :cells :alexandria))
(in-package :g-cells)
;; https://github.com/plops/cl-learn-cells
;; https://github.com/kennytilton/celtk
;; http://www.x.org/archive/X11R7.5/doc/x11proto/proto.pdf

;; spatial datastructure:
;; https://github.com/rpav/spatial-trees

;; storage for pointer history
;; https://github.com/gwkkwg/cl-containers/blob/master/dev/ring-buffers.lisp

;; constraints programming
;; 10.1.1.32.24.pdf


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
	  (sb-concurrency:send-message *mailbox-rx* (list event-x event-y))
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


(defparameter *ring* (cl-containers:make-container 'cl-containers:ring-buffer :total-size 12))



(cl-containers:insert-item  *ring*
			    (sb-concurrency:receive-message *mailbox-rx*))

(cl-containers:iterate-nodes *ring*
			     #'(lambda (item)
				 (format t "~a" item)))



(defstruct (p (:constructor p (x y)))
  x y)

(p 5 6)

(defun random-p (x y)
  (p (random (float x))
     (random (float y))))

(defun p-bbox (p)
  (with-slots (x y) p
    (rectangles:make-rectangle
     :lows (list (- x .1) (- y .1))
     :highs (list (+ x .1) (+ y .1)))))

(defparameter *tree* (spatial-trees:make-spatial-tree :r :rectfun #'p-bbox))

(loop for i below 100 do
     (spatial-trees:insert (random-p 5 5) *tree*))

(spatial-trees:search (random-p 3.5 3.5) *tree*)

(defmacro .defmodel (name super slots)
  `(defmodel ,name ,super
     ,(loop for slot in slots collect
	  (destructuring-bind (slot-name &key (accessor slot-name)
					 (initarg (make-keyword slot-name))
					 (type 'double-float)
					 (initform `(c-in (coerce 0 ',type)))
					 ) slot
	      `(,slot-name :accessor ,accessor
		      :initarg ,initarg
		      :initform ,initform
		      ;:type ,type
		      )))))

(.defmodel rect ()
	   #.`((x)
	     (y)
	     (xspan)
	     (yspan)
	     ,@(loop for c in '(x y) append
		    (loop for (n op) in '((min -) (max +)) collect
			 `(,(symbolicate c n)
			    :initform (c? (,op (self ,c)
					     (/ (self
						 ,(symbolicate 'x 'span)) 2))))))))



(make-instance 'rect)
