(ql:quickload "cells")
(ql:quickload "pure-x11")
(ql:quickload "sb-concurrency")
(ql:quickload "spatial-trees")
(ql:quickload "cl-containers")
(ql:quickload "alexandria")
(defpackage :g-cells
  (:use :cl :pure-x11 :cells :alexandria))
(in-package :g-cells)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

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

(handler-case (let ((fn (format nil "~a" (first (directory "/tmp/.X11-unix/X*")))))
		(format t "trying to open ~a.~%" fn)
	       (connect :filename fn))
  (sb-bsd-sockets:connection-refused-error ()

    (sb-ext:run-program "/usr/bin/socat" `("-d" "-d" "TCP-LISTEN:6000,fork,bind=localhost"
						,(format nil "UNIX-CONNECT:~a"
							 (first (directory "/tmp/.X11-unix/X*"))))
			:wait nil)
    (connect))
  #+nil (t
      (sb-ext:run-program "/usr/bin/xhost" `("+")
			:wait t)))



(progn ;; open a window and draw a line
  (make-window)
  (draw-window 0 0 100 100))

(draw-window 0 0 100 100)

(pure-x11::clear-area)

(query-pointer)
(force-output pure-x11::*s*)


#+nil
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
	  (format t "pointer ~{~?~^ ~}.~%"
		  `("pos=~{~4,'0d~^x~}" ((,event-x ,event-y))
		    "state=0x~4,'0x" (,state)
		    "ts=~6,'0d" (,timestamp)
		    "button=~a" (,button))))))
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
					 (initval 0)
					 (initform `(c-in (coerce ,initval ',type)) initform-p)
					 ) slot
	    `(,slot-name :accessor ,accessor
			 ,@(if initform-p
			       `(:initform ,initform)
			       `(:initarg ,initarg
					  :initform ,initform)
			       )
					;:type ,type
			 )))))

(.defmodel rect ()
	   #.`((x :initval 0)
	       (y :initval 0)
	       (xspan :initval 1)
	       (yspan :initval 1)
	     ,@(loop for c in '(x y) append
		    (loop for (n op) in '((min -) (max +)) collect
			 `(,(symbolicate c n)
			    :initform (c? (,op (,c self)
					     (/ (,(symbolicate 'x 'span) self)
						2))))))))

(defmethod draw ((self rect); &key (gc pure-x11::*gc*)
				)
  (format t "method-draw-rect")
  (progn
    (with-slots (xmin xmax ymin ymax) self
      (when (and xmin xmax ymin ymax)
       (let ((x1 (floor xmin))
	     (y1 (floor ymin))
	     (x2 (floor xmax))
	     (y2 (floor ymax))
	     (gc pure-x11::*gc*))
	 (draw-window x1 y1 x2 y1	 :gc gc
		      )
	 (draw-window x2 y1 x2 y2	 :gc gc
		      )
	 (draw-window x2 y2 x1 y2	:gc gc
		      )
	 (draw-window x1 y2 x1 y1	 :gc gc
		      ))))
   (force-output pure-x11::*s*)))

(pure-x11::clear-area)


(eval
 `(progn
    ,@(loop for e in '(x y xspan yspan xmin xmax ymin ymax) collect
	   `(defobserver ,e ((self rect))
	      (format t "~&~a=~a~%" ',e  new-value)
	      (draw self)
	      ))))


(defparameter *bla* (make-instance 'rect :x (c-in 200) :y (c-in 120) :xspan (c-in 30) :yspan (c-in 80)))

#+nil
(cells:cells-reset)

(pure-x11::clear-area)

(setf (x *bla*) 202)
