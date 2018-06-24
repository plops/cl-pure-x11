
(declaim (optimize (safety 3) (debug 3) (speed 0)))
(ql:quickload :defclass-std)
(ql:quickload :pure-x11)
(require :sb-concurrency)

(defpackage :g
  (:use :cl :pure-x11 :defclass-std))
(in-package :g)

;; nowadays X servers normally don't listen on port 6000
;; make it work without restarting
;; socat -d -d TCP-LISTEN:6000,fork,bind=localhost UNIX-CONNECT:/tmp/.X11-unix/X0
;; the tcp connection is useful because I can see the communication in wireshark
;; https://askubuntu.com/questions/41330/let-xorg-listen-on-tcp-but-only-to-localhost




;(pure-x11::clear-area)
;(draw-window 0 0 120 200)
;(imagetext8 "hello" :x 100 :y 100)
;(query-pointer)
;(force-output pure-x11::*s*)

;; first i have to sort out the socket handling i have listen
;; continuously for events and parse them. use a single thread that
;; listens and sends. use two mailboxes to communicate with the rest
;; of the program


;; the server sends three types of packages back: errors, events and
;; replies. errors and events (page 176) are 32 bytes long. 

;; replies always contain a length and a sequence number. that means
;; i can easily read all replies. i can skip replies even if i didn't
;; implement a parser for them yet.
 

;; then i need some polygon handling (point in polygon test)
;; https://cses.fi/book.pdf p. 269 geometry
;; and datastructures for many objects (quadree or kdtree)
;; p. 1097 in numerical recipes



(printing-unreadably
 (coord)
 (defclass/std vec2 ()
   ((coord :type (complex double-float)))))

#+nil
(defun vec2 (x &optional y)
  (make-instance 'vec2 :coord (complex (* 1d0 x) (* 1d0 y))))

(defmethod vec2 ((x number) &optional (y 0d0))
  (make-instance 'vec2 :coord (complex (* 1d0 x) (* 1d0 y))))

(defmethod vec2 ((z complex) &optional y)
  (make-instance 'vec2 :coord z))

#+nil
(vec2 2d0 3d0)
#+nil
(vec2 (complex 1d0))

#+nil
(vec2 1 2)

(defmethod dot ((a vec2) (b vec2))
  (realpart (* (coord b) (conjugate (coord a)))))

#+nil
(dot (vec2 1 2) (vec2 1 1))

(defmethod norm ((a vec2))
  (sqrt (dot a a)))
#+nil
(norm (vec2 1 1))

(defmethod dist ((a vec2) (b vec2))
  (let ((d  (- (coord a) (coord b))))
   (sqrt (realpart (* d (conjugate d))))))

(dist (vec2 0 1) (vec2 1 0))

(printing-unreadably (lo hi)
 (defclass/std box ()
   ((lo :type vec2)
    (hi :type vec2))))

(defun box (cx cy w h)
  (make-instance 'box
		 :lo (vec2 (- cx (* .5 w))
			   (- cy (* .5 h)))
		 :hi (vec2 (+ cx (* .5 w))
			   (+ cy (* .5 h)))))
#+nil
(box 100 100 30 8)

(defmethod rmove ((b box) (v vec2))
  (incf (coord (lo b)) (coord v))
  (incf (coord (hi b)) (coord v))
  b)


(defmethod move ((b box) (v vec2))
  (let ((c (* .5 (+ (coord (lo b))
	 	   (coord (hi b)))))
	(w (* .5 (- (coord (hi b))
		   (coord (lo b))))))
    (setf (coord (lo b)) (- (coord v) w))
    (setf (coord (hi b)) (+ (coord v) w)))
  b)

#+nil
(rmove (box 100 100 10 10) (vec2 10 10))


(defmethod draw ((b box))
  (format t "draw box ~a~%" b)
  (with-slots (lo hi) b
    (let ((x1 (floor (realpart (coord lo))))
	  (y1 (floor (imagpart (coord lo))))
	  (x2 (floor (realpart (coord hi))))
	  (y2 (floor (imagpart (coord hi)))))
      (draw-window x1 y1 x2 y1)
      (draw-window x2 y1 x2 y2)
      (draw-window x2 y2 x1 y2)
      (draw-window x1 y2 x1 y1)))
  (force-output pure-x11::*s*))

(defmethod dist ((b box) (p vec2))
  "0 when inside, positive when outside"
  (let ((d 0d0))
    (macrolet ((inc (component b p)
		 `(let ((pc (,component (coord ,p)))
			(bcl (,component (coord (lo ,b))))
			(bch (,component (coord (hi ,b)))))
		    (when (< pc bcl)
		      (incf d (expt (- pc bcl) 2)))
		    (when (< bch pc)
		      (incf d (expt (- pc bch) 2))))))
      (inc realpart b p)
      (inc imagpart b p))
    (sqrt d)))

#+nil
(dist (box 0 0 1 1) (vec2 0 .5))


(defclass/std observer ()
  ())

(defgeneric update (observer))

(printing-unreadably (lo hi)
		     (defclass/std button (box observer)
		       ((name))
		      ))
(defun button (cx cy w h &key name)
  (make-instance 'button
		 :name name 
		 :lo (vec2 (- cx (* .5 w))
			   (- cy (* .5 h)))
		 :hi (vec2 (+ cx (* .5 w))
			   (+ cy (* .5 h)))))

(defun vec2 (z )
  (declare (type (complex double-float) z))
  (vec2 (realpart z)
	(imagpart z)))

(vec2 (complex 0.0 1d0))

(defmethod center ((b button))
  (* .5 (+ (coord (lo b))
	   (coord (hi b)))))

(defmethod notify ((b button) (v vec2))
					;(format t "button ~a received update ~a" (name b) v)
  (if (< 0 (dist v (center b)))
      (pure-x11::clear-area)
      (move b v)
      (draw b)))

(printing-unreadably (observers)
		     (defclass/std subject ()
		       ((observers)
			(name))))

(defmethod attach ((s subject) (o observer))
  (push o (observers s))
  s)

(defmethod detach ((s subject) (o observer))
  (setf (observers s) (delete o (observers s)))
  s)

(printing-unreadably (pointer observers)
		     (defclass/std subject-rx (subject)
		       ((pointer :type vec2 :std (vec2 0d0 0d0)))))

(defmethod move ((s subject-rx) (v vec2))
  (setf (pointer s) v))

(defmethod move :after ((s subject-rx) (v vec2))
  (loop for o in (observers s) do
       (notify o v)))

#+nil
(let ((a (make-instance 'subject :name "subject-rx"))
      (b  (button 100 100 10 10  :name 1)))
  (attach a b)
  (detach a b))

;; p.22 Observer with :after method combination
;; http://norvig.com/design-patterns/design-patterns.pdf

;; Intent: When an object changes, notify all interested
;; Participants: Subject, Observer
;; Implementation:
;;    Subject: methods for attach/detach observer, notify
;;    Observer: method for update
;; use :after method combination



#+nil
(defun notify-after (fn)
  (eval `(defmethod ,fn :after (x)
		    (mapc #'notify (observers x)))))
#+nil
(mapc #'notify-after '(cut paste edit))

;; https://www.x.org/wiki/guide/debugging/
;; xrestop shows how much memory clients allocate in xserver

;; https://www.overleaf.com/blog/513-how-tex-calculates-glue-settings-in-an-slash-hbox
(connect :filename "/tmp/.X11-unix/X0")
#+nil (connect)
(make-window)
(draw-window 0 0 100 100)

(defparameter *mailbox-rx* (sb-concurrency:make-mailbox :name 'rx))

(sb-concurrency:list-mailbox-messages *mailbox-rx*)

(sb-thread:make-thread
 #'(lambda ()
     (loop while t do
	  (sb-concurrency:send-message *mailbox-rx* (pure-x11::read-reply-wait))))
 :name "rx-post")

(defparameter *subject-rx* (make-instance 'subject-rx :name "subject-rx"))

(sb-thread:make-thread
 #'(lambda ()
     (loop while t do
	  (let ((msg  (sb-concurrency:receive-message *mailbox-rx*)))
	    (case (aref msg 0)
	      (0 (format t "error ~{~2x ~}~%" (loop for e across msg
						 collect e)))
	      (1 (format t "reply ~{~2x ~}~%" (loop for e across msg
						 collect e)))
	      (6 ;; pointer moved
	       (multiple-value-bind (event-x event-y state) (pure-x11::parse-motion-notify msg)
		 (move *subject-rx* (vec2 event-x event-y))
		 (format t "motion ~a ~a ~{~2x ~}~%"
			 event-x event-y
			 (loop for e across msg
			    collect e))))
	      (t (format t "event ~{~2x ~}~%" (loop for e across msg
						 collect e))))
	    )))
 :name "rx-print")

(defparameter *button* (button 100 100 80 8))

(attach *subject-rx* *button*)


