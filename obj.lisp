
(declaim (optimize (safety 0) (debug 3) (speed 0)))
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


#+nil
(connect :filename "/tmp/.X11-unix/X0")
(connect)
(make-window)
(draw-window 0 0 100 100)

(defparameter *mailbox-rx* (sb-concurrency:make-mailbox :name 'rx))

(sb-concurrency:list-mailbox-messages *mailbox-rx*)

(sb-thread:make-thread
 #'(lambda ()
     (loop while true do
	  (sb-concurrency:send-message *mailbox-rx* (pure-x11::read-reply-wait))))
 :name "rx-post")

(sb-thread:make-thread
 #'(lambda ()
     (loop while true do
	  (format t "~{~2x ~}~%" (loop for e across (sb-concurrency:receive-message *mailbox-rx*)
				  collect e))))
 :name "rx-print")

(pure-x11::clear-area)
(draw-window 0 0 120 200)
(imagetext8 "hello" :x 100 :y 100)
(query-pointer)
(force-output pure-x11::*s*)
(dotimes (i 100)
  (sleep .1)
  (format nil "~a" (query-pointer)))



(pure-x11::read-reply-unknown-size)
(sb-impl::ansi-stream-p pure-x11::*s*)
(defparameter *b* (pure-x11::read-reply-unknown-size))
(pure-x11::parse-expose *b*)
*b*
(pure-x11::parse-motion-notify *b*)


(query-pointer)


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


;; p.22 Observer with :after method combination
;; http://norvig.com/design-patterns/design-patterns.pdf

;; Intent: When an object changes, notify all interested
;; Participants: Subject, Observer
;; Implementation:
;;    Subject: methods for attach/detach observer, notify
;;    Observer: method for update
;; use :after method combination



(printing-unreadably
 (coord)
 (defclass/std vec2 ()
   ((coord :type (complex double-float)))))

(defun vec2 (x &optional y)
  (make-instance 'vec2 :coord (complex (* 1d0 x) (* 1d0 y))))

(vec2 1 2)

(defmethod dot ((a point) (b point))
  (* (coord b) (conjugate (coord a))))



(defclass/std box ()
  ((lo :type point)
   (hi :type point)))

(class/std button box)

(defun notify-after (fn)
  (eval `(defmethod ,fn :after (x)
		    (mapc #'notify (observers x)))))

(mapc #'notify-after '(cut paste edit))

;; https://www.x.org/wiki/guide/debugging/
;; xrestop shows how much memory clients allocate in xserver

;; https://www.overleaf.com/blog/513-how-tex-calculates-glue-settings-in-an-slash-hbox

