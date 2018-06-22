(ql:quickload :defclass-std)
(ql:quickload :pure-x11)

(defpackage :g
  (:use :cl :pure-x11))
(in-package :g)

;; nowadays X servers normally don't listen on port 6000
;; make it work without restarting
;; socat -d -d TCP-LISTEN:6000,fork,bind=localhost UNIX-CONNECT:/tmp/.X11-unix/X0
;; https://askubuntu.com/questions/41330/let-xorg-listen-on-tcp-but-only-to-localhost



(connect :filename "/tmp/.X11-unix/X0")
(make-window)
(draw-window 0 0 100 100)


(pure-x11::clear-area)
(draw-window 0 0 120 200)
(imagetext8 "hello" :x 100 :y 100)
(query-pointer)
(force-output pure-x11::*s*)
(dotimes (i 100)
  (sleep .1)
  (format nil "~a" (query-pointer)))

(pure-x11::read-reply-unknown-size)

(defparameter *b* (pure-x11::read-reply-unknown-size))
(pure-x11::parse-expose *b*)
*b*
(pure-x11::parse-motion-notify *b*)


(query-pointer)


;; first i have to sort out the socket handling i have listen
;; continuously for events and parse them. use a single thread that
;; listens and sends. use two mailboxes to communicate with the rest
;; of the program

;; then i need some polygon handling (point in polygon test)
;; https://cses.fi/book.pdf p. 269 geometry
;; and datastructures for many objects (quadree or kdtree)
;; p. 1097 in numerical recipes


;; p.22 Observer with :after method combination
;; http://norvig.com/design-patterns/design-patterns.pdf


;; https://www.x.org/wiki/guide/debugging/
;; xrestop shows how much memory clients allocate in xserver

;; https://www.overleaf.com/blog/513-how-tex-calculates-glue-settings-in-an-slash-hbox

