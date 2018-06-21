(ql:quickload :defclass-std)
(ql:quickload :pure-x11)

(defpackage :g
  (:use :cl :pure-x11))
(in-package :g)

;; nowadays X servers normally don't listen on port 6000
;; make it work without restarting
;; socat -d -d TCP-LISTEN:6000,fork,bind=localhost UNIX-CONNECT:/tmp/.X11-unix/X0
;; https://askubuntu.com/questions/41330/let-xorg-listen-on-tcp-but-only-to-localhost

(connect)
(make-window)
(draw-window 0 0 100 100)


(pure-x11::clear-area)
(draw-window 0 0 120 200)
(imagetext8 "hello" :x 100 :y 100)
(dotimes (i 100)
  (sleep .1)
  (format nil "~a" (query-pointer)))

(pure-x11::read-reply-unknown-size)

(defparameter *b* (pure-x11::read-reply-unknown-size))
(pure-x11::parse-expose *b*)
*b*
(pure-x11::parse-motion-notify *b*)


(query-pointer)


;; p.22 Observer with :after method combination
;; http://norvig.com/design-patterns/design-patterns.pdf


;; https://www.x.org/wiki/guide/debugging/
;; xrestop shows how much memory clients allocate in xserver
