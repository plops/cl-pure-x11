(ql:quickload :defclass-std)
(ql:quickload :pure-x11)

(defpackage :g
  (:use :cl :pure-x11))
(in-package :g)

;; nowadays X servers normally don't listen on port 6000
;; make it work without restarting
;; socat -d -d TCP-LISTEN:6000,fork,bind=localhost UNIX-CONNECT:/tmp/.X11-unix/X0
;; https://askubuntu.com/questions/41330/let-xorg-listen-on-tcp-but-only-to-localhost

(progn ;; open a window and draw a line
  (connect)
  (make-window)
  (draw-window 0 0 100 100))

(query-pointer)


;; p.22 Observer with :after method combination
;; http://norvig.com/design-patterns/design-patterns.pdf


;; https://www.x.org/wiki/guide/debugging/
;; xrestop shows how much memory clients allocate in xserver
