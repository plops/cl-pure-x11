(mgl-pax:define-package :pure-x11
  (:documentation "This package provides a socket based lisp-only
  interface to X11. It started as an experiment but as I added support
  for XPutImage using BIG-REQUESTS it now seems more useful to me than
  CLX. See PURE-X11:@PURE-X11-MANUAL.")
  (:use #:common-lisp #:sb-bsd-sockets #:mgl-pax))

(defsection @pure-x11-manual (:title "Pure X11 manual")
  "Here you describe what's common to all the referenced (and
  exported) functions that follow. They work with *FOO-STATE*,
  and have a :RANDOM-STATE keyword arg. Also explain when to 
  choose which."
  (connect function)
  (make-window function)
  (draw-window function)
  (query-pointer function)
  (put-image-big-req function)
  (@pure-x11-examples section))

(defsection @pure-x11-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
  with PURE-X11:

  ```cl-transcript
  (progn ;; open a window and draw a line
    (connect)
    (make-window)
    (draw-window 0 0 100 100))

  (query-pointer) ;; ask for current mouse cursor position
  => 700, 700, 302, -321
  ```")


;; (defpackage :pure-x11
;;   (:use :cl )
;;   (:export
;;    #:connect
;;    #:parse-initial-reply
;;    #:query-extension
;;    #:query-pointer
;;    #:make-window
;;    #:draw-window
;;    #:put-image-big-req
;;    #:read-reply
;;    #:read-reply-wait
;; ))
