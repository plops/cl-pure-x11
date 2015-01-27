(mgl-pax:define-package :pure-x11
  (:documentation "This package provides a socket based lisp-only
  interface to X11. It started as an experiment but as I added support
  for XPutImage using BIG-REQUESTS it now seems more useful to me than
  CLX. See PURE-X11:@PURE-X11-MANUAL.")
  (:use #:common-lisp #:sb-bsd-sockets #:mgl-pax))

(in-package :pure-x11)

(defsection @pure-x11-manual (:title "Pure X11 manual")
  "Connect will send a request to open a connection to the X-Server
and parses its response to obtain the constants *RESOURCE-ID-BASE*,
*RESOURCE-ID-MASK* and *ROOT*. These are stored in dynamic variables
and are later used by other functions, e.g. by MAKE-WINDOW to create a
new window."
  (connect function)
  (make-window function)
  (draw-window function)
  (query-pointer function)
  (put-image-big-req function)

  (parse-initial-reply function)
  (read-reply-wait function)

  (*s* variable)
  (*resource-id-base* variable)
  (*resource-id-mask* variable)
  (*root* variable)
  (*window* variable)
  
  (@pure-x11-examples section)
  (@pure-x11-internal section))

(defsection @pure-x11-examples (:title "Examples")
  "Let's see the transcript of a real session of someone working
  with PURE-X11:

  ```cl-transcrip
  (progn ;; open a window and draw a line
    (connect)
    (make-window)
    (draw-window 0 0 100 100))

  (query-pointer) ;; ask for current mouse cursor position
  => 700, 700, 302, -321
  ```")
