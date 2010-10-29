(defpackage :x 
  (:use :cl :sb-bsd-sockets))
(in-package :x)

(progn 
  (defparameter *s*
    (socket-make-stream
     (let ((s (make-instance 'inet-socket :type :stream :protocol :tcp)))
       (socket-connect s #(127 0 0 1) 6000)
       s)
     :element-type '(unsigned-byte 8)
     :input t
     :output t
     :buffering :none))
  (let*((req-l (list #x6c ;; LE
		     0	  ;; unused
		     11 0 ;; major
		     0 0  ;; minor
		     0 0 ;; authorization name could be "MIT-MAGIC-COOKIE-1"
		     0 0 ;; authorization data should be cookie from .Xauthority
		     0 0 ;; unused
		     ))
	(req (make-array (length req-l)
			 :element-type '(unsigned-byte 8)
			 :initial-contents req-l)))
    (write-sequence req *s*)
    (force-output *s*))
  (cond ((not (listen *s*))
	 (defparameter *resp* nil)
	 :timeout)
	(t (let ((buf (make-array (1+ (- #x7d5 #x42)) 
				  :element-type '(unsigned-byte 8))))
	     (sb-sys:read-n-bytes *s* buf 0 (length buf))
	     (defparameter *resp* buf)))))

(let ((success (= 1 (aref *resp* 0)))
      (maximum-request-length (+ (ash (aref *resp* 7) 8)
				 (aref *resp* 6))))
  success)

(let* ((create-win-list '(1    ; opcode
			  24   ; depth
			  13 0 ; length
			  #x0d 0 0 3 ; wid
			  #x17 1 0 0; parent
			  0 0 ; x
			  0 0 ; y
			  1 0 ; w
			  1 0 ; h
			  1 0 ; border-width
			  1 0 ; input-output window-class
			  0 0 0 0 ; copy-from-parent
			  #x1a #x28 0 0 ; value-mask
			  255 255 255 0 ; bg pixel
			  0 0 0 0 ; border pixel
			  1 ; bit-gravity north-west
			  0 0 0 ; unused
			  0 0 #x62 0 ; event-mask
			  #x20 0 0 0 ; colormap
			  ))
       (create-window (make-array (length create-win-list)
				  :element-type '(unsigned-byte 8)
				  :initial-contents create-win-list)))
  (write-sequence create-window *s*)
  (force-output *s*))

#+nil
((major-op-code 1
length-in-4-bytes 2
data 1)
create-window->opcode=1
 wid 8
parent 8 #x117
class 1
depth 1
visual-id 8
x 2
y 2
width 2
height 2
border-width 2
value-mask ?
value-list n*4)