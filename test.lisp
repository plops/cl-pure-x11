
(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
        '(*default-pathname-defaults*
          #p"/home/martin/stage/cl-pure-x11/"))
  (asdf:load-system "pure-x11"))

(defpackage :g (:use  :cl :pure-x11))
(in-package :g)

#+nil
(let*((w 512)
      (h 512)
      (c 4)
      (a (make-array (list h w c)
		     :element-type '(unsigned-byte 8))))
  (dotimes (j h)
    (dotimes (i w)
      (setf (aref a j i 0) (mod i 255)	  ;; b
	    (aref a j i 1) (mod j 255)		  ;; g
	    (aref a j i 2) 255 ;; r 
	    (aref a j i 3) 255))) ;; a
  (put-image-big-req a))

#+nil
(progn
  (connect)
  (parse-initial-response pure-x11::*resp*)
  (query-extension "BIG-REQUESTS")
  (big-req-enable)

  (make-window)
  (draw-window 0 0 100 100))

#+nil
(draw-window 0 0 100 100)
#+nil
(read-response)
#+nil
(progn
  (defparameter *my-resp* nil)
 (sb-thread:make-thread #'(lambda ()
			    (setf *my-resp* (list (get-universal-time)
						  (read-response-wait))))
			:name "bla"))

;; Every reply contains a 32-bit length field expressed in units of
;; four bytes. Every reply consists of 32 bytes followed by zero or
;; more additional bytes of data, as specified in the length field.
;; Unused bytes within a reply are not guaranteed to be zero. Every
;; reply also contains the least sig- nificant 16 bits of the sequence
;; number of the corresponding request.

#+nil
(array-dimensions (second *my-resp*))
#+nil
(time
 (dotimes (i 100000)
   (draw-window (random 300) (random 300) (random 200) (random 300))))

#+nil
(query-pointer)
