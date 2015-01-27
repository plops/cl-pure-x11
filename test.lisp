; (ql:quickload :mgl-pax)
(eval-when (:execute :load-toplevel :compile-toplevel)
  (setf asdf:*central-registry*
        '(*default-pathname-defaults*
          #p"/home/martin/stage/cl-pure-x11/"))
  (asdf:load-system "pure-x11"))


(defpackage :g (:use  :cl :pure-x11))
(in-package :g)

#+nil
(with-open-file (s "/home/martin/stage/cl-pure-x11/README.md" :direction :output :if-exists :supersede :if-does-not-exist :create)
  (describe @pure-x11-manual s))

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
  (make-window)
  (draw-window 0 0 100 100))

#+nil
(query-pointer)

#+nil
(draw-window 0 0 100 100)
#+nil
(read-reply)
#+nil
(progn
  (defparameter *my-resp* nil)
 (sb-thread:make-thread #'(lambda ()
			    (setf *my-resp* (list (get-universal-time)
						  (multiple-value-list (read-reply-wait)))))
			:name "bla"))


#+nil
(array-dimensions (second *my-resp*))
#+nil
(time
 (dotimes (i 100000)
   (draw-window (random 300) (random 300) (random 200) (random 300))))

#+nil
(query-pointer)
