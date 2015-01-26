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
  (parse-initial-response *resp*)
  (query-extension "BIG-REQUESTS")
  (big-req-enable)

  (make-window)
  (draw-window 0 0 100 100))

#+nil
(draw-window 0 0 100 100)

#+nil
(time
 (dotimes (i 100000)
   (draw-window (random 300) (random 300) (random 200) (random 300))))

#+nil
(query-pointer)
