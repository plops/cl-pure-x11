(in-package :pure-x11)

;; shm code
(defconstant +ipc-creat+ #o1000)
(defconstant +ipc-private+ 0) 

(sb-alien:define-alien-routine shmget 
    sb-alien:int
  (key sb-alien:int)
  (size sb-alien:long)
  (shmflg sb-alien:int))

(sb-alien:define-alien-routine shmat 
    (* sb-alien:unsigned-char)
  (shmid sb-alien:int)
  (shmaddr (* sb-alien:unsigned-char))
  (shmflg sb-alien:int))

(sb-alien:define-alien-routine shmdt 
    sb-alien:int
  (shmaddr (* sb-alien:unsigned-char)))

(defparameter *shm-id* nil)
(defparameter *at* nil)

(defun init-shm ()
  (setf *shm-id* (shmget +ipc-private+ 
			 (* 255 256 4) 
			 (logior +ipc-creat+ #o777))
	*at* (shmat *shm-id*
		    (sb-sys:int-sap 0)
		    0))
  (shm-attach *shm-id*))



#+nil
(shmdt *at*)

(defparameter *lut*
  (make-array 256
	      :element-type 'single-float))

(defun init-lut ()
  (setf (aref *lut* 0) 0s0)
  (loop for i from 1 below (length *lut*) do
       (setf (aref *lut* i) (log i))))
(defun log-lut (v)
  (declare ((unsigned-byte 8) v)
	   (values single-float &optional))
  (aref *lut* v))

(defun clamp (v)
  (cond ((< v 0s0) 0)
	((< 255s0 v) 255)
	(t (floor v))))

(defun clone-screen ()
  (let ((w 256)
	(h 255))
    (shm-get-image 30 80 w h)
    (sleep .02)
    (let* ((a (make-array (list h w 4)
			  :element-type '(unsigned-byte 8)))
	   (a1 (sb-ext:array-storage-vector a)))
      (sb-sys:with-pinned-objects (a a1)
	  (sb-impl::%byte-blt (sb-alien:alien-sap *at*) 0 
			      a1 0 (* 255 256 4)))
      (let* ((b (make-array (array-dimensions a)
			    :element-type 'single-float))
	     (b1 (sb-ext:array-storage-vector b))
	     (edge (make-array (array-dimensions a)
			       :element-type '(unsigned-byte 8)))
	     (spread (make-array (array-dimensions a)
			       :element-type '(unsigned-byte 8))))
	(dotimes (i (length a1))
	  (setf (aref b1 i) (log-lut (aref a1 i))
		#+nil(let ((v (aref a1 i))) 
			      (if (/= 0 v) 
				  (log v)
				  0s0))))
	(dotimes (k 3)
	 (loop for j from 1 below (1- h) do
	   (loop for i from 1 below (1- w) do
	     (dotimes (k 3)
	       (let ((v 0)
		     (mi -.2)
		     (ma .07))
		 (declare ((unsigned-byte 8) v))
		 (when (< mi 
			  (- (aref b j (1+ i) k)
			     (aref b j (1- i) k))
			  ma)
		    (setf v 1))
		 (when (< mi 
			  (- (aref b (1+ j) i k)
			     (aref b (1- j) i k))
			  ma)
		    (setf v (logior v 2)))
		 (setf (aref edge j i k) v))))))

	#+nil (dotimes (k 3)
	  (dotimes (j h)
	    (let ((current (floor (aref a j 0 k) 2)))
	     (dotimes (i w)
	       (when (= 1 (logand 1 (aref edge j i k)))
		   (setf current (floor (aref a j i k) 2)))
	       (setf (aref spread j i k) current)))))
	(dotimes (k 3)
	  (dotimes (i w)
	    (let ((current (floor (aref a 0 i k) 2)))
	     (dotimes (j h)
	       (when (= 1 (logand 2 (aref edge j i k)))
		   (setf current (floor (aref a j i k) 2)))
	       (incf (aref spread j i k) current)))))
	
	(put-image spread)))))


#+nil
(init-lut)

#+nil
(dotimes (i 100) 
   (clone-screen))


#+nil
(progn
  (connect)
  (parse-initial-response *resp*)
  (make-window)
  (init-shm))


;; X11/extensions/shmproto.h
(defun shm-attach (shmid)
  (setf *shmseg* (1+ *gc*))
  (with-packet
    (card8 139)				; shm req code
    (card8 1)				; shm req type
    (card16 4)				; length
    (card32 *shmseg*)			; shmseg e.g. #x2e0001
    (card32 shmid)	      		; shmid, result of shmget
    (card8 0)				; read only
    (card8 0)				; pad0
    (card16 0)				; pad1
    )
  (force-output *s*))

(defun shm-get-image (x y w h)
  (with-packet
    (card8 139)				; shm req code
    (card8 4)				; shm req type
    (card16 8)				; length
    (card32 *root*)			; drawable
       
    (card16 x)
    (card16 y)
    (card16 w) 
    (card16 h) 
    (card32 #x00ffffff)			; plane mask
    (card8 2)				; format = z
    (card8 0)				; pad0
    (card8 0)				; pad1
    (card8 0)				; pad2
    (card32 *shmseg*)			; shmseg
    (card32 0)				; offset
    )
  (force-output *s*))

