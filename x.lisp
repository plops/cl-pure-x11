(defpackage :x 
  (:use :cl :sb-bsd-sockets))
(in-package :x)

(defparameter *s* nil)
(defparameter *resp* nil)
(defparameter *root* nil)
(defparameter *window* nil)
(defparameter *gc* nil)
(defparameter *resource-id-base* nil)
(defparameter *resource-id-mask* nil)

(defmacro with-packet (&body body)
  "Write values into a list of bytes with card{8,16,32}. Finally all
the data is sent over the stream *s*."
  `(let* ((l ()))
     (labels ((card8 (a)
		(declare ((unsigned-byte 8) a))
		(push a l))
	      (card16 (a)
		(declare ((unsigned-byte 16) a))
		(dotimes (i 2)
		  (push (ldb (byte 8 (* 8 i)) a) l)))
	      (card32 (a)
		(declare ((unsigned-byte 32) a))
		(dotimes (i 4)
		  (push (ldb (byte 8 (* 8 i)) a) l))))
       ,@body
       (let ((buf (make-array (length l)
			      :element-type '(unsigned-byte 8)
			      :initial-contents (nreverse l))))
	 (write-sequence buf *s*)
	 (force-output *s*)))))

(defun connect ()
  (defparameter *s*
    (socket-make-stream (let ((s (make-instance 'inet-socket 
						:type :stream 
						:protocol :tcp)))
			  (socket-connect s #(127 0 0 1) 6000)
			  s)
			:element-type '(unsigned-byte 8)
			:input t
			:output t
			:buffering :none))
  (with-packet
    (card8 #x6c)	       ; LE
    (card8 0)		       ; unused
    (card16 11)		       ; major
    (card16 0)		       ; minor
    (card16 0)		       ; length of authorization protocol name
    (card16 0)		       ; length of authorization protocol data
    (card16 0)		       ; unused
    )
  (cond ((not (listen *s*))
	 (error "timeout")
	 :timeout)

	;; use sbcl internals to check how many bytes came in response
 	(t (let* ((n (sb-impl::buffer-tail (sb-impl::fd-stream-ibuf *s*)))
		  (buf (make-array n
				   :element-type '(unsigned-byte 8))))
	     (format t "~a~%" (list 'response-length n))
	     (sb-sys:read-n-bytes *s* buf 0 (length buf))
	     (defparameter *resp* buf)))))

#+nil
(connect)

#+nil
(parse-initial-response *resp*)

(defun pad (n)
  "difference to next number that is dividable by 4"
  (if (= 0 (mod n 4))
      0
      (- (* 4 (1+ (floor n 4)))
	 n)))
#+nil
(pad 7)
#+nil
(pad 8)

(defun parse-initial-response (r)
  "Extracts *root*, *resource-id-{base,mask}* from first response of
the server and stores into dynamic variables."
  (let ((current 0))
   (labels ((card8 ()
	      (prog1
		  (aref r current)
		(incf current)))
	    (card16 ()
	      (prog1
		  (+ (aref r current)
		     (* 256 (aref r (1+ current))))
		(incf current 2)))
	    (card32 ()
	      (prog1
		  (+ (aref r current)
		     (* 256 (+ (aref r (1+ current))
			       (* 256 (+ (aref r (+ 2 current))
					 (* 256 (aref r (+ 3 current))))))))
		(incf current 4)))
	    (string8 (n)
	      (prog1
		  (map 'string #'code-char (subseq *resp* current
						   (+ current n)))
		(incf current n)))
	    (inc-current (n)
	      (incf current n)))
       (let* ((success (card8))
	      (unused (card8))
	      (protocol-major (card16))
	      (protocol-minor (card16))
	      (length (card16))
	      (release (card32))
	      (resource-id-base (card32))
	      (resource-id-mask (card32))
	      (motion-buffer-size (card32))
	      (length-of-vendor (card16))
	      (maximum-request-length (card16))
	      (number-of-screens (card8))
	      (number-of-formats (card8))
	      (image-byte-order (card8))
	      (bitmap-format-bit-order (card8))
	      (bitmap-format-scanline-unit (card8))
	      (bitmap-format-scanline-pad (card8))
	      (min-keycode (card8))
	      (max-keycode (card8))
	      (unused2 (card32))
	      (vendor (string8 length-of-vendor))
	      (unused3 (inc-current (pad length-of-vendor)))
	      )
	 (unless (= 1 success)
	       (error "connection didn't succeed."))
	 (defparameter *resource-id-base* resource-id-base)
	 (defparameter *resource-id-mask* resource-id-mask)
	 (format t "~a~%" (list 'major protocol-major
				'minor protocol-minor
				'vendor vendor 'release release
				'resource-id-base resource-id-base
				'resource-id-mask resource-id-mask
				'motion-buffer-size motion-buffer-size
				'number-of-screens number-of-screens))
	 (dotimes (i number-of-formats)
	   (let ((depth (card8))
		 (bpp (card8))
		 (scanline-pad (card8))
		 (unused (inc-current 5)))
	     (format t "~a~%" (list 'format 'depth depth 'bpp bpp
				    'scanline-pad scanline-pad))))
	 (dotimes (i number-of-screens)
	   (let ((root (card32))
		 (default-colormap (card32))
		 (white-pixel (card32))
		 (black-pixel (card32))
		 (current-input-mask (card32))
		 (width (card16))
		 (height (card16))
		 (width-in-mm (card16))
		 (height-in-mm (card16))
		 (min-installed-maps (card16))
		 (max-installed-maps (card16))
		 (root-visual (card32))
		 (backing-stores (card8))
		 (save-unders (card8))
		 (root-depth (card8))
		 (number-of-allowed-depths (card8)))
	     (defparameter *root* root)
	     (format t "~a~%" (list 'root root
				    'w width
				    'h height
				    'white white-pixel
				    'black black-pixel
				    'current-input-mask current-input-mask
				    'backing-stores backing-stores
				    'save-unders save-unders
				    'root-depth root-depth
				    'number-of-allowed-depths 
				    number-of-allowed-depths))
	     (dotimes (i number-of-allowed-depths)
	       (let ((depth (card8))
		     (unused (card8))
		     (number-of-visuals (card16))
		     (unused2 (card32))
		     )
		 (format t "~a~%" (list 'allowed-depth depth
					'nr-visuals number-of-visuals))
		 (dotimes (j number-of-visuals)
		   (let ((visual-id (card32))
			 (class (card8))
			 (bits-per-rgb (card8))
			 (colormap-entries (card16))
			 (red-mask (card32))
			 (green-mask (card32))
			 (blue-mask (card32))
			 (unused (card32)))
		     (format t "~a~%" (list 'visual j 'id visual-id
					    'class class
					    'colormap-entries colormap-entries))))))))))))


(defmacro with-reply (r &body body)
  `(let ((current 0))
     (labels ((card8 ()
		(prog1
		    (aref ,r current)
		  (incf current)))
	      (card16 ()
		(prog1
		    (+ (aref ,r current)
		       (* 256 (aref ,r (1+ current))))
		  (incf current 2)))
	      (int16 ()
		(let ((v (card16)))
		  (if (< v (ash 1 15)) ;; -128 in 8bit: (- (ash 1 7) (ash 1 8))
		      v
		      (- v (ash 1 16)))))
	      (card32 ()
		(prog1
		    (+ (aref ,r current)
		       (* 256 (+ (aref ,r (1+ current))
				 (* 256 (+ (aref ,r (+ 2 current))
					   (* 256 (aref ,r (+ 3 current))))))))
		  (incf current 4)))
	      (string8 (n)
		(prog1
		    (map 'string #'code-char (subseq ,r current
						     (+ current n)))
		  (incf current n)))
	      (inc-current (n)
		(incf current n)))
       ,@body)))



#+nil
(let ((buf (make-array 31 :element-type '(unsigned-byte 8))))
  (prog1
      (read-sequence buf *s*)
    (defparameter *buf* buf)))
#+nil
(with-reply *buf*
  (let*((same-screen (card8))
	(sequence-number (card16))
	(reply-length (card32))
	(root (card32))
	(child (card32))
	(root-x (int16))
	(root-y (int16))
	(win-x (int16))
	(win-y (int16))
	(mask (card16))
	(unused (inc-current 6)))
    (format t "~a~%" (list 'sequence-number sequence-number
			   'root root
			   'child child
			   'root-xy (list root-x root-y)
			   'win-xy (list win-x win-y)
			   'mask mask))))


; x11r7proto.pdf p.123 describes request formats




(defun make-window ()
  (let* ((window (logior *resource-id-base* 
			 (logand *resource-id-mask* 1)))
	 (gc (logior *resource-id-base* 
		     (logand *resource-id-mask* 2))))
    (defparameter *window* window)
    (defparameter *gc* gc)
    (with-packet 
      (card8 1)			       ; opcode create-window
      (card8 0)			       ; depth
      (card16 13)		       ; length
      (card32 window)		       ; wid
      (card32 *root*)		       ;parent
      (card16 101)		       ;x
      (card16 102)		       ;y
      (card16 201)		       ;w
      (card16 301)		       ;h
      (card16 1)		       ; border
      (card16 0)		       ; window-class copy-from-parent
      (card32 0)		       ; visual-id copy-from-parent
      (card32 #x281a) ; value-mask bg border bit-grav event-mask colormap
      (card32 0)      ; bg
      (card32 #x00ffffff)	   ; border
      (card32 5)		   ; bit-grav center
      (card32 #x8004)		   ; event-mask button-press  exposure
      (card32 #x20)		   ;colormap
   
      (card8 55)			; opcode create-gc
      (card8 0)				; unused
      (card16 6)			; length
      (card32 gc)			; cid
      (card32 window)			; drawable
      (card32 #x0c)			; gc-value-mask fg bg
      (card32 #x00ffffff)		; fg
      (card32 0)			; bg
   
      (card8 8)				; opcode map-window
      (card8 0)				; unused
      (card16 2)			; length
      (card32 window)			; window
      )))

(defun draw-window (x1 y1 x2 y2)
  (declare ((unsigned-byte 16) x1 y1 x2 y2))
  (with-packet
    (card8 61)				; opcode clear-area
    (card8 0)				; exposures
    (card16 4)				; length
    (card32 *window*)			; window
    (card16 0)				; x
    (card16 0)				; y 
    (card16 0)				; w 
    (card16 0)				; h
   
    (let ((segs (list (list x1 y1 x2 y2))))
      (card8 66)			  ; opcode poly-segment
      (card8 0)				  ; unused
      (card16 (+ 3 (* 2 (length segs))))  ; length
      (card32 *window*)			  ; drawable
      (card32 *gc*)			  ; gc
      (dolist (s segs)
	(dolist (p s)
	  (card16 p))))))


(defun query-pointer ()
  (with-packet
    (card8 38)				; opcode
    (card8 0)				; unused
    (card16 2)				; length
    (card32 *window*)			; window
    ))

(defun put-image (img)
  (declare ((simple-array (unsigned-byte 8) 2) img))
  (destructuring-bind (h w)
      (array-dimensions img)
   (let*((img1 (sb-ext:array-storage-vector img))
	 (n (length img1))
	 (p (pad n))) 
     (with-packet
       (card8 72)			; opcode
       (card8 2)			; format Z-pixmap
       (card16 (+ 6 (/ (+ n p) 4)))	; length
       (card32 *window*)		; window
       (card32 *gc*)
       (card16 (/ w 4))
       (card16 h)
       (card16 0) ; dst-x
       (card16 0) ; dst-y
       (card8 0) ; left-pad
       (card8 24) ; depth
       (card16 0) ; unused
       )
     (write-sequence img1 *s*)
     (dotimes (i p)
       (write-byte 0 *s*))
     (force-output *s*))))

#+nil
(let*((w 12)
      (h 12)
      (c 4)
      (a (make-array (list h (* w c))
		     :element-type '(unsigned-byte 8))))
  (dotimes (j h)
    (dotimes (i w)
      (setf (aref a j (+ 0 (* c i))) i
	    (aref a j (+ 1 (* c i))) j
	    (aref a j (+ 2 (* c i))) 255)))
  (put-image a))

#+nil
(progn
  (connect)
  (parse-initial-response *resp*)
  (make-window)
  (draw-window 0 0 100 100))

#+nil
(time
 (dotimes (i 100000)
   (draw-window (random 300) (random 300) (random 200) (random 300))))

#+nil
(query-pointer)
