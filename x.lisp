(defpackage :x 
  (:use :cl :sb-bsd-sockets))
(in-package :x)

(defun connect ()
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
		     (format t "~a~%" (list 'visual visual-id
					    'class class
					    'colormap-entries colormap-entries))))))))))))

(parse-initial-response *resp*)

(let ((success (= 1 (aref *resp* 0)))
      (maximum-request-length (+ (ash (aref *resp* 7) 8)
				 (aref *resp* 6))))
  success)
; x11r7proto.pdf p.123 describes request formats
(let* ((create-gc-list '(55 ; opcode
			 0 ; unused
			 5 0 ; length
			 0 0 #xc2 2 ; cid
			 #x17 1 0 0 ; drawable
			 8 0 0 0 ; value-mask background
			 255 255 255 0 ; background

			 ;; 20 ; opcode get-property
			 ;; 0 ; delete
			 ;; 6 0 ; length
			 ;; #x17 1 0 0 ; window
			 ;; 23 0 0 0 ; resource-manager
			 ;; 31 0 0 0 ; get-property-type string
			 ;; 0 0 0 0 ; offset
			 ;; 0 #xe1 #xf5 5 ; length
			 )))
  (write-sequence 
   (make-array (length create-gc-list)
	       :element-type '(unsigned-byte 8)
	       :initial-contents create-gc-list)
   *s*)
  (force-output *s*))

(progn
  (connect)
  (parse-initial-response *resp*)
  (make-window)
  (draw-window))

(defun make-window ()
 (let* ((l ()) ;; first byte of request gets pushed in first
	(window (logior *resource-id-base* 
			(logand *resource-id-mask* 1)))
	(gc (logior *resource-id-base* 
			(logand *resource-id-mask* 2))))
   (defparameter *window* window)
   (defparameter *gc* gc)
   (labels ((card8 (a)
	      (push a l))
	    (card16 (a)
	      (dotimes (i 2)
		(push (ldb (byte 8 (* 8 i)) a) l)))
	    (card32 (a)
	      (dotimes (i 4)
		(push (ldb (byte 8 (* 8 i)) a) l))))
     (card8 1)	   ; opcode create-window
     (card8 0)	   ; depth
     (card16 13)   ; length
     (card32 window)		       ; wid
     (card32 *root*)		       ;parent
     (card16 101)		       ;x
     (card16 102)		       ;y
     (card16 201)		       ;w
     (card16 301)		       ;h
     (card16 1)			       ; border
     (card16 0)			       ; window-class copy-from-parent
     (card32 0)			       ; visual-id copy-from-parent
     (card32 #x281a) ; value-mask bg border bit-grav event-mask colormap
     (card32 0)	     ; bg
     (card32 #x00ffffff)	   ; border
     (card32 5)			   ; bit-grav center
     (card32 #x8004)		   ; event-mask button-press  exposure
     (card32 #x20)		   ;colormap
   
     (card8 55)				; opcode create-gc
     (card8 0)				; unused
     (card16 6)				; length
     (card32 gc)			; cid
     (card32 window)			; drawable
     (card32 #x0c)			; gc-value-mask fg bg
     (card32 #x00ffffff)		; fg
     (card32 0)				; bg
   
     (card8 8)				; opcode map-window
     (card8 0)				; unused
     (card16 2)				; length
     (card32 window)			; window

     (let ((buf (make-array (length l)
			    :element-type '(unsigned-byte 8)
			    :initial-contents (nreverse l))))
       (write-sequence buf *s*)
       (force-output *s*)
       ))))

(defun draw-window ()
 (let* ((l ()) ;; first byte of request gets pushed in first
	)
   (labels ((card8 (a)
	      (push a l))
	    (card16 (a)
	      (dotimes (i 2)
		(push (ldb (byte 8 (* 8 i)) a) l)))
	    (card32 (a)
	      (dotimes (i 4)
		(push (ldb (byte 8 (* 8 i)) a) l))))


     (card8 61)				; opcode clear-area
     (card8 0)				; exposures
     (card16 4)				; length
     (card32 *window*)			; window
     (card16 0)				; x
     (card16 0)				; y 
     (card16 0)				; w 
     (card16 0)				; h
   
     (let ((segs '((10 20 30 200))))
       (card8 66)			; opcode poly-segment
       (card8 0)			; unused
       (card16 (+ 3 (* 2 (length segs)))) ; length
       (card32 *window*)			  ; drawable
       (card32 *gc*)			  ; gc
       (dolist (s segs)
	 (dolist (p s)
	   (card16 p))))

     (let ((buf (make-array (length l)
			    :element-type '(unsigned-byte 8)
			    :initial-contents (nreverse l))))
       (write-sequence buf *s*)
       (force-output *s*)
       ))))


(let* ((create-win-list '(1    ; opcode
			  0   ; depth
			  13 0 ; length
			  #x0d 0 1 3 ; wid
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