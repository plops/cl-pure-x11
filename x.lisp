;(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :pure-x11)

(defparameter *s* nil "Socket for communication with X server.")
(defparameter *resp* nil "Reply of the X server to a request")
(defparameter *root* nil "Root ID as extracted from the initial reply of the X server.")
(defparameter *window* nil)
(defparameter *gc* nil)
(defparameter *shmseg* nil)
(defparameter *resource-id-base* nil)
(defparameter *resource-id-mask* nil)

(defparameter *big-request-opcode* 133)

(defsection @pure-x11-internal (:title "Internal details")
  "I used <http://www.x.org/archive/X11R7.5/doc/x11proto/proto.pdf> as
  a reference to implement the X protocol. There are requests and
  replys. Requests are sent from the Lisp code to the X Server and
  replys are read back. I implemented several versions of functions
  for reading from the socket: blocking, non-blocking and one that
  uses SBCL interals to read everything that is currently in the
  buffer. Of those, I strive to only use the blocking READ-REPLY-WAIT,
  as this is the only one which will give robust code.

  The packets as defined by the X protocol contain information stored
  in various types of which I currently support CARD{8,16,32} and
  STRING8. I use the macro WITH-PACKET to create a request and
  WITH-REPLY to parse a response. Both define a local function for
  each type that will write/return a properly constructed binary
  packet/parsed Common Lisp value to/from the stream, while
  maintaining a counter to keep track of the currently written byte
  position.

  The following function QUERY-POINTER can act as a simple
  example. First a query pointer request is constructed and sent using
  WITH-PACKET. Then the reply is read back using READ-REPLY-WAIT and
  parsed inside the macro WITH-REPLY:

```common-lisp
(defun query-pointer ()
  (with-packet
    (card8 38)				; opcode
    (card8 0)				; unused
    (card16 2)				; length
    (card32 *window*)			; window
    )
  (with-reply (read-reply-wait)
    (let ((reply (card8))
	  (same-screen (card8))
	  (sequence-number (card16))
	  (reply-length (card32))
	  (root (card32))
	  (child (card32))
	  (root-x (card16))
	  (root-y (card16))
	  (win-x (card16))
	  (win-y (card16)))
      (values root-x root-y win-x win-y))))
```
")

(defmacro with-packet (&body body)
  "Write values into a list of bytes with card{8,16,32}. Finally all
the data is sent over the stream *s*."
  `(let* ((l ()))
     (labels ((string8 (a)
		(declare (type string a))
		(loop for i below (length a) do
		     (push (char-code (aref a i)) l)))
	      (card8 (a)
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

(defmacro with-reply (buf &body body)
  (let ((r (gensym)))
    `(let ((,r ,buf)
	   (current 0))
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
	 ,@body))))



(defun read-reply-wait ()
  "The protocol specifcation states:

Every reply contains a 32-bit length field expressed in units of
four bytes. Every reply consists of 32 bytes followed by zero or
more additional bytes of data, as specified in the length field.
Unused bytes within a reply are not guaranteed to be zero. Every
reply also contains the least significant 16 bits of the sequence
number of the corresponding request. (this is implicitly assigned)

This code first reads 32 bytes from the socket *s*. It parses the
reply length and if necessary reads the rest of the reply packet.
"
  (let* ((buf (make-array 32
			  :element-type '(unsigned-byte 8))))
    (sb-sys:read-n-bytes *s* buf 0 (length buf))
    (with-reply buf
      (let ((reply (card8))
	    (unused (card8))
	    (sequence-number (card16))
	    (reply-length (card32)))
	(declare (ignorable reply unused))
	(if (< 0 reply-length)
	    (let ((m (make-array (+ 32 (* 4 reply-length)) :element-type '(unsigned-byte 8))))
	      (dotimes (i 32)
		(setf (aref m i) (aref buf i)))
	      (sb-sys:read-n-bytes *s* m 32 (* 4 reply-length))
	      (values m sequence-number))
	    (values buf sequence-number))))))

(defun read-reply ()
  (cond ((not (listen *s*))
	 (error "timeout")
	 :timeout)

 	(t  (read-reply-wait))))

(defun read-reply-unknown-size ()
  (cond ((not (listen *s*))
	 (error "timeout")
	 :timeout)
 	(t  (let* ((n (sb-impl::buffer-tail (sb-impl::fd-stream-ibuf *s*))) ;; use sbcl internals to check how many bytes came in response
		   (buf (make-array n
				    :element-type '(unsigned-byte 8))))
	      (format t "~a~%" (list 'response-length n))
	      (sb-sys:read-n-bytes *s* buf 0 (length buf))
	      buf))))

(defun read-connection-response ()
  ;; Upon connection the server responds with one of three replies. The
  ;; first 8 bytes are similar in those. The card16 at index six
  ;; contains the length in 4-byte units
  (let ((buf (make-array 8 :element-type '(unsigned-byte 8))))
    (sb-sys:read-n-bytes *s* buf 0 (length buf))
    (with-reply buf
      (let ((success-state (card8))
	    (length-of-reason (card8))
	    (protocol-major-version (card16))
	    (protocol-minor-version (card16))
	    (reply-length (card16)))
	(format t "~a" (list success-state length-of-reason protocol-major-version protocol-minor-version))
	(let ((m (make-array (+ 8 (* 4 reply-length)) :element-type '(unsigned-byte 8))))
	  (dotimes (i 8)
	    (setf (aref m i) (aref buf i)))
	  (sb-sys:read-n-bytes *s* m 8 (* 4 reply-length))
	  (ecase success-state
	    (0 (error "failed"))
	    (2 (error "authenticate"))
	    (1 m)))))))

(defun connect ()
  "Initiate the connection with the X server. Use little endian, parse
the servers initial response to obtain *root* and
*resource-id-{base,mask}* (for creating new window ids). Enable big
requests (which just means that for some requests you can send zero in
the 16-bit length field and use an additional 32-bit length field for
the request instead)."
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
    (card8 #x6c)	       ; little endian
    (card8 0)		       ; unused
    (card16 11)		       ; major
    (card16 0)		       ; minor
    (card16 0)		       ; length of authorization protocol name
    (card16 0)		       ; length of authorization protocol data
    (card16 0)		       ; unused
    )
  (setf *resp* (read-connection-response))
  (parse-initial-reply *resp*)
  (big-requests-enable))

#+nil
(connect)

#+nil
(parse-initial-reply *resp*)

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



(defun parse-initial-reply (r)
  "Extracts *root*, *resource-id-{base,mask}* from first response of
the server and stores into dynamic variables."
  (with-reply r
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
					'colormap-entries colormap-entries)))))))))))






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




(defun make-window (&key (width 512) (height 512) (x 0) (y 0))
  "Create a window with size WIDTH x HEIGHT at position (X Y) using
*root*. The window id is generated using *RESOURCE-ID-BASE* and
*RESOURCE-ID-MASK* and returned."
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
      (card16 x)		       ;x
      (card16 y)		       ;y
      (card16 width)		       ;w
      (card16 height)		       ;h
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
      )
    window))

(defun draw-window (x1 y1 x2 y2)
  "Draw a line from (x1 y1) to (x2 y2) in *WINDOW*."
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
  "Ask the X server for the current cursor position. Returns the 4
multiple values (values root-x root-y win-x win-y)."
  (with-packet
    (card8 38)				; opcode
    (card8 0)				; unused
    (card16 2)				; length
    (card32 *window*)			; window
    )
  (with-reply (read-reply-wait)
    (let ((reply (card8))
	  (same-screen (card8))
	  (sequence-number (card16))
	  (reply-length (card32))
	  (root (card32))
	  (child (card32))
	  (root-x (card16))
	  (root-y (card16))
	  (win-x (card16))
	  (win-y (card16)))
      (values root-x root-y win-x win-y))))

(defun query-extension (name)
  "Query the X server for the string NAME. I use this to obtain the
opcode to enable BIG-REQUESTS."
  (declare (type string name)) ;; string should be latin iso 1 encoded
  (format t "query-extension ~a~%" name)
  (let ((n (length name)))
   (with-packet
     (card8 98)				; opcode
     (card8 0)				; unused
     (card16 (+ 2 (floor (+ n (pad n)) 4))) ; request length
     (card16 n) ; length of name
     (card16 0) ;; unused
     (string8 name))
   (dotimes (i (pad n))
     (write-byte 0 *s*)))
  (with-reply (read-reply-wait)
    (let ((reply (card8))
	  (unused (card8))
	  (sequence-number (card16))
	  (reply-length (card32))
	  (present (card8)) ;; it is actually BOOL
	  (major-opcode (card8))
	  (first-event (card8))
	  (first-error (card8)))
      major-opcode))) 



#+nil
(query-extension "BIG-REQUESTS")
                  
(defun big-requests-enable ()
  "If it hasn't been done so far, obtain the opcode for BIG-REQUESTS
and then issue an enable request."
  (unless *big-request-opcode*
    (setf *big-request-opcode* (query-extension "BIG-REQUESTS")))
  (with-packet
    (card8 *big-request-opcode*)				; opcode
    (card8 0)				; bigreq opcode
    (card16 1)				; length
    ))

(defun put-image-big-req (img &key (dst-x 0) (dst-y 0))
  "Create a PutImage request to write the 3D array IMG with
dimensions (h w c) as a WxH image with 32 bits per pixel into *WINDOW*
using *GC*."
  (declare (type (simple-array (unsigned-byte 8) 3) img)
	   (type (unsigned-byte 16) dst-x dst-y))
  (destructuring-bind (h w c)
      (array-dimensions img)
   (let*((img1 (sb-ext:array-storage-vector img))
	 (n (length img1))
	 (p (pad n))) 
     (with-packet
       (card8 72)			; opcode
       (card8 2)			; format Z-pixmap
       (card16 0)	                ; length=0 => this is a big request
       (card32 (+ 7 (/ (+ n p) 4)))
       (card32 *window*)		; window
       (card32 *gc*)
       (card16 w)
       (card16 h)
       (card16 dst-x) ; dst-x
       (card16 dst-y) ; dst-y
       (card8 0) ; left-pad
       (card8 24) ; depth
       (card16 0) ; unused
       )
     (write-sequence img1 *s*)
     (dotimes (i p)
       (write-byte 0 *s*))
     (force-output *s*))))



