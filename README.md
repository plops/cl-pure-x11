<a id='x-28PURE-X11-3A-40PURE-X11-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Pure X11 manual

## Table of Contents

- [1 Examples][0857]
- [2 Internal details][17e9]

###### \[in package PURE-X11\]
This package provides a socket based lisp-only interface to X11. It
started as an experiment but as I added support for XPutImage using
BIG-REQUESTS it now seems more useful to me than CLX. 

Connect will send a request to open a connection to the X-Server and
parses its response to obtain the constants [`*RESOURCE-ID-BASE*`][d9cb],
[`*RESOURCE-ID-MASK*`][7637] and [`*ROOT*`][b419]. These are stored in dynamic variables
and are later used by other functions, e.g. by [`MAKE-WINDOW`][05e1] to create
a new window.

<a id='x-28PURE-X11-3ACONNECT-20FUNCTION-29'></a>

- [function] **CONNECT** *&KEY (IP #(127 0 0 1)) (PORT 6000)*

    Initiate the connection with the X server. Use little endian, parse
    the servers initial response to obtaining *root* and
    *resource-id-{base, mask}* (for creating new window identifiers). Enable big
    requests (which just means that for some requests you can send zero in
    the 16-bit length field and use an additional 32-bit length field for
    the request instead).

<a id='x-28PURE-X11-3AMAKE-WINDOW-20FUNCTION-29'></a>

- [function] **MAKE-WINDOW** *&KEY (WIDTH 512) (HEIGHT 512) (X 0) (Y 0)*

    Create a window with size `WIDTH` x `HEIGHT` at position (`X` `Y`) using
    *root*. The window id is generated using [`*RESOURCE-ID-BASE*`][d9cb] and
    [`*RESOURCE-ID-MASK*`][7637] and returned.

<a id='x-28PURE-X11-3ADRAW-WINDOW-20FUNCTION-29'></a>

- [function] **DRAW-WINDOW** *X1 Y1 X2 Y2*

    Draw a line from (x1 y1) to (x2 y2) in [`*WINDOW*`][11f6].

<a id='x-28PURE-X11-3AQUERY-POINTER-20FUNCTION-29'></a>

- [function] **QUERY-POINTER** 

    Ask the X server for the current cursor position. Returns the 4
    multiple values (values root-x root-y win-x win-y).

<a id='x-28PURE-X11-3APUT-IMAGE-BIG-REQ-20FUNCTION-29'></a>

- [function] **PUT-IMAGE-BIG-REQ** *IMG &KEY (DST-X 0) (DST-Y 0)*

    Create a PutImage request to write the 3D array `IMG` with
    dimensions (h w c) as a WxH image with 32 bits per pixel into [`*WINDOW*`][11f6]
    using `*GC*`.

<a id='x-28PURE-X11-3APARSE-INITIAL-REPLY-20FUNCTION-29'></a>

- [function] **PARSE-INITIAL-REPLY** *R*

    Extracts *root*, *resource-id-{base, mask}* from the first response
    of the server and stores into dynamic variables.

<a id='x-28PURE-X11-3AREAD-REPLY-WAIT-20FUNCTION-29'></a>

- [function] **READ-REPLY-WAIT** 

    The protocol specification states:
    
    Every reply contains a 32-bit length field expressed in units of
    four bytes. Every reply consists of 32 bytes followed by zero or
    more additional bytes of data, as specified in the length field.
    Unused bytes within a reply are not guaranteed to be zero. Every
    reply also contains the least significant 16 bits of the sequence
    number of the corresponding request. (this is implicitly assigned)
    
    This code first reads 32 bytes from the socket *s*. It parses the
    reply length and if necessary reads the rest of the reply packet.

<a id='x-28PURE-X11-3A-2AS-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*S\*** *NIL*

    Socket for communication with X server.

<a id='x-28PURE-X11-3A-2ARESOURCE-ID-BASE-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*RESOURCE-ID-BASE\*** *NIL*

<a id='x-28PURE-X11-3A-2ARESOURCE-ID-MASK-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*RESOURCE-ID-MASK\*** *NIL*

<a id='x-28PURE-X11-3A-2AROOT-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*ROOT\*** *NIL*

    Root ID as extracted from the initial reply of the X server.

<a id='x-28PURE-X11-3A-2AWINDOW-2A-20-28VARIABLE-29-29'></a>

- [variable] **\*WINDOW\*** *NIL*

<a id='x-28PURE-X11-3ABIG-REQUESTS-ENABLE-20FUNCTION-29'></a>

- [function] **BIG-REQUESTS-ENABLE** 

    If it hasn't been done so far, obtain the opcode for BIG-REQUESTS
    and then issue an enable request.

<a id='x-28PURE-X11-3A-40PURE-X11-EXAMPLES-20MGL-PAX-3ASECTION-29'></a>

## 1 Examples

Let's see the transcript of a real session of someone working
  with `PURE-X11`:

```common-lisp
(progn ;; open a window and draw a line
  (connect)
  (make-window)
  (draw-window 0 0 100 100))

(query-pointer) ;; ask for current mouse cursor position
;; => 700, 700, 302, -321
 
;; while a *WINDOW* is open, one can copy image data into it
;; like this:
(let*((w 512)
      (h 512)
      (c 4)
      (a (make-array (list h w c)
		     :element-type '(unsigned-byte 8))))
  (dotimes (j h)
    (dotimes (i w)
      (setf (aref a j i 0) (mod i 255)  ;; b
	    (aref a j i 1) (mod j 255)  ;; g
	    (aref a j i 2) 255          ;; r
	    (aref a j i 3) 255)))       ;; a
  (put-image-big-req a))
```


<a id='x-28PURE-X11-3A-40PURE-X11-INTERNAL-20MGL-PAX-3ASECTION-29'></a>

## 2 Internal details

I used <http://www.x.org/archive/X11R7.5/doc/x11proto/proto.pdf> as
  a reference to implement the X protocol. There are requests and
  replies. Requests are sent from the Lisp code to the X Server and
  replies are read back. I implemented several versions of functions
  for reading from the socket: blocking, non-blocking and one that
  uses SBCL internals to read everything that is currently in the
  buffer. Of those, I strive to only use the blocking [`READ-REPLY-WAIT`][866d],
  as this is the only one which will give robust code.

The packets as defined by the X protocol contain information stored
  in various types of which I currently support CARD{8,16,32} and
  `STRING8`. I use the macro `WITH-PACKET` to create a request and
  `WITH-REPLY` to parse a response. Both define a local function for
  each type that will write/return a properly constructed binary
  packet/parsed Common Lisp value to/from the stream while
  maintaining a counter to keep track of the currently written byte
  position.

The following function [`QUERY-POINTER`][c5c4] can act as a simple
  example. First, a query pointer request is constructed and sent using
  `WITH-PACKET`. Then the reply is read back using [`READ-REPLY-WAIT`][866d] and
  parsed inside the macro `WITH-REPLY`:

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


  [05e1]: #x-28PURE-X11-3AMAKE-WINDOW-20FUNCTION-29 "(PURE-X11:MAKE-WINDOW FUNCTION)"
  [0857]: #x-28PURE-X11-3A-40PURE-X11-EXAMPLES-20MGL-PAX-3ASECTION-29 "Examples"
  [11f6]: #x-28PURE-X11-3A-2AWINDOW-2A-20-28VARIABLE-29-29 "(PURE-X11:*WINDOW* (VARIABLE))"
  [17e9]: #x-28PURE-X11-3A-40PURE-X11-INTERNAL-20MGL-PAX-3ASECTION-29 "Internal details"
  [7637]: #x-28PURE-X11-3A-2ARESOURCE-ID-MASK-2A-20-28VARIABLE-29-29 "(PURE-X11:*RESOURCE-ID-MASK* (VARIABLE))"
  [866d]: #x-28PURE-X11-3AREAD-REPLY-WAIT-20FUNCTION-29 "(PURE-X11:READ-REPLY-WAIT FUNCTION)"
  [b419]: #x-28PURE-X11-3A-2AROOT-2A-20-28VARIABLE-29-29 "(PURE-X11:*ROOT* (VARIABLE))"
  [c5c4]: #x-28PURE-X11-3AQUERY-POINTER-20FUNCTION-29 "(PURE-X11:QUERY-POINTER FUNCTION)"
  [d9cb]: #x-28PURE-X11-3A-2ARESOURCE-ID-BASE-2A-20-28VARIABLE-29-29 "(PURE-X11:*RESOURCE-ID-BASE* (VARIABLE))"
