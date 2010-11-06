(require 'sb-bsd-sockets)

;;; uni-make-socket
;;; This function takes 2 parameters which are the host and port address of
;;; a passive socket.  It should open an active socket to that address and
;;; return a stream for communicating over that connection.
;;; the created socket that the stream points to is returned as the second value
(defun uni-make-socket (host port)
  (let* ((sock (make-instance 'sb-bsd-sockets::inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets::socket-connect sock (sb-bsd-sockets::make-inet-address host) port)
    (let ((stream (sb-bsd-sockets::socket-make-stream sock :input t :output t :buffering :none)))
      (values stream sock))))

;;; uni-prepare-socket
;;; this is the server version of uni-make-socket; call uni-prepare-socket on the server
;;; call uni-make-socket on the client
(defmacro uni-prepare-socket (host port)
  `(let ((sock (make-instance 'sb-bsd-sockets::inet-socket :type :stream :protocol :tcp :buffering :none))
	 (host ,(symbol-name host))
	 (port ,port))
     (setf (sb-bsd-sockets:non-blocking-mode sock) t)
     (sb-bsd-sockets::socket-bind sock (sb-bsd-sockets::make-inet-address host) port)
     (sb-bsd-sockets::socket-listen sock 5)
     (define-job :name ,(symb `connect-socket- host `- port) 
       :Fn (plambda () (sock host port)
	     (awhen (sb-bsd-sockets::socket-accept sock) 
		    (format t "connecting~%")
		    (values (sb-bsd-sockets::socket-make-stream it :input t :output t) it)))
       :quota (* *updates-per-second* 1)))) ;have this plambda fire ~ once every second

;;; uni-without-interrupts
;;;
;;; A macro to abstract without-interrupts because SBCL puts it 
;;; in a different package.  This makes other module code which
;;; needs to use without-interrupts more portable.  It also allows
;;; for handling other Lisps in the future which may or may not
;;; have a without-interrupts available in the default package.
(defmacro uni-without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

;;; uni-send-string 
;;; This function takes two parameters the first is a socket stream
;;; and the second is a string of a message to send.  That message is
;;; printed down that stream with a "newline" (CR/LF or whatever is used
;;; by the system) after it and the stream is flushed so that the 
;;; line is actually sent.
(defun uni-send-string (socket string)
  (uni-without-interrupts 
   (write-string 
    (format nil "~a~%" (string-right-trim (list #\Newline #\Return #\LineFeed) string))
    socket)
   (finish-output socket)))

;;; uni-socket-read-line
;;; This function takes one parameter which is a stream and reads a 
;;; line from it (terminated by some sort of CR/LF depending on the system)
;;; and returns the string containing that line.
;;; I got this function from Scott because in MCL the line endings caused
;;; problems for him and I noticed similar problems.  The allegro version
;;; should work for other Lisps.
;;; I've moved to using the ccl provided function in MCL, and perhaps no
;;; longer need the complex function for the other systems, but for now
;;; I'll leave it.
(defun uni-socket-read-line (stream)
  "Read a line terminated by \\n"
  (read-line stream nil nil))
