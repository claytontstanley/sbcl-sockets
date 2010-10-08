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
	 (host ,host)
	 (port ,port))
     (setf (sb-bsd-sockets:non-blocking-mode sock) t)
     (sb-bsd-sockets::socket-bind sock (sb-bsd-sockets::make-inet-address host) port)
     (sb-bsd-sockets::socket-listen sock 5)
     (plambda () (sock host port)
       (aif (sb-bsd-sockets::socket-accept sock)
	    (values (sb-bsd-sockets::socket-make-stream it :input t :output t) it)
	    (values nil nil)))))

;;; uni-run-process
;;; This function takes 2 parameters.  The first is a string which will be
;;; used to name the process and the second is a function that is to be run
;;; in a new process.  It creates a new process that runs the specified 
;;; function (in the appropriate package) and returns that process.
(defun uni-run-process (name function)
  (sb-thread:make-thread #'(lambda ()
                       #+:packaged-actr (in-package :act-r)
                       (funcall function))
			 :name name))

;;; uni-process-kill
;;; This function takes one parameter which is a process and kills that
;;; process.
(defun uni-process-kill (process)
  (sb-thread:terminate-thread process))

;;; uni-process-system-events
;;; This function takes no parameters and calls the necessary function
;;; to process events - process-pending-events, event-dispatch, etc.  It
;;; gets called in a loop that's just waiting for a signal to exit so that
;;; ACT-R stops when the stepper is open. (do I need to do this in a non-ide
;;; Lisp???
(defun uni-process-system-events ()
  )

;;; uni-wait-for
;;; This function takes one parameter which is a function.  It waits until that
;;; function returns true before returning.
(defun uni-wait-for (function)
  (loop (uni-process-system-events)
        (when (funcall function)
          (return))))

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
   (write-string string socket)
   (finish-output socket)))


;;; uni-stream-closed
;;; This function takes one parameter which is a socket stream.  It
;;; should return t if that stream has been closed.  It's only really
;;; here for MCL right now because the way sockets get handled there doesn't 
;;; result in an error on a closed stream, thus the process waiting for
;;; input doesn't ever end. 
;;;
(defun uni-stream-closed (stream)
  (not (open-stream-p stream)))

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

;;; uni-report-error
;;; This function takes 2 parameters. The first is a condition and the second
;;; is a string.  It prints the string message followed by "Error:" and the
;;; information about the error in the condition to *error-output*.  
;;; If there isn't any error info in the condition then "unspecified error" 
;;; is displayed.
(defun uni-report-error (err message)
  (format sb-sys:*stderr* "~a~%Error:~a" message err))

;;; uni-wait-for-chars
;;; This function takes one parameter which is a socket.  It should wait until
;;; there is a character available on that stream before returning, or in
;;; the event of an error just close down the process associated with handling
;;; that socket stream.  This assumes that read-char blocks "well".
;;;
;;; In MCL the socket read locks the socket for output which isn't good -
;;; so I've got to resort to a polling loop until I figure out how to 'fix'
;;; that (or just wait until MCL and OS 9 are dead and ignore it...)
;;;
;;; I've resolved that problem with MCL, so the same function should
;;; work for all Lisps (at least all supported at this time).

(defun uni-wait-for-char (stream)
  ;; first make sure there's a connection
  (when (uni-stream-closed stream)
    (uni-report-error nil "Connection closed while waiting for a character.~%")
    (close stream)
    nil)
      
  ;; then check it for available data terminating on an error
  (multiple-value-bind (value condition)
      (ignore-errors (peek-char nil stream t)) 
    
    (declare (ignore value))
    
    (if (subtypep (type-of condition) 'condition)
        (progn
          (uni-report-error condition "Failed while waiting for a character")
          (close stream)
          nil)
      t)))

;;; functionify
;;; This function is needed by some Lisp implementations to coerce a lambda
;;; list to a function that can be funcalled (actually, upon further 
;;; investigation this is a case where ACL is "overly helpful" because the
;;; spec doesn't say a cons (lambda ...) should be coerced to a function, but
;;; it's doing it for me, though I probably shouldn't take advantage of it).
(defun functionify (x)
  (if (consp x) (coerce x 'function) x))