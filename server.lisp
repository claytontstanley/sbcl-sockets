;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;; 
;;; Author      : Clayton Stanley
;;; Address     : Air Force Research Laboratory
;;;             : Mesa, AZ 85212 USA
;;;             : clayton.stanley@wpafb.af.mil
;;;             : cstanley@cstanley.no-ip.biz
;;; Filename    : server.lisp
;;; Version     : 1.0
;;; 
;;; Description : A socket-based server; this is a synchronous, non-threaded implementation
;;;               Asynchronous behavior and threads are achieved by using a discreet event simulator
;;;             
;;; Dependencies: requires that letf.lisp is loaded
;;;
;;; Bugs        : ???
;;;
;;; ----- History -----
;;;
;;; 2010.10.11  : Creation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro! acond (&rest clauses)
 "works just like cond, but stores the 
  value of each condition as 'it', which is accessable in the code
  following the condition"
    (if clauses
	(let ((cl1 (car clauses)))
	  `(let ((,g!sym ,(car cl1)))
	     (if ,g!sym
		 (let ((it ,g!sym)) 
		   (declare (ignorable it)) 
		   ,@(cdr cl1))
		 (acond ,@(cdr clauses)))))))

(defmacro! square (o!x)
  "squares something"
  `(* ,g!x ,g!x))

(defmacro with-gensyms ((&rest names) &body body)
  "paul graham's with-gensyms"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro with-outputs-to-string (args &body body)
  "works like with-output-to-string, except that you can set/return a string bound to multiple streams"
  (destructuring-bind (vars &rest rest) args
    (if vars
	`(with-output-to-string (,(car vars) ,@rest)
	   (with-outputs-to-string (,(cdr vars) ,@rest) ,@body))
	`(progn
	   ,@body))))

(defun linear-regression (xs ys)
  "returns the best-fitting slope & intercept for the x,y points"
  (let* ((x-bar (mean xs))
	 (y-bar (mean ys))
	 (Lxx (reduce #'+ (mapcar (lambda (xi) (square (- xi x-bar))) xs)))
	 ;(Lyy (reduce #'+ (mapcar (lambda (yi) (square (- yi y-bar))) ys)))
	 (Lxy (reduce #'+ (mapcar (lambda (xi yi) (* (- xi x-bar) (- yi y-bar)))
				  xs ys)))
	 (b (/ Lxy Lxx))
	 (a (- y-bar (* b x-bar))))
    ;slope then intercept
    (values b a)))

(defun parse-float (string) 
  "Return a float read from string, and the index to the remainder of string." 
  (multiple-value-bind (integer i) 
      (parse-integer string :junk-allowed t)
    (if (and (null integer) (eq i 0)) (setf integer 0))
    (multiple-value-bind (fraction j)
	(if (eq (length string) i)
	    (values 0 i)
	    (parse-integer string :start (+ i 1) :junk-allowed nil))
      (values (coerce (+ integer (/ fraction (expt 10 (- j i 1)))) 'double-float) j))))

(defun trim-data (data N)
  "trims the ends off all channels in the data hash table, so that no channel is > N in length"
  (loop for channel being the hash-values of data do 
       (when (listp channel)
	 (let ((Ln (+ 1 (length channel)))) 
	   (while (> (decf Ln) N)
	     (setf channel (nbutlast channel)))))))

(defpun headroom (val) ((hsh (make-hash-table)) (N 1000))
  (trim-data hsh N)
  (push val (gethash 'headroom hsh)))

(defmacro print-headroom (&key (strm t))
  `(with-pandoric (hsh) 'headroom
     (aif (gethash 'headroom hsh)
	  (format ,strm "headroom: ~,8f seconds, ~,8f percent~%"
		  (mean it) (* 100 (/ (mean it) *seconds-per-update*))))))

(define-symbol-macro headroom
    (print-headroom :strm nil))

(defmacro! with-time (time &body body)
  "executes body in 'time' seconds; time is intended to be longer than it should take to execute body"
  `(let* ((,g!start (get-internal-real-time))
	  (,g!finish (+ (* ,time internal-time-units-per-second) ,g!start))
	  (,g!time-to-sleep))
     ,@body
     (setf ,g!time-to-sleep (/ (- ,g!finish (get-internal-real-time)) internal-time-units-per-second))
     (headroom ,g!time-to-sleep)
     (if (< ,g!time-to-sleep 0)
	 (format t "lagging behind by ~a seconds~%" (coerce (abs ,g!time-to-sleep) 'double-float))
	 (sleep ,g!time-to-sleep))))

(defmacro define-job (&key (name) (Fn) (active-p t) (quota 1))
  "discrete event simulator that executes functions at the specified time, 
   when active, you can pass functions to execute after each time the quota is reached
   the functioned returned is pandoric, so you can access/change state variables"
  `(plambda () ((name ',name)
		(quot 0)
		(quota ,quota)
		(active ,active-p)
		(Fn ,Fn))
     (when active
       (incf quot)
       (when (eq quot quota)
	 (setf quot 0)
	 (attempt (funcall Fn))))))

(defmacro add-job (&key (job) (agent))
  "adds job to agent; errors if already present"
  (let ((name (guard (second (member :name (flatten job)))
		     (assert (car it) nil "job form ~a does not have a name~%" job))))
    `(progn
       (assert (not (job-present-p ,name ,agent)) nil "tried to add job ~a that is already present~%" ',name)
       (with-pandoric (jobs) ',agent
	 (setf (gethash ',name jobs) ,job)))))

(defmacro remove-job (name agent)
  "removes job from agent; errors if not already present"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to remove job ~a that is not present~%" ',name)
     (with-pandoric (jobs) ',agent
       (remhash ',name jobs))))

(defmacro job-present-p (name agent)
  "if job is present in agent"
  `(with-pandoric (jobs) ',agent
     (key-present ',name jobs)))

(defmacro activate-job (name agent)
  "activates job from agent; errors if not present, or present and already active"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to activate job ~a that is not present" ',name)
     (with-pandoric (jobs) ',agent
       (with-pandoric (active) (gethash ',name jobs)
	 (assert (not active) nil "tried to activate job ~a that is already activated~%" ',name)
	 (setf active t)))))

(defmacro deactivate-job (name agent)
  "deactivates job from agent; errors if not present, or present and not already active"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to deactivate job ~a that is not present" ',name)
     (with-pandoric (jobs) ',agent
       (with-pandoric (active) (gethash ',name jobs)
	 (assert active nil "tried to deactivate job ~a that is already deactivated~%" ',name)
	 (setf active nil)))))

(defmacro job-active-p (name agent)
  "returns t/nil if job from agent is active; errors if not present"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to check active flag for job ~a that is not present" ',name)
     (with-pandoric (jobs) ',agent
       (get-pandoric (gethash ',name jobs) 'active))))

(defmacro run-job (name agent)
  "short circuits through the quota run-jobs loop, and just executes the job once"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to run job ~a that is not present~%" ',name)
     (with-pandoric (jobs) ',agent
       (with-pandoric (Fn) (gethash ',name jobs)
	 (funcall Fn)))))

;stores the refresh rate of various loops in the system
(defvar *updates-per-second* 60) 
(defvar *seconds-per-update* (/ 1 *updates-per-second*))

(defmacro! updates/second->quota (o!updates/second)
  `(progn
     (assert (<= ,g!updates/second *updates-per-second*) nil 
	     "updates/second at ~a not less than *updates-per-second* at ~a" ,g!updates/second *updates-per-second*)
     (aif ,g!updates/second (/ *updates-per-second* it))))

;////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////
;stuff to make sending/receiving/connecting across sockets easier
;borrowed heavily from uni-files.lisp in the actr6 distribution
#+:sbcl (require 'sb-bsd-sockets)

(defmacro uni-stream-active-p (stream)
  `(aif ,stream (open-stream-p it)))

#+:sbcl
(defmacro uni-client-socket (host port)
  "opens an active socket on host:port; client-side function
   returns a pandoric function that, when called
   attempts to open and then return a stream and socket for communicating over the connection"
  `(define-job :name ,(symb `connect- host `- port)
     :Fn (plambda () ((host ,host) 
		      (port ,port))
	   (block try
	     (let ((sock (make-instance 'sb-bsd-sockets::inet-socket :type :stream :protocol :tcp)))	 
	       (attempt (sb-bsd-sockets::socket-connect sock (sb-bsd-sockets::make-inet-address ,host) ,port)
			:on-error (progn
				    (format t "~a~%" condition) 
				    (sb-bsd-sockets:socket-close sock)
				    (return-from try (values nil nil))))
	       (format t "connecting ~a:~a~%" host port)
	       (let ((stream (sb-bsd-sockets::socket-make-stream sock :input t :output t :buffering :none)))
		 (values stream sock)))))
     :quota (updates/second->quota 1))) ;have this plambda fire ~ once every second

#+:ccl
(defmacro uni-client-socket (host port)
  `(define-job :name ,(symb `connect- host `- port)
     :Fn (plambda () ((host ,host)
		      (port ,port))
	   (values (make-socket :remote-host host :remote-port port) nil))
     :quota (updates/second->quota 1)))

(defun uni-connect (host port)
  "attempts to connect to socket on host:port; client-side function
   will hang until a connection is established
   returns a stream and socket for communicating over the connection"
  (let ((uni-client-socket-fn (uni-client-socket host port))
	(bsd-stream) (bsd-socket))
    (while (and (not bsd-stream) (not bsd-socket))
      (with-time *seconds-per-update*
	(multiple-value-setq (bsd-stream bsd-socket) (funcall uni-client-socket-fn))))
    (values bsd-stream bsd-socket)))
  
#+:sbcl
(defmacro! uni-server-socket (host port)
  "opens an active socket on host:port; server-side function
   returns a pandoric function that, when called
   attempts to open and then return a stream and socket for communicating over the connection"
  `(let ((sock)
	 (host ,host)
	 (port ,port)
	 (,g!init-sock))
     (setf ,g!init-sock (lambda ()
			  (setf sock (make-instance 'sb-bsd-sockets::inet-socket :type :stream :protocol :tcp :buffering :none))
			  (setf (sb-bsd-sockets:non-blocking-mode sock) t)
			  (sb-bsd-sockets::socket-bind sock (sb-bsd-sockets::make-inet-address host) port)
			  (sb-bsd-sockets::socket-listen sock 5)
			  nil))
     (funcall ,g!init-sock)
     (define-job :name ,(symb `accept- host `- port) 
       :Fn (plambda () (sock host port)
	     (format t "listening with sock/host/port ~a/~a/~a~%" sock host port)
	     (attempt
	      (awhen (sb-bsd-sockets::socket-accept sock) 
		(format t "connecting ~a:~a~%" host port)
		(values (sb-bsd-sockets::socket-make-stream it :input t :output t) it))
	      :on-error (funcall ,g!init-sock)))
       :quota (updates/second->quota 1)))) ;have this plambda fire ~ once every second

#+:ccl
(defmacro uni-server-socket (host port)
  `(let ((sock (make-socket :local-host host :local-port port :connect :passive)))
     (define-job :name ,(symb `connect- host `- port)
       :Fn (plambda () ((host ,host)
			(port ,port))
	     (awhen (accept-connection sock :wait nil)
	       (format t "connecting ~a:~a~%" host port)
	       (values it nil)))
       :quota (updates/second->quota 1))))

#+:sbcl
(defmacro uni-without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

#+:ccl
(defmacro uni-without-interrupts (&body body)
  `(without-interrupts ,@body))

(defun uni-send-string (socket string)
  "sends string down socket; string will have a newline appended if not present already"
  (uni-without-interrupts 
   (write-string 
    (format nil "~a~%" (string-right-trim (list #\Newline #\Return #\LineFeed) string))
    socket)
   ;flush socket so that the string is guaranteed to be sent now
   (finish-output socket)))

(defmacro uni-read-line (stream)
  "Read a line terminated by \\n"
  `(read-line ,stream nil nil))

#+:sbcl
(defun uni-close (bsd-stream &optional bsd-socket)
  (if bsd-stream (sb-bsd-sockets::close bsd-stream))
  (if bsd-socket (sb-bsd-sockets:socket-close bsd-socket)))

#+:ccl
(defun uni-close (bsd-stream &optional bsd-socket)
  (assert (null bsd-socket) nil "ccl only returns a stream; socket should be nil here")
  (close bsd-stream))
  
(defmacro read-registers (strm)
  "macro builder for read-registers function
   returns a pandoric lexical closure that, when called, will ping the server (through the socket)
   to request current values at each register provided as an argument; returns floating-point
   values for each register"
  `(plambda (&rest registers) ((strm ,strm))
     ;stub
     (car registers)))

(defmacro write-registers (strm)
  "macro builder for write-registers function
   returns a pandoric lexical closure that, when called, will send the floating-point values
   provided as arguments down the socket, to arrive at the array of registers (designated by registerStart)
   on the DAQ"
  `(plambda (registerStart &rest vals) ((strm ,strm))
     (declare (ignorable vals registerStart))
     ;stub
     nil))

;////////////////////////////////////////////////////////////
;///////////////////////////////////////////end socket stuff

(defun gethash-and-trigger (key hash)
  "alias for gethash; used to tweak the setf gethash function so that events are triggered when the hash table is updated"
  (multiple-value-bind (x y) (gethash key hash)
    (values x y)))

(defsetf gethash-and-trigger (key hash) (val)
  "works just like the setf gethash method, except that all events associated with the key of the hash table being changed will also be executed"
  `(progn
     ;update the hash table (analogous to the gethash setf method)
     (setf (gethash ,key ,hash) ,val)
     ;and... grab the event hash table, and eval all events associated with the key being updated
     ;if these events update the hash table, then events associated with that update will also be triggered
     ;and so on...
     (aif (gethash "events" ,hash)
	  (with-pandoric (host port) (gethash "socket" ,hash)
	    (dolist (event (gethash ,key it))
	      (format t "evaluating event on ~a:~a: ~a~%" host port event)
	      (attempt (funcall event)))))
     ,val))

(defmacro make-socket-builder (host port type &rest process-line)
  "defines a socket that is a pandoric function
   the function has an inner loop that processes all lines currently on the stream
   the way that each line is processed is determined by the code supplied in process-line
   'line' (and any other variable within the scope of this builder) is the anaphor"
  `(let* (;stream that will be used to send & receive messages between the lisp backend & the stream
	  (bsd-stream)
	  (bsd-socket)
	  (type ',type)
          ;data storing any received messages
	  (data (make-hash-table :test #'equalp))
          ;maximum length of a channel in the data hash table
	  (host ,(aif host (symbol-name it)))
	  (port ,port)
	  (N 100)
	  (uni-socket-Fn))
     ;data storing any triggered events
     (setf (gethash "events" data) (make-hash-table :test #'equalp))
     (setf (gethash "socket" data)
	   (plambda () (bsd-stream bsd-socket data N uni-socket-Fn host port type)
	     ;if we know where to look for a connection (host:port), but haven't initialized the function that looks for the connection, then init it
	     (if (and (not uni-socket-Fn) host port)
		 (setf uni-socket-Fn (,(symb 'uni- type '-socket) host port)))
             ;if there's not a socket connection currently, and we have a way to look for a connection, then look for it
	     (if (and uni-socket-Fn (not bsd-stream) (not bsd-socket))
		 (multiple-value-setq (bsd-stream bsd-socket) (funcall uni-socket-Fn)))
	     ;make sure that all output on the bsd-stream has reached its destination via the bsd-socket
	     (uni-without-interrupts 
	      (finish-output bsd-stream))
	     (trim-data data N)
	     (let ((line))
	       ;update all of the raw data
	       (while (ignore-errors (listen bsd-stream))
		 (setf line (uni-read-line bsd-stream))
		 (format t "received on ~a:~a: ~a~%" host port line)
		 (cond ((string-equal line "[QUIT]")
			(if (equal ',type 'client)
			    (uni-send-string bsd-stream "[QUIT]"))
			(uni-close bsd-stream bsd-socket)
			(setf bsd-stream nil)
			(setf bsd-socket nil))
		       ;run any extra code to process the received line
		       (t
			,@process-line))))))))

(defmacro make-standard-socket (&key (host) (port) (type `server) (agent))
  "if a line is of the form 'a=b', it will push the evaled value 'b' onto the channel 'a' in data
   otherwise, it will eval the line in place"
  `(make-socket-builder ,host ,port ,type
     (acond ((line2element line)
	     (push (parse-float (cdr it)) (get-channel (car it) ,agent)))
	    (t ;execute a remote procedure call (RPC); that is, run the message on the server, and return the output to the caller
	     (let ((val))
	       (setf val (attempt (eval (read-from-string line)) :on-error (format nil "error: ~a~%" condition)))
	       ;if the caller's socket is still active, return the output to the caller
	       (attempt (uni-send-string bsd-stream (format nil "~a" val))))))))

(defmacro make-modbus-socket-initializer (&key (host) (port) (type `server) (agent))
  "builds a socket that is waiting for the first string returned through the socket
   this string will contain information about the modbus connection (port, SN)
   takes this information and builds read/write registers functions, and stores them in agent 'agent"
  `(make-socket-builder ,host ,port ,type
     ;(remove-job initialize-modbus-port-fn ,(symb agent `-initializer))
     (kill-agent ,(symb agent `-initializer))
     ;modbus port will be passed in on 'line'
     (let ((port ,port))
       (setf (get-pandoric (get-socket ,agent) 'port) port)
       (while (not (get-bsd-stream ,agent))
	 (with-time *seconds-per-update*
	   (format t "trying to connect to modbus on ~a:~a~%" ,(symbol-name host) ,port)
	   (funcall (get-socket ,agent))))
       (setf (get-channel read-registers-fn ,agent) (read-registers (get-bsd-stream ,agent)))
       (setf (get-channel write-registers-fn ,agent) (write-registers (get-bsd-stream ,agent))))))

(defmacro make-modbus-socket (&key (host) (type `server))
  "builds a modbus-style socket
   modbus sockets send/recieve information completely outside of the socket's inner loop
   (using read/write registers methods stored in the sockets data hash table)
   so if a modbus socket receives something sent down the stream while in the inner loop, it should error"
  ;we don't know the port yet, so set it to nil for now
  `(make-socket-builder ,host nil ,type
     (error "read ~a from modbus socket; should not have received anything" line)))

(defmacro define-agent (&key (name) (socket) (host) (port) (type `server))
  "container to store a collection of jobs (in a hash table)
   defines a pandoric function that will loop through all the jobs and 
   execute the functions that have been latched to each job"
  `(progn
     (defpun ,name () ((jobs (make-hash-table))
		       (socket (aif ,socket it (make-standard-socket :host ,host :port ,port :type ,type :agent ,name))))
       (loop for job being the hash-values of jobs
	  do (funcall job)))
     (add-job :agent ,name
	      :job (define-job :name ,name :quota 1 :Fn (get-pandoric ',name 'socket)))
     (push-to-end ',name (get-pandoric 'agents 'agents))))

(defmacro define-modbus-agent (&key (name) (host) (port) (type `server))
  "a modbus-style agent; builds two agents; one responsible for communicating with the daq to get the port location
   of the modbus socket; the second is responsible for sending/receiving information through that modbus socket
   the first agent is killed after relaying port information to the second agent (see 'make-modbus-socket-initializer above)"
  (let ((init-name (symb name `-initializer)))
    `(progn
       (define-agent :name ,init-name
	 :socket (make-modbus-socket-initializer :host ,host :port ,port :type ,type :agent ,name))
       (add-job :agent ,init-name
		:job (define-job :name initialize-modbus-port-fn :quota (updates/second->quota 1)
				 :Fn (lambda ()
				       (aif (get-bsd-stream ,init-name)
					    (uni-send-string it "SCAN")))))
       (define-agent :name ,name :socket (make-modbus-socket :host ,host :type ,type)))))
			
(defmacro get-socket (agent)
  "returns the agent's socket"
  `(get-pandoric ',agent 'socket))

(defmacro get-bsd-stream (agent)
  "returns the bsd-stream inside the agent's socket"
  `(get-pandoric (get-socket ,agent) 'bsd-stream))

(defmacro get-data (agent)
  "returns the data hash table inside the agent's socket"
  `(get-pandoric (get-socket ,agent) 'data))

(defmacro get-channel (channel% agent &key (N) (from `get-data))
  "returns a channel (or subset of the channel) in the data/event hash table inside the agent's socket"
  (let ((channel `(gethash-and-trigger ,(if (symbolp channel%) (symbol-name channel%) channel%) (,from ,agent))))
    (cond ((not N)
	   channel)
	  ((equal N 1)
	   `(car ,channel))
	  (t
	   `(subseq ,channel 0 (min ,N (length ,channel)))))))

(defmacro get-events (agent)
  "returns the events hash table inside the agent's socket"
  `(get-channel events ,agent))

(defpun terminal () ((stdout-default *standard-output*)
		     (stderr-default *error-output*)
		     (stdout *standard-output*)
		     (stderr *error-output*))
  (setf *standard-output* stdout)
  (setf *error-output* stderr))

(defmacro terminal-reset ()
  "resets stdout/stderr to the server's default streams"
  `(progn
     (with-pandoric (stdout stderr stdout-default stderr-default) 'terminal
       (setf stdout stdout-default)
       (setf stderr stderr-default))
     (terminal)))

(define-symbol-macro term-res
    (terminal-reset))

(defmacro terminal-redirect (agent)
  "redirects all stdout/stderr output to the agent's bsd stream"
  `(progn
     (with-pandoric (stdout stderr) 'terminal
       (setf stdout (get-bsd-stream ,agent))
       (setf stderr (get-bsd-stream ,agent)))
     (terminal)))

(define-symbol-macro term-red
    (terminal-redirect monitor))

;defining function that prints an agent
(let ((fun (lambda (agent)
	     (with-pandoric (jobs) (symbol-function agent)
	       (loop for job being the hash-values of jobs
		  do (with-pandoric (name active quot quota) job
		       (format t "from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%" agent name active quot quota)))))))

  (defmacro print-agent (agent)
    "prints agent"
    `(funcall ,fun ',agent))

  (defmacro! print-agents ()
    "prints all of the agents that have been created"
    `(dolist (,g!agent (get-pandoric 'agents 'agents))
       (funcall ,fun ,g!agent))))

(defmacro update-agent (agent)
  "updates agent 'agent'"
  `(funcall (symbol-function ',agent)))

(defmacro! update-agents ()
  "updates all of the agents that have been created"
  `(dolist (,g!agent (get-pandoric #'agents 'agents))
     (funcall (symbol-function ,g!agent))))

(defpun agents () ((agents)))

(let ((disconnect 
       (lambda (agent)
	 (with-pandoric (bsd-socket bsd-stream type) (get-pandoric agent 'socket)
	   ;if stdout/stderr are currently piped to the agent's stream, reset them before killing the agent
	   (if (or (eq bsd-stream *error-output*)
		   (eq bsd-stream *standard-output*))
	       (terminal-reset))
	   (when (uni-stream-active-p bsd-stream)
	     (uni-send-string bsd-stream "[QUIT]")
	     (cond ((equal type 'client)
		    (uni-close bsd-stream bsd-socket)
		    (setf bsd-stream nil)
		    (setf bsd-socket nil))
		   ((equal type 'server)
		    (while (uni-stream-active-p bsd-stream)
		      (with-time *seconds-per-update*
			(funcall (get-pandoric agent 'socket)))))
		   (t (error "shouldn't get here")))))))
      (kill 
       (lambda (agent)
	 (with-pandoric (agents) 'agents
	   (setf agents (remove-if (lambda (x) (equal x agent)) agents)))
	 (fmakunbound agent))))

  (defmacro kill-agent (agent)
    "attempts to cleanly close the agent's socket; then removes the agent from agents"
    `(progn
       (funcall ,disconnect ',agent)
       (funcall ,kill ',agent)))

  (defmacro! kill-agents ()
    "attempts to cleanly close all agents' sockets; then removes all agents from agents"
    `(dolist (,g!agent (get-pandoric 'agents 'agents))
       (funcall ,disconnect ,g!agent)
       (funcall ,kill ,g!agent)))

  (defmacro disconnect-agent (agent)
    "attempts to cleanly close the agent's socket"
    `(funcall ,disconnect ',agent))

  (defmacro! disconnect-agents ()
    "attempts to cleanly close all agents' sockets"
    `(dolist (,g!agent (get-pandoric 'agents 'agents))
       (funcall ,disconnect ,g!agent)))
  
  #+:sbcl
  (defmacro! save-agents ()
    `(progn
       (dolist (,g!agent (get-pandoric 'agents 'agents))
	 (funcall ,disconnect ,g!agent))
       (sb-ext:save-lisp-and-die "saved.core" :toplevel 'run :purify t)))

  )

(defun run ()
  ;update-agents keeps getting called until there are no agents left to update
  ;agents can be removed by evaling (kill-agent agent), or (kill-agents)
  ;currently, the monitor sends a "(kill-agents)" RPC message to the server, which
  ;clears all the agents, which then causes this while loop to exit
  (while (get-pandoric 'agents 'agents)
    (with-time (/ 1 *updates-per-second*) ;updating 60x/second
      (update-agents))))

(define-symbol-macro kill 
    (kill-agents))

(define-symbol-macro disconnect
    (disconnect-agents))

#+:sbcl
(define-symbol-macro save
    (save-agents))

(defmacro add-event (&key (trigger-channel) (Fn))
  "adds an event Fn to the trigger-channel"
  `(push-to-end ,Fn (get-channel ,@trigger-channel :from get-events)))

(defmacro with-channels (channels &body body)
  "allows read/write access to each channel in the 'body forms; access using the channel's name"
  `(symbol-macrolet
       ,(mapcar (lambda (channel) 
		  `(,(car channel) (get-channel ,@channel)))
		channels)
     ,@body))

(defmacro with-channel (channel &body body)
  "same as with-channels, but use when you only want to access a single channel"
  `(with-channels (,channel) ,@body))

(defmacro let-channels (channels &body body)
  "allows read access to each channel in the 'body forms; access using the channel's name"
  `(let 
       ,(mapcar (lambda (channel)
		  `(,(car channel) (get-channel ,@channel)))
		channels)
     ,@body))

(defmacro let-channel (channel &body body)
  "same as let-channels, but use when you only want read access to a single channel"
  `(let-channels (,channel) ,@body))

(defmacro send-output (channel agent val)
  "sends key=val~% to agent's stream"
  `(uni-send-string (get-bsd-stream ,agent)
		    (format nil "~a=~a" ,(symbol-name channel) ,val)))
  
(defmacro add-output-job (&key (agent%) (name%) (output-channel) (updates/second) (value))
  (let ((agent (aif agent% it (second output-channel)))
	(name (aif name% it (first output-channel)))
	(quota (updates/second->quota updates/second)))
    "adds a job to agent that sends 'value' through the socket when quota is reached"
    `(add-job :agent ,agent
	      :job (define-job :name ,name ,@(aif quota (list :quota it))
			       :Fn (lambda ()
				     (aif ,value
					  (send-output ,@output-channel it)))))))
  
(defmacro add-output-event (&key (trigger-channel) (output-channel) (value%))
  "adds an event to agent that sends 'value' through the socket when trigger-channel is updated"
  (let ((value (aif value% it `(get-channel ,@trigger-channel :N 1))))
    `(add-event :trigger-channel ,trigger-channel
		:Fn (lambda ()
		      (aif ,value
			   (send-output ,@output-channel it))))))

(defmacro add-channel-job (&key (agent%) (name%) (channel) (updates/second) (value))
  "adds a job to agent that pushes 'value' onto channel when quota is reached"
  (let ((agent (aif agent% it (second channel)))
	(name (aif name% it (first channel)))
	(quota (updates/second->quota updates/second)))
    `(add-job :agent ,agent
	      :job (define-job :name ,name ,@(aif quota (list :quota it))
			       :Fn (lambda ()
				     (aif ,value
					  (push it (get-channel ,@channel))))))))

;TODO: might want to define add-channel-event...
  
(defmacro! add-calibration (&key (N 10) (measured-channel) (calibrated-channel) (raw-channel))
  "adds a job for an agent that calibrates the raw channel by using the discrepency between the measured and raw-when-measured channels"
  (let* (;place abstracted channels on the agent from the raw channel, and give them gensymed names
	 (slope-channel (list (gensym "SLOPE") (second raw-channel)))
	 (intercept-channel (list (gensym "INTERCEPT") (second raw-channel)))
	 (raw-when-measured-channel (list (gensym "RAW-WHEN-MEASURED") (second raw-channel)))
	 (raw-when-measured (car raw-when-measured-channel))
	 (measured (car measured-channel))
	 (slope (car slope-channel))
	 (intercept (car intercept-channel))
	 (raw (car raw-channel))
	 (calibrated (car calibrated-channel)))
    `(progn
       ;add an event that pushes the calibrated value from the raw-channel on the calibrated-channel
       (add-event :trigger-channel ,raw-channel
		  :Fn (lambda () (with-channels ((,@raw-channel :N 1) (,@intercept-channel :N 1) (,@slope-channel :N 1) (,@calibrated-channel))
				   (push (+ (aif ,intercept it 0) (* (aif ,slope it 1) ,raw)) ,calibrated))))
       ;add an event that saves the raw channel's current value when a measured value is sent back
       (add-event :trigger-channel ,measured-channel 
		  :Fn (lambda () (push (get-channel ,@raw-channel :N 1) 
				       (get-channel ,@raw-when-measured-channel))))
       ;add an event that calibrates the calibrated channel; will be triggered by the above event updating the raw-when-measured-channel
       (add-event :trigger-channel ,raw-when-measured-channel 
		  :Fn (lambda ()
			(with-channels ((,@raw-when-measured-channel :N ,N) (,@measured-channel :N ,N)
					,slope-channel ,intercept-channel)
		          ;grab the updated slope and intercept, and push them on to the slope/intercept channel
			  (when (> (length ,raw-when-measured) 5)
		            (format t "raw-when-measured: ~a~%" ,raw-when-measured)
		            (format t "measured: ~a~%" ,measured)
			      (multiple-value-bind (,g!slope ,g!intercept) (linear-regression ,raw-when-measured ,measured)
				(format t "slope/intercept: ~a/~a~%" ,g!slope ,g!intercept)
				(push ,g!slope ,slope)
				(push ,g!intercept ,intercept)))))))))

(defmacro convert (form)
  "converts a form to a string that when (eval (read-from-string str)) returns the evaled form"
  `(format nil "(eval ~a )" ,(toString form)))
       
(defun compile-server ()
  (compile-file "server.lisp" :output-file "server.fasl"))

;dynamically scoped variable that stores the monitor's bsd-stream
(defvar *monitor-bsd-stream*)
  
(defmacro send (form)
  "send is a shorthand for the monitor to send a message to the server;
   form gets converted to a string that the server will eval"
  `(uni-send-string *monitor-bsd-stream* (convert ,form)))

(defmacro send-string (str)
  "sends string str to server"
  `(uni-send-string *monitor-bsd-stream* ,str))

(defmacro send-to-agent (agent form)
  "send a message that's sent first to the server, then bounced to the agent, to be evaled"
  `(send (uni-send-string (get-bsd-stream ,agent) (convert ,form))))

(defmacro send-string-to-agent (agent str)
  "sends string str to agent via server"
  `(send (uni-send-string (get-bsd-stream ,agent) ,str)))

(defvar *server-ip* "127.0.0.1") 

(defun run-monitor (port)
  "connects a monitor agent to the server"
  (let* ((bsd-socket) (bsd-stream) (line) (host *server-ip*))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-connect host port))
    (setf *monitor-bsd-stream* bsd-stream)
    (while (uni-stream-active-p bsd-stream)
      (with-time *seconds-per-update*
	(if (listen) (attempt (eval (read))))
	(while (and (uni-stream-active-p bsd-stream) (listen bsd-stream))
	  (setf line (uni-read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (uni-close bsd-stream bsd-socket)))))))

(defun run-display ()
  "connects a display agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host *server-ip*) (port 9557))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-connect host port))
    (while (uni-stream-active-p bsd-stream)
      (with-time *seconds-per-update*
	(while (and (uni-stream-active-p bsd-stream) (listen bsd-stream))
	  (setf line (uni-read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (aif (line2element line)
	       (if (equalp (car it) "rpm-raw")
		   (uni-send-string bsd-stream (format nil "rpm-measured=~f" 
						       (+ (parse-float (cdr it)) 
							  (- (/ (random 1000) 10000) .05))))))
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (uni-close bsd-stream bsd-socket)))))))

(defun run-daq ()
  "connects a daq agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host *server-ip*) (port 9556))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-connect host port))
    (while (uni-stream-active-p bsd-stream)
      (dotimes (i 10)
	(with-time *seconds-per-update*
	  (uni-send-string bsd-stream (format nil "RPM-Raw=~a" i))))
      (while (and (uni-stream-active-p bsd-stream) (listen bsd-stream))
	(setf line (uni-read-line bsd-stream))
	(format t "received on ~a:~a: ~a~%" host port line)
	(when (string-equal line "[QUIT]")
	  (uni-send-string bsd-stream "[QUIT]")
	  (uni-close bsd-stream bsd-socket))))))

;(setf *break-on-signals* t)
       




	  



	



