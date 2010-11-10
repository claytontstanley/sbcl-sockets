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
;;; Description : A socket-based server; this implementation is not concurrent/threaded
;;;             
;;; Dependencies: requires that letf.lisp is loaded; loads uni-files.lisp
;;;
;;; Bugs        : ???
;;;
;;; ----- History -----
;;;
;;; 2010.10.11  : Creation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;load some functions that make it easier to send/receive messages across a socket
(load "uni-files.lisp")

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

(defmacro attempt (form &key (on-error `(format t "error: ~a~%" condition)))
  "anaphoric macro for attemping to evaluate form; default is to print error to screen on error"
  `(handler-case ,form (error (condition) ,on-error)))

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

(defmacro socket-active-p (socket)
  `(and ,socket (sb-bsd-sockets:socket-open-p ,socket)))

(defmacro define-job (&key (name) (Fn) (active-p t) (quota 1))
  "discrete event simulator that executes functions at the specified time, 
   when active, you can pass functions to execute after each time the quota is reached
   the functioned returned is pandoric, so you can access/change state variables"
    `(let ((quota ,quota)
	   (quot 0)
	   (name ',name)
	   (active ,active-p)
	   (Fn ,Fn))
       (plambda () (name quot quota active Fn)
	 (when active
	   (incf quot)
	   (when (eq quot quota)
	     (setf quot 0)
	     (attempt (funcall Fn)))))))

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
  
(defmacro make-socket (&key (bsd-stream) (bsd-socket) (host) (port))
  "defines a socket that is a pandoric function
   the function has an inner loop that processes all lines currently on the stream;
   if a line is '[QUIT]', it will close the stream;
   if a line is of the form 'a=b', it will push the evaled value 'b' onto the channel 'a' in data
   otherwise, it will eval the line in place"
  `(let* (;stream that will be used to send & receive messages between the lisp backend & the stream
	  (bsd-stream ,bsd-stream)
	  (bsd-socket ,bsd-socket)
          ;data storing any received messages
	  (data (make-hash-table :test #'equalp))
          ;maximum length of a channel in the data hash table
	  (host ,(aif host (symbol-name it)))
	  (port ,port)
	  (N 100)
	  (uni-prepare-socket-Fn (if (and host port) (uni-prepare-socket ,host ,port))))
     ;data storing any triggered events
     (setf (gethash "events" data) (make-hash-table :test #'equalp))
     (setf (gethash "socket" data)
	   (plambda () (bsd-stream bsd-socket data N uni-prepare-socket-Fn host port)
             ;if there's not a socket connection currently, and we have a way to look for a connection, then look for it
	     (if (and (not bsd-stream) (not bsd-socket) uni-prepare-socket-Fn)
		 (multiple-value-setq (bsd-stream bsd-socket) (funcall uni-prepare-socket-Fn)))
	     (uni-without-interrupts 
	      (finish-output bsd-stream))
	     (let ((line))
	       ;update all of the raw data
	       (while (ignore-errors (listen bsd-stream))
		 (trim-data data N)
		 (setf line (uni-socket-read-line bsd-stream))
		 (format t "received on ~a:~a: ~a~%" host port line)
		 (acond ((string-equal line "[QUIT]")
			 (if bsd-stream (sb-bsd-sockets::close bsd-stream))
			 (if bsd-socket (sb-bsd-sockets:socket-close bsd-socket))
			 (setf bsd-stream nil)
			 (setf bsd-socket nil))
			((line2element line)
			 (push (parse-float (cdr it)) (gethash-and-trigger (car it) data)))
			(t ;execute a remote procedure call (RPC); that is, run the message on the server, and return the output to the caller
			 (let ((val))
			   (attempt (setf val (eval (read-from-string line)))
				    :on-error (setf val (format nil "~a~%error: ~a~%" val condition)))
		           ;if the caller's socket is still active, return the output to the caller
			   (attempt (uni-send-string bsd-stream (format nil "~a" val))))))))))))

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

(defmacro define-agent (&key (name) (socket) (host) (port))
  "container to store a collection of jobs (in a hash table)
   defines a pandoric function that will loop through all the jobs and 
   execute the functions that have been latched to each job"
  `(let ((jobs (make-hash-table))
	 (socket (aif ,socket it (make-socket :host ,host :port ,port))))
     (defpun ,name () (jobs socket)
       (loop for job being the hash-values of jobs
	  do (funcall job)))
     (add-job :agent ,name
	      :job (define-job :name ,name :quota 1 :Fn socket))
     (push-to-end ',name (get-pandoric 'agents 'agents))))

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
  (let ((channel `(gethash-and-trigger ,(symbol-name channel%) (,from ,agent))))
    (cond ((not N)
	   channel)
	  ((equal N 1)
	   `(car ,channel))
	  (t
	   `(subseq ,channel 0 (min ,N (length ,channel)))))))

(defmacro get-events (agent)
  "returns the events hash table inside the agent's socket"
  `(get-channel events ,agent))

(let* ((stdout-default *standard-output*)
       (stderr-default *error-output*)
       (stdout stdout-default)
       (stderr stderr-default))
  (defpun terminal () (stdout-default stderr-default stdout stderr)
    (setf *standard-output* stdout)
    (setf *error-output* stderr)
    ))

(defmacro terminal-reset ()
  "resets stdout/stderr to the server's default streams"
  `(progn
     (with-pandoric (stdout stderr stdout-default stderr-default) 'terminal
       (setf stdout stdout-default)
       (setf stderr stderr-default))
     (terminal)))

(defmacro terminal-redirect (agent)
  "redirects all stdout/stderr output to the agent's bsd stream"
  `(progn
     (with-pandoric (stdout stderr) 'terminal
       (setf stdout (get-bsd-stream ,agent))
       (setf stderr (get-bsd-stream ,agent)))
     (terminal)))

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

(let ((agents))
  (defpun agents () (agents)
    ()))

(let ((fun (lambda (agent)
	     (with-pandoric (bsd-socket bsd-stream) (get-pandoric agent 'socket)
	       ;if stdout/stderr are currently piped to the agent's stream, reset them before killing the agent
	       (if (eq bsd-stream *standard-output*) (terminal-reset))
	       (when (socket-active-p bsd-socket)
		 (uni-send-string bsd-stream "[QUIT]")
		 (while (socket-active-p bsd-socket)
		   (funcall (get-pandoric agent 'socket)))))
	     (with-pandoric (agents) 'agents
	       (setf agents (remove-if (lambda (x) (equal x agent)) agents)))
	     (fmakunbound agent))))

  (defmacro kill-agent (agent)
    "attempts to cleanly close the agent's socket; then removes the agent from agents"
    `(funcall ,fun ',agent))

  (defmacro! kill-agents ()
    "attempts to cleanly close all agent's sockets; then removes all agents from agents"
    `(dolist (,g!agent (get-pandoric 'agents 'agents))
       (funcall ,fun ,g!agent))))

(define-symbol-macro kill 
    (kill-agents))

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
  
;(defmacro add-output (&key (agent) (name) (quota) (value))
;  "adds an output to agent that sends 'value' through the socket"
;  `(add-job :agent ,agent
;	    :job (define-job :name ,name ,@(aif quota (list :quota it))
;			     :Fn (lambda ()
;				   (aif ,value
;					(send-output ,name ,agent it))))))

(defmacro add-output-event (&key (output-channel) (trigger-channel) (value%))
  "adds an output event to agent that sends 'value' through the socket when trigger-channel is updated"
  (let ((value (aif value% it `(get-channel ,@trigger-channel :N 1))))
    `(add-event :trigger-channel ,trigger-channel
		:Fn (lambda ()
		      (aif ,value
			   (send-output ,@output-channel it))))))

;(defmacro add-channel (&key (agent) (name) (quota) (value))
;  "adds channel to agent that evaluates value"
;  `(add-job :agent ,agent
;	    :job (define-job :name ,name ,@(aif quota (list :quota it))
;			     :Fn (lambda ()
;				   (aif ,value
;					(push it (get-channel ,name ,agent)))))))
  
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
;stores the refresh rate of the server
(defvar *updates-per-second* 60) 

(let ((i -1)
      (average-headroom 0))
  (defpun headroom (val) (i average-headroom)
    (incf i)
    (setf average-headroom 
	  (/ (+ (* average-headroom i) val) (+ i 1)))))
  
(defmacro send (form)
  "send is a shorthand for the monitor to send a message to the server;
   form gets converted to a string that the server will eval"
  `(uni-send-string *monitor-bsd-stream* (convert ,form)))

(defmacro send-string (str)
  "sends string str to server"
  `(uni-send-string *monitor-bsd-stream* ,str))

(defmacro send-to (agent form)
  "send a message that's sent first to the server, then bounced to the agent, to be evaled"
  `(send (uni-send-string (get-bsd-stream ,agent) (convert ,form))))

(defmacro send-string-to (agent str)
  "sends string str to agent via server"
  `(send (uni-send-string (get-bsd-stream ,agent) ,str)))

(defmacro! with-time (time &body body)
  "executes body in 'time' seconds; time is intended to be longer than it should take to execute body"
  `(let* ((,g!start (get-internal-real-time))
	  (,g!finish (+ (* ,time internal-time-units-per-second) ,g!start))
	  (,g!time-to-sleep))
     ,@body
     (setf ,g!time-to-sleep (/ (- ,g!finish (get-internal-real-time)) internal-time-units-per-second))
     ;(headroom ,g!time-to-sleep)
     (if (< ,g!time-to-sleep 0)
	 (format t "lagging behind by ~a seconds~%" (coerce (abs ,g!time-to-sleep) 'double-float))
	 (sleep ,g!time-to-sleep))))

(defun run-monitor ()
  "connects a monitor agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9558))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (setf *monitor-bsd-stream* bsd-stream)
    (while (socket-active-p bsd-socket)
      (with-time (/ 1 *updates-per-second*)
	(if (listen) (attempt (eval (read))))
	(while (and (socket-active-p bsd-socket) (listen bsd-stream))
	  (setf line (read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (sb-bsd-sockets::close bsd-stream)
	    (sb-bsd-sockets:socket-close bsd-socket)))))))
  
(defun run-display ()
  "connects a display agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9557) (hsh (make-hash-table :test #'equalp)))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (while (socket-active-p bsd-socket)
      (with-time (/ 1 *updates-per-second*)
	(while (and (socket-active-p bsd-socket) (listen bsd-stream))
	  (trim-data hsh 200)
	  (setf line (read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (aif (line2element line)
	       (if (equalp (car it) "rpm-raw")
		   (uni-send-string bsd-stream (format nil "rpm-measured=~f" (+ (parse-float (cdr it)) 0 (- (/ (random 1000) 10000) .05))))))
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (sb-bsd-sockets::close bsd-stream)
	    (sb-bsd-sockets:socket-close bsd-socket)))))))

(defun run-daq ()
  "connects a daq agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9556))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (while (socket-active-p bsd-socket)
      (dotimes (i 10)
	(with-time (/ 1 *updates-per-second*)
	  (uni-send-string bsd-stream (format nil "RPM-Raw=~a" i))))
      (while (and (socket-active-p bsd-socket) (listen bsd-stream))
	(setf line (read-line bsd-stream))
	(format t "received on ~a:~a: ~a~%" host port line)
	(when (string-equal line "[QUIT]")
	  (uni-send-string bsd-stream "[QUIT]")
	  (sb-bsd-sockets::close bsd-stream)
	  (sb-bsd-sockets:socket-close bsd-socket))))))

;(setf *break-on-signals* t)
       




	  



	



