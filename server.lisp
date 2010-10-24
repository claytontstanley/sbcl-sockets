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
  "alias for gethash; used to tweak the setf function so that events are triggered when the hash table is updated"
  (multiple-value-bind (x y) (gethash key hash)
    (values x y)))

(defsetf gethash-and-trigger (key hash) (val)
  "works just like the gethash setf method, except that all events associated with the key of the hash table being changed will also be executed"
  `(progn
     ;update the hash table (analogous to the gethash setf method)
     (setf (gethash ,key ,hash) ,val)
     ;and... grab the event hash table, and eval all events associated with the key being updated
     ;if these events update the hash table, then events associated with that update will also be triggered
     ;and so on...
     (aif (gethash "events" ,hash)
	  (dolist (event (gethash ,key it))
	    (format t "evaluating event: ~a~%" event)
	    (attempt (funcall event))))
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
     (plambda () (bsd-stream bsd-socket data N uni-prepare-socket-Fn host port)
       ;if there's not a socket connection currently, and we have a way to look for a connection, then look for it
       (if (and (not bsd-stream) (not bsd-socket) uni-prepare-socket-Fn)
	   (multiple-value-setq (bsd-stream bsd-socket) (funcall uni-prepare-socket-Fn)))
       (trim-data data N)
       (let ((line))
	 ;update all of the raw data
	 (while (ignore-errors (listen bsd-stream))
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
		   (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
			 (val))
		     (attempt (progn
				(with-output-to-string (*standard-output* fstr)
				  (with-output-to-string (*error-output* fstr)
				    (setf val (eval (read-from-string line)))))
				(setf fstr (format nil "~a~%~a" fstr val)))
			      :on-error (setf fstr (format nil "~a~%error: ~a" fstr condition)))
		     (format t "~a~%" fstr)
		     ;if the caller's socket is still active, return the output to the caller
		     (attempt (uni-send-string bsd-stream fstr))))))))))

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
  `(let ,(mapcar (lambda (channel)
		   `(,(car channel) (get-channel ,@channel)))
		 channels)
     ,@body))

(defmacro let-channel (channel &body body)
  "same as let-channels, but use when you only want read access to a single channel"
  `(let-channels (,channel) ,@body))

(defmacro send-output (agent key val)
  "sends key=val~% to agent's stream"
  `(uni-send-string (get-bsd-stream ,agent)
		    (format nil "~a=~a" ,(symbol-name key) ,val)))
  
;(defmacro add-output (&key (agent) (name) (quota) (value))
;  "adds an output to agent that sends 'value' through the socket"
;  `(add-job :agent ,agent
;	    :job (define-job :name ,name ,@(aif quota (list :quota it))
;			     :Fn (lambda ()
;				   (aif ,value
;					(send-output ,agent ,name it))))))

(defmacro add-output-event (&key (agent) (name%) (trigger-channel) (value%))
  "adds an output event to agent that sends 'value' through the socket when trigger-channel is updated"
  (let ((name (aif name% it (car trigger-channel)))
	(value (aif value% it `(get-channel ,@trigger-channel :N 1))))
    `(add-event :trigger-channel ,trigger-channel
		:Fn (lambda ()
		      (aif ,value
			   (send-output ,agent ,name it))))))

;(defmacro add-channel (&key (agent) (name) (quota) (value))
;  "adds channel to agent that evaluates value"
;  `(add-job :agent ,agent
;	    :job (define-job :name ,name ,@(aif quota (list :quota it))
;			     :Fn (lambda ()
;				   (aif ,value
;					(push it (get-channel ,name ,agent)))))))
  
(defmacro add-calibration (&key (slope-channel%) (intercept-channel%) (N 30)
			  (measured-channel) (displayed-channel) (calibrated-channel) (raw-channel))
  "adds a job for an agent that calibrates the raw channel by using the discrepency between the measured and displayed channels"
  (let* (;if slope-channel name and agent not provided, place it on the agent from the raw channel, and give it a random name
	 (slope-channel (aif slope-channel% it (list (gensym "SLOPE") (second raw-channel))))
	 ;if intercept-channel name and agent not provided, place it on the agent from the raw channel, and give it a random name
	 (intercept-channel (aif intercept-channel% it (list (gensym "INTERCEPT") (second raw-channel))))
	 (displayed (car displayed-channel))
	 (measured (car measured-channel))
	 (slope (car slope-channel))
	 (intercept (car intercept-channel))
	 (raw (car raw-channel))
	 (calibrated (car calibrated-channel))
	 (Fn (gensym)))
    `(let ((,Fn (define-job :name ,(symb `calibration-for- calibrated-channel)
		  :Fn (lambda ()
			(let-channels ((,@displayed-channel :N ,N) (,@measured-channel :N ,N)
				       (,@slope-channel :N 1) (,@intercept-channel :N 1))
			  ;we need to convert the displayed channel back to raw data scale
			  ;(format t "displayed before lambda: ~a~%" ,displayed)
			  (if (and ,intercept ,slope)
			      (setf ,displayed (mapcar (lambda (y) (/ (- y ,intercept) ,slope)) ,displayed)))
			  ;then grab the updated slope and intercept, and push them on to the slope/intercept channel
			  (when (> (length ,displayed) 5)
			    ;(format t "displayed: ~a~%" ,displayed)
			    ;(format t "measured: ~a~%" ,measured)
			    (multiple-value-setq (,slope ,intercept) (linear-regression ,displayed ,measured))
			    ;(format t "slope/intercept: ~a/~a~%" ,slope ,intercept)
			    (push ,slope (get-channel ,@slope-channel))
			    (push ,intercept (get-channel ,@intercept-channel)))))
		  :quota 2))) ;quota is 2 here b/c this job should only fire after both the measured and displayed values are sent back       
       ;add an event that; whenever the raw-channel receives a value, the calibrated value from the raw-channel will be pushed
       ;on the calibrated-channel
       (add-event :trigger-channel ,raw-channel
		  :Fn (lambda () (with-channels ((,@raw-channel :N 1) (,@intercept-channel :N 1) (,@slope-channel :N 1) (,@calibrated-channel))
				   (push (+ (aif ,intercept it 0) (* (aif ,slope it 1) ,raw)) ,calibrated))))
       ;add the job that calibrates the calibrated channel
       (add-event :trigger-channel ,measured-channel :Fn ,Fn)
       (add-event :trigger-channel ,displayed-channel :Fn ,Fn))))

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
    (uni-send-string bsd-stream "(print-agents)")
    (while (socket-active-p bsd-socket)
      (with-time (/ 1 *updates-per-second*)
	(while (and (socket-active-p bsd-socket) (listen bsd-stream))
	  (trim-data hsh 200)
	  (setf line (read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (aif (line2element line)
	       (if (equalp (car it) "rpm-raw")
		   (uni-send-string bsd-stream (format nil "rpm-measured=~a" (cdr it)))
		   (uni-send-string bsd-stream (format nil "rpm-displayed=~a" (+ (parse-float (cdr it)) 0 #|(- (/ (random 1000) 10000) .05)|#)))))
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (sb-bsd-sockets::close bsd-stream)
	    (sb-bsd-sockets:socket-close bsd-socket)))))))

(defun run-daq ()
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9556))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (while (socket-active-p bsd-socket)
      (dotimes (i 10)
	(with-time 1 ;(/ 1 *updates-per-second*)
	  (uni-send-string bsd-stream (format nil "RPM-Raw=~a" i))))
      (while (and (socket-active-p bsd-socket) (listen bsd-stream))
	(setf line (read-line bsd-stream))
	(format t "received on ~a:~a: ~a~%" host port line)
	(when (string-equal line "[QUIT]")
	  (uni-send-string bsd-stream "[QUIT]")
	  (sb-bsd-sockets::close bsd-stream)
	  (sb-bsd-sockets:socket-close bsd-socket))))))

;(setf *break-on-signals* t)
       
(defun run-server ()
  "top-level function that runs the lisp backend server"

  ;agent in charge of all jobs concerning the DAQ (that is, the DAQ->lisp bridge)
  (define-agent :name DAQ
    :host 127.0.0.1
    :port 9556)

  ;agent in charge of all jobs concerning the display (that is, the lisp->OSX bridge)
  (define-agent :name display
    :host 127.0.0.1 
    :port 9557)

  ;agent in charge of monitoring the server; 
  ;agent can send the server messages, query, execute remote procedure calls (RPCs) etc.
  (define-agent :name monitor
    :host 127.0.0.1
    :port 9558)

  ;add an event that creates a calibrated-channel from the raw-channel, using the discrepency between the measured-channel
  ;and displayed-channel to calibrate
  (add-calibration :measured-channel (rpm-measured display)
		   :displayed-channel (rpm-displayed display)
		   :calibrated-channel (RPM DAQ)
		   :raw-channel (RPM-raw DAQ))
    
  ;send the calibrated rpm signal to the display
  (add-output-event :agent display 
		    :trigger-channel (RPM DAQ))
  
  ;send the raw rpm signal to the display as well
  (add-output-event :agent display
		    :trigger-channel (RPM-Raw DAQ))
  
  ;display all agents and their jobs that will be called each time 'update-call' is called
  (print-agents)

  ;update-agents keeps getting called until there are no agents left to update
  ;agents can be removed by evaling (kill-agent agent), or (kill-agents)
  ;currently, the monitor sends a "(kill-agents)" RPC message to the server, which
  ;clears all the agents, which then causes this while loop to exit
  (while (get-pandoric 'agents 'agents)
    (with-time (/ 1 *updates-per-second*) ;updating 60x/second
      (update-agents))))



	  



	



