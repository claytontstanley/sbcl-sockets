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

(defmacro! trim-data (data N)
  "trims the ends off all channels in the data hash table, so that no channel is > N in length"
  `(loop for ,g!channel being the hash-values of ,data
      do (let ((Ln (+ 1 (length ,g!channel)))) 
	   (while (> (decf Ln) ,N)
	     (setf ,g!channel (nbutlast ,g!channel))))))

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
	     (funcall Fn)
	     (setf quot 0))))))

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
	  ;data storing any triggered events
	  (events (make-hash-table :test #'equalp))
          ;maximum length of a channel in the data hash table
	  (host ,(aif host (symbol-name it)))
	  (port ,port)
	  (N 100)
	  (uni-prepare-socket-Fn (if (and host port) (uni-prepare-socket host port))))
     (plambda () (bsd-stream bsd-socket data events N uni-prepare-socket-Fn host port)
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
		   (push (eval (read-from-string (cdr it))) (gethash (car it) data))
		   (dolist (event (gethash (car it) events))
		     (format t "evaluating event on ~a:~a: ~a~%" host port event)
		     (handler-case
			 (funcall event)
		       (error (condition) (format t "error: ~a~%" condition)))))
		  (t ;execute a remote procedure call (RPC); that is, run the message on the server, and return the output to the caller
		   (let ((fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
			 (val))
		     (handler-case 
			 (progn
			   (with-output-to-string (*standard-output* fstr)
			     (with-output-to-string (*error-output* fstr)
			       (setf val (eval (read-from-string line)))))
			   (setf fstr (format nil "~a~%~a" fstr val)))
		       (error (condition) (setf fstr (format nil "~a~%error: ~a" fstr condition))))
		     (format t "~a~%" fstr)
		     ;if the caller's socket is still active, return the output to the caller
		     (handler-case
			 (uni-send-string bsd-stream (format nil "~a~%" fstr))
		       (error (condition) (format t "error: ~a~%" condition)))))))))))

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
		 (uni-send-string bsd-stream (format nil "[QUIT]~%"))
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

(defmacro get-events (agent)
  "returns the events hash table inside the agent's socket"
  `(get-pandoric (get-socket ,agent) 'events))

(defmacro get-channel (channel agent &key (N) (from `get-data))
  "returns a channel (or subset of the channel) in the data/event hash table inside the agent's socket"
  (setf channel `(gethash ,(symbol-name channel) (,from ,agent)))
  (cond ((not N)
	 channel)
	((equal N 1)
	 `(car ,channel))
	(t
	 `(subseq ,channel 0 (min ,N (length ,channel))))))

(defmacro add-event (&key (trigger-channel) (Fn))
  `(push-to-end ,Fn (get-channel ,@trigger-channel :from get-events)))

;TODO; can you allow 'write' access as well? that would be pretty sweet
(defmacro with-channels (channels &body body)
  "allows read access to each channel in the 'body forms; access using the channel's name"
  `(let ,(mapcar (lambda (channel)
		   `(,(car channel) (get-channel ,@channel)))
		 channels)
     ,@body))

(defmacro with-channel (channel &body body)
  "same as with-channels, but use when you only want to access a single channel"
  `(with-channels (,channel) ,@body))

(defmacro send-output (agent key val)
  "sends key=val~% to agent's stream"
  `(uni-send-string (get-bsd-stream ,agent)
		    (format nil "~a=~a~%" ,(symbol-name key) ,val)))
  
(defmacro add-output (&key (agent) (name) (quota) (value))
  "adds an output to agent that sends 'value' through the socket"
  `(add-job :agent ,agent
	    :job (define-job :name ,name ,@(aif quota (list :quota it))
			     :Fn (lambda ()
				   (aif ,value
					(send-output ,agent ,name it))))))

(defmacro add-output-event (&key (agent) (name) (trigger-channel) (value))
  (if (not name) (setf name (car trigger-channel)))
  (if (not value) (setf value `(get-channel ,@trigger-channel :N 1)))
  `(add-event :trigger-channel ,trigger-channel
	      :Fn (lambda ()
		    (aif ,value
			 (send-output ,agent ,name it)))))

(defmacro add-channel (&key (agent) (name) (quota) (value))
  "adds channel to agent that evaluates value"
  `(add-job :agent ,agent
	    :job (define-job :name ,name ,@(aif quota (list :quota it))
			     :Fn (lambda ()
				   (aif ,value
					(push it (get-channel ,name ,agent)))))))

(defmacro add-calibration (&key (slope-channel) (intercept-channel) (N 30)
			  (measured-channel) (displayed-channel) (calibrated-channel) (raw-channel))
  "adds a job for an agent that calibrates the raw channel by using the discrepency between the measured and displayed channels"
  (let* (;if slope-channel name and agent not provided, place it on the agent from the raw channel, and give it a random name
	 (slope-channel (aif slope-channel it (list (gensym "SLOPE") (second raw-channel))))
	 ;if intercept-channel name and agent not provided, place it on the agent from the raw channel, and give it a random name
	 (intercept-channel (aif intercept-channel it (list (gensym "INTERCEPT") (second raw-channel))))
	 (displayed (car displayed-channel))
	 (measured (car measured-channel))
	 (slope (car slope-channel))
	 (intercept (car intercept-channel))
	 (raw (car raw-channel))
	 (Fn (gensym)))
    `(let ((,Fn (let ((count 0))
		  (lambda ()
		    (incf count)
		    ;(format t "###~a~%" count)
		    (when (equal (mod count 2) 0)
		      (with-channels ((,@displayed-channel :N ,N) (,@measured-channel :N ,N)
				      (,@slope-channel :N 1) (,@intercept-channel :N 1))
			;we need to convert the displayed channel back to raw data scale
			(if (and ,intercept ,slope)
			    (setf ,displayed (mapcar (lambda (y) (/ (- y ,intercept) ,slope)) ,displayed)))
			;then grab the updated slope and intercept, and push them on to the slope/intercept channel
			(when (> (length ,displayed) 5)
			  (multiple-value-setq (,slope ,intercept) (linear-regression ,measured ,displayed))
			  (push ,slope (get-channel ,@slope-channel))
			  (push ,intercept (get-channel ,@intercept-channel)))))))))
       
       ;add an event that; whenever the raw-channel receives a value, the calibrated value from the raw-channel will be pushed
       ;on the calibrated-channel
       (add-event :trigger-channel ,raw-channel
		  :Fn (lambda () (with-channels ((,@raw-channel :N 1) (,@intercept-channel :N 1) (,@slope-channel :N 1))
				   (aif (+ (aif ,intercept it 0) (* (aif ,slope it 1) ,raw))
					(push it (get-channel ,@calibrated-channel))))))
       ;add the job that calibrates the calibrated channel
       (add-event :trigger-channel ,measured-channel
		  :Fn ,Fn)
       (add-event :trigger-channel ,displayed-channel
		  :Fn ,Fn))))
       
(defun compile-server ()
  (compile-file "server.lisp" :output-file "server.fasl"))

(let ((monitor-bsd-stream))
  (defpun send-Fn (str) (monitor-bsd-stream)
    (uni-send-string monitor-bsd-stream (format nil "~a~%" str)))

  (defmacro send (form)
    "send is a shorthand for the monitor to send a message to the server;
     form gets converted to a string that the server will eval"
    `(send-Fn ,(fast-concatenate "(eval " (toString form) ")")))

  (defmacro send-string (str)
    "sends string str to server"
    `(send-Fn ,str)))

(defun run-monitor ()
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9558))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (with-pandoric (monitor-bsd-stream) #'send-Fn
      (setf monitor-bsd-stream bsd-stream))
    (while (socket-active-p bsd-socket)
	       
      (handler-case (format t "> ~a~%" (eval (read)))
	(error (condition) (format t "~a~%" condition)))

      (while (and (socket-active-p bsd-socket) (listen bsd-stream))
	(setf line (read-line bsd-stream))
	(format t "received on ~a:~a: ~a~%" host port line)
	(when (string-equal line "[QUIT]")
	  (uni-send-string bsd-stream (format nil "[QUIT]~%"))
	  (sb-bsd-sockets::close bsd-stream)
	  (sb-bsd-sockets:socket-close bsd-socket))))))
  
(defun run-display ()
  (let ((bsd-socket) (bsd-stream) (line) (host "127.0.0.1") (port 9557))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-make-socket host port))
    (uni-send-string bsd-stream (format nil "(print-agents)~%"))
    (while (socket-active-p bsd-socket)
      (while (and (socket-active-p bsd-socket) (listen bsd-stream))
	(setf line (read-line bsd-stream))
	(format t "received on ~a:~a: ~a~%" host port line)
	(dotimes (i 10)
	  (uni-send-string bsd-stream (format nil "RPM-displayed=~a~%" (random 10)))
	  (uni-send-string bsd-stream (format nil "RPM-measured=~a~%" (random 10))))
	(when (string-equal line "[QUIT]")
	  (uni-send-string bsd-stream (format nil "[QUIT]~%"))
	  (sb-bsd-sockets::close bsd-stream)
	  (sb-bsd-sockets:socket-close bsd-socket))))))

;(setf *break-on-signals* t)
       
(defun run-server ()
  "top-level function that runs the lisp backend server"

  ;agent in charge of all jobs concerning the DAQ (that is, the DAQ->lisp bridge)
  ;mocking up some data in here for now, until we can actually hook up the DAQ to the server
  (define-agent :name DAQ 
    :socket (make-socket :bsd-stream 
			 (make-string-input-stream
			  (with-output-to-string (out)
			    (format out "RPM-Raw=5~%") ;typical look of a single line in the DAQ stream
			    (format out "RPM-Raw=4~%")
			    (format out "(print \"hello world\")~%") ;but, you can also send lisp code to get evaled in place
			    (format out "[QUIT]~%") ;and, when you want to close the channel, just send this string
			    ))))

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
  (time
   (while (get-pandoric 'agents 'agents)
     (update-agents))))



	



