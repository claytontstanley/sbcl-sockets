(load "uni-files.lisp")

;newly-discovered anaphoric cond; works just like cond, but stores the 
;value of each condition as 'it', which is accessable in the code
;following the condition
(defmacro! acond (&rest clauses)
    (if clauses
	(let ((cl1 (car clauses)))
	  `(let ((,g!sym ,(car cl1)))
	     (if ,g!sym
		 (let ((it ,g!sym)) 
		   (declare (ignorable it)) 
		   ,@(cdr cl1))
		 (acond ,@(cdr clauses)))))))

(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro! trim-data (data N)
  "trims the ends off all channels in the data hash table, so that no channel is > N in length"
  `(loop for ,g!channel being the hash-values of ,data
      do (if (> (length ,g!channel) ,N)
	     (setf ,g!channel (nbutlast ,g!channel)))))

(let ((agents))
  (defpun agents () (agents)
    ()))

;DEVS (discreet event simulator) that executes functions at the specified time, 
;when active, you can pass functions to execute at each iteration (at quot)
;and after each time the quota is reached (at quota)
;the functioned returned is pandoric, so you can access/change state variables
(defmacro define-job (&key (name) (quota) (QuotaFn) (QuotFn))
    `(let ((quota ,quota)
	   (quot 0)
	   (name ,name)
	   (active t))
       (plambda () (name quot quota active)
	 ,(if QuotFn
	      `(if active (funcall ,QuotFn)))
	 (incf quot)
	 (when (eq quot quota)
	   ,(if QuotaFn
		`(if active (funcall ,QuotaFn)))
	   (setf quot 0)))))

;defines a stream that is a pandoric function
;the function has an inner loop that processes all lines currently on strm!
;if a line is "[QUIT]", it will close the stream
;if a line is of the form "a=b", it will push the evaled value 'b' onto the channel 'a' in data
;otherwise, it will eval the line in place
(defmacro make-socket (&key (bsd-stream) (bsd-socket))
  `(let (;stream that will be used to send & receive messages between the lisp backend & the stream
	 (bsd-stream ,bsd-stream)
	 (bsd-socket ,bsd-socket)
         ;data storing any received messages
	 (data (make-hash-table :test #'equalp)) 
         ;maximum length of a channel in the data hash table
	 (N 100)) 
     (plambda () (bsd-stream bsd-socket data N)
       (trim-data data N)
       (let ((line))
	       ;update all of the raw data
	 (while (handler-case (listen bsd-stream) 
		  (error (condition) (declare (ignore condition)) nil))
	   (setf line (uni-socket-read-line bsd-stream))
	   (acond ((string-equal line "[QUIT]")
		   (if bsd-stream (sb-bsd-sockets::close bsd-stream))
		   (if bsd-socket (sb-bsd-sockets:socket-close bsd-socket)))
		  ((line2element line)
		   (push (eval (read-from-string (cdr it))) (gethash (car it) data)))
		  (t
		   (eval (read-from-string line)))))))))

(defmacro! add-job (o!job to)
  "adds job o!job to pandoric 'to'; errors if already present"
  `(let ((,g!name (get-pandoric ,g!job 'name)))
     (assert (not (job-present-p ,g!name ,to)) nil "tried to add job ~a that is already present~%" ,g!name)
     (with-pandoric (jobs) #',to
       (setf (gethash ,g!name jobs) ,g!job))))

(defmacro! remove-job (o!name from)
  "removes job o!name from pandoric 'from'; errors if not already present"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to remove job ~a that is not present~%" ,g!name)
     (with-pandoric (jobs) #',from
       (remhash ,g!name jobs))))

(defmacro! job-present-p (o!name in)
  "if job o!name is present in pandoric 'in'"
  `(with-pandoric (jobs) #',in
     (key-present ,g!name jobs)))

(defmacro! activate-job (o!name from)
  "activates job o!name from pandoric 'from'; errors if not present, or present and already active"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to activate job ~a that is not present" ,g!name)
     (with-pandoric (jobs) #',from
       (with-pandoric (active) (gethash ,g!name jobs)
	 (assert (not active) nil "tried to activate job ~a that is already activated~%" ,g!name)
	 (setf active t)))))

(defmacro! deactivate-job (o!name from)
  "deactivates job o!name from pandoric 'from'; errors if not present, or present and not already active"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to deactivate job ~a that is not present" ,g!name)
     (with-pandoric (jobs) #',from
       (with-pandoric (active) (gethash ,g!name jobs)
	 (assert active nil "tried to deactivate job ~a that is already deactivated~%" ,g!name)
	 (setf active nil)))))

(defmacro! job-active-p (o!name from)
  "returns t/nil if job o!name from pandoric 'from' is active; errors if not present"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to check active flag for job ~a that is not present" ,g!name)
     (with-pandoric (jobs) #',from
       (get-pandoric (gethash ,g!name jobs) 'active))))

;container to store a collection of jobs (in a hash table)
;returns a pandoric function that will loop through all the jobs and 
;execute the functions that have been latched to each job
;FIXME; this macro needs some work
(defmacro define-agent (&key (name) (socket))
  `(alet% ((jobs (make-hash-table :test #'equalp))
	   (socket ,socket))
	  (if socket
	      (add-job (define-job :name (format nil "\"~a\"" (symbol-name ',name)) 
			     :quota 1 
			     :quotaFn socket) 
			   ,name))
	  (push-to-end ',name (get-pandoric #'agents 'agents))
	  (defpun ,name () (jobs socket)
	    (loop for job being the hash-values of jobs
	       do (funcall job)))))

(defmacro get-socket (agent)
  `(get-pandoric #',agent 'socket))

(defmacro get-bsd-stream (agent)
  `(get-pandoric (get-socket ,agent) 'bsd-stream))

(defmacro get-data (agent)
  `(get-pandoric (get-socket ,agent) 'data))

(defmacro get-channel (agent channelName)
  `(gethash ,channelName (get-data ,agent)))

(defmacro get-datum (agent channelName)
  `(car (get-channel ,agent ,channelName)))

(defmacro! add-channel% (toAgent fromAgent o!name quota)
  "attaches a channel from 'from' to 'to' stream"
  `(add-job
    (define-job :name ,g!name :quota ,quota
		    :quotaFn (lambda ()
			       (aif (get-datum ,fromAgent ,g!name)
				    (uni-send-string (get-bsd-stream ,toAgent) 
						     (format nil "~a=~a~%" ,g!name it)))))
    ,toAgent))
(defmacro add-channel (&key (toAgent) (fromAgent) (name) (quota))
  `(add-channel% ,toAgent ,fromAgent ,name ,quota))
  
(defmacro! add-transform% (agent o!name quota o!fromName xferFn)
  "adds a channel to data who's value is a function of another channel; xfer-fn defines the transfer function (f(x); x is from, f(x) is to)"
  `(add-job
    (define-job :name ,g!name :quota ,quota
		    :quotaFn (lambda ()
			       ;anaphor b/c we're injecting a xferFn that has an 'x' defined
			       (let ((x (get-datum ,agent ,g!fromName))) 
				 (if x
				     (push ,xferFn (get-channel ,agent ,g!name))))))
    ,agent))
(defmacro add-transform (&key (agent) (name) (quota) (fromName) (xferFn))
  `(add-transform% ,agent ,name ,quota ,fromName ,xferFn))

(defmacro! print-agent (o!agent)
  "print agent 'agent'"
  `(with-pandoric (jobs) (symbol-function ,g!agent)
     (loop for job being the hash-values of jobs
	do (with-pandoric (name active quot quota) job
	     (format t "from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%" ,g!agent name active quot quota)))))

(defmacro! print-agents ()
  "prints all of the agents that have been created"
  `(dolist (,g!agent (get-pandoric #'agents 'agents))
     (print-agent ,g!agent)))

(defmacro update-agent (agent)
  "updates agent 'agent'"
  `(funcall (symbol-function ,agent)))

(defmacro! update-agents ()
  "updates all of the agents that have been created"
  `(dolist (,g!agent (get-pandoric #'agents 'agents))
     (update-agent ,g!agent)))
     
;top-level function that runs the lisp backend server
(defun run-server ()
  ;agent in charge of all jobs concerning the DAQ (that is, the DAQ->lisp bridge)
  (define-agent :name DAQ 
    :socket (make-socket :bsd-stream 
			 (make-string-input-stream
			  (with-output-to-string (out)
			    (format out "RPM-Raw=5~%") ;typical look of a single line in the DAQ strem
			    (format out "(print \"hello world\")~%") ;but, you can also send lisp code to get evaled in place
			    (format out "[QUIT]~%") ;and, when you want to close the channel, just send this string
			    ))))
  
  ;agent in charge of all jobs concerning the display (that is, the lisp->os x bridge)
  (multiple-value-bind (bsd-stream bsd-socket) (uni-prepare-socket "127.0.0.1" 9557)
    (define-agent :name display 
      :socket (make-socket :bsd-stream bsd-stream
			   :bsd-socket bsd-socket)))

  ;;;define the jobs for the DAQ agent
  
  ;for kicks, lets do a linear transformation on one of the channels in the DAQ
  (add-transform :agent DAQ 
		 :name "RPM" 
		 :fromName "RPM-Raw" 
		 :xferFn (+ x (* x 100) 5)
		 :quota 4)
  ;this pattern can take care of calibration signals sent from OSX 
  (add-transform :agent DAQ
		 :name "RPM-Special" 
		 :fromName "RPM-Raw" 
		 :xferFn (* x (aif (get-datum display "RPM-xfer-m") it 1))
		 :quota 2)

  ;;;define the jobs for the display agent

  ;then initialize any OSX outgoings
  (add-channel :toAgent display
	       :fromAgent DAQ
	       :name "RPM"
	       :quota 1)
  
  (add-channel :toAgent display
	       :fromAgent DAQ
	       :name "RPM-Raw"
	       :quota 2)
  
  (add-channel :toAgent display
	       :fromAgent DAQ
	       :name "RPM-Special"
	       :quota 1)
  
  ;display all agents and their jobs that will be called each time 'update-call' is called
  (print-agents) 

  ;call 'update-all' a bunch of times; this will eventually turn into the 'while' loop 
  ;that keeps looping forever; checking for new agents that want to sign on... letting agents
  ;sign off; adding jobs to different agents, and running all the jobs, etc.
  (time
   (with-pandoric (bsd-socket bsd-stream) (get-socket display)
     ;just looping through N times for now; eventually, this will be looped until os x signals an exit
     (let ((i 0))
       (while (and (< (incf i) 50) bsd-socket (sb-bsd-sockets:socket-open-p bsd-socket))
	 (update-agents)))
     ;telling the os x client to exit
     (uni-send-string bsd-stream (format nil "[QUIT]~%"))
     ;ox x will receive the exit signal, and send one back (letting the server know that it's going to kill itself)
     ;we wait until the client sends the exit signal back, and then exit the server
     (while (and bsd-socket (sb-bsd-sockets:socket-open-p bsd-socket))
       (funcall (get-socket display))))))



	



