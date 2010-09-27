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
  `(* ,g!x ,g!x))

(defun linear-regression (xs ys)
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
      do (if (> (length ,g!channel) ,N)
	     (setf ,g!channel (nbutlast ,g!channel)))))

(defmacro define-job (&key (name) (quota 1) (Fn) (active-p 't))
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

(defmacro make-socket (&key (bsd-stream) (bsd-socket))
  "defines a socket that is a pandoric function
   the function has an inner loop that processes all lines currently on the stream;
   if a line is '[QUIT]', it will close the stream;
   if a line is of the form 'a=b', it will push the evaled value 'b' onto the channel 'a' in data
   otherwise, it will eval the line in place"
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

(defmacro! add-job (&key (job) (agent))
  "adds job o!job to pandoric 'to'; errors if already present"
  `(let* ((,g!job ,job)
	  (,g!name (get-pandoric ,g!job 'name)))
     (assert (not (job-present-p ,g!name ,agent)) nil "tried to add job ~a that is already present~%" ,g!name)
     (with-pandoric (jobs) ',agent
       (setf (gethash ,g!name jobs) ,g!job))))

(defmacro! remove-job (o!name from)
  "removes job o!name from pandoric 'from'; errors if not already present"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to remove job ~a that is not present~%" ,g!name)
     (with-pandoric (jobs) ',from
       (remhash ,g!name jobs))))

(defmacro job-present-p (name in)
  "if job o!name is present in pandoric 'in'"
  `(with-pandoric (jobs) ',in
     (key-present ',name jobs)))

(defmacro! activate-job (o!name from)
  "activates job o!name from pandoric 'from'; errors if not present, or present and already active"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to activate job ~a that is not present" ,g!name)
     (with-pandoric (jobs) ',from
       (with-pandoric (active) (gethash ,g!name jobs)
	 (assert (not active) nil "tried to activate job ~a that is already activated~%" ,g!name)
	 (setf active t)))))

(defmacro! deactivate-job (o!name from)
  "deactivates job o!name from pandoric 'from'; errors if not present, or present and not already active"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to deactivate job ~a that is not present" ,g!name)
     (with-pandoric (jobs) ',from
       (with-pandoric (active) (gethash ,g!name jobs)
	 (assert active nil "tried to deactivate job ~a that is already deactivated~%" ,g!name)
	 (setf active nil)))))

(defmacro! job-active-p (o!name from)
  "returns t/nil if job o!name from pandoric 'from' is active; errors if not present"
  `(progn
     (assert (job-present-p ,g!name ,from) nil "tried to check active flag for job ~a that is not present" ,g!name)
     (with-pandoric (jobs) ',from
       (get-pandoric (gethash ,g!name jobs) 'active))))

(defmacro run-job (name agent)
  "short-circuits through the quota run-jobs loop, and just executes the job once"
  `(progn
     (assert (job-present-p ,name ,agent) nil "tried to run job ~a that is not present~%" ',name)
     (with-pandoric (jobs) ',agent
       (with-pandoric (Fn) (gethash ',name jobs)
	 (funcall Fn)))))

(defmacro define-agent (&key (name) (socket))
  "container to store a collection of jobs (in a hash table)
   defines a pandoric function that will loop through all the jobs and 
   execute the functions that have been latched to each job"
  `(let ((jobs (make-hash-table))
	 (socket ,socket))
     (defpun ,name () (jobs socket)
       (loop for job being the hash-values of jobs
	  do (funcall job)))
     (if socket
	 (add-job :agent ,name
		  :job (define-job :name ,name
			 :quota 1 
			 :Fn socket)))
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

(defmacro get-channel (channel agent &key (N))
  "returns a channel (or subset of the channel) in the data hash table inside the agent's socket"
  (setf channel `(gethash ,(symbol-name channel) (get-data ,agent)))
  (cond ((not N)
	 channel)
	((equal N 1)
	 `(car ,channel))
	(t
	 `(subseq ,channel 0 (min ,N (length ,channel))))))

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
  
(defmacro add-output (&key (agent) (name) (quota) (value))
  "adds an output to agent that sends 'value' through the socket"
  `(add-job :agent ,agent
	    :job (define-job :name ,name :quota ,quota
			     :Fn (lambda ()
				   (aif ,value
					(uni-send-string (get-bsd-stream ,agent)
							 (format nil "~a=~a~%" ,(symbol-name name) it)))))))

(defmacro add-channel (&key (agent) (name) (quota) (value))
  "adds channel to agent that evaluates value"
  `(add-job :agent ,agent
	    :job (define-job :name ,name :quota ,quota
			     :Fn (lambda ()
				   (aif ,value
					(push it (get-channel ,name ,agent)))))))

(defmacro! print-agent (o!agent)
  "print agent 'agent'"
  `(with-pandoric (jobs) (symbol-function ,g!agent)
     (loop for job being the hash-values of jobs
	do (with-pandoric (name active quot quota) job
	     (format t "from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%" ,g!agent name active quot quota)))))

(defmacro! print-agents ()
  "prints all of the agents that have been created"
  `(dolist (,g!agent (get-pandoric 'agents 'agents))
     (print-agent ,g!agent)))

(defmacro update-agent (agent)
  "updates agent 'agent'"
  `(funcall (symbol-function ,agent)))

(defmacro! update-agents ()
  "updates all of the agents that have been created"
  `(dolist (,g!agent (get-pandoric #'agents 'agents))
     (update-agent ,g!agent)))

(let ((agents))
  (defpun agents () (agents)
    ()))

(defun compile-server ()
  (compile-file "server.lisp" :output-file "server.fasl"))
     
(defun run-server ()
  "top-level function that runs the lisp backend server"


  ;agent in charge of all jobs concerning the DAQ (that is, the DAQ->lisp bridge)
  (define-agent :name DAQ 
    :socket (make-socket :bsd-stream 
			 (make-string-input-stream
			  (with-output-to-string (out)
			    (format out "RPM-Raw=5~%") ;typical look of a single line in the DAQ strem
			    (format out "(print \"hello world\")~%") ;but, you can also send lisp code to get evaled in place
			    (format out "[QUIT]~%") ;and, when you want to close the channel, just send this string
			    ))
			 ))

  
  ;agent in charge of all jobs concerning the display (that is, the lisp->os x bridge)
  (multiple-value-bind (bsd-stream bsd-socket) (uni-prepare-socket "127.0.0.1" 9557)
    (define-agent :name display 
      :socket (make-socket :bsd-stream bsd-stream
			   :bsd-socket bsd-socket)))

  ;;;define the jobs for the DAQ agent

  

  ;add a calibrated rpm channel to the daq; uses the rpm-intercept and rpm-slope daq channels to perform
  ;the linear transformation from the rpm-raw signal
  ;these rpm-intercept and rpm-slope channels are updated (recalibrated) every time the rpm-regression
  ;job is run
  (add-channel :agent DAQ
	       :name RPM
	       :value (with-channels ((rpm-raw DAQ :N 1) (rpm-intercept DAQ :N 1) (rpm-slope DAQ :N 1))
			(+ (aif rpm-intercept it 0) (* (aif rpm-slope it 1) rpm-raw)))
	       :quota 4)


  ;;;define the jobs for the display agent

  ;add a job that the display agent can run; the job pushes an updated slope and intercept on the
  ;daq slope/intercept channels
  (add-job :agent display
	   :job (define-job :name RPM-regression :active-p nil
			    :Fn (lambda ()
				  (with-channels ((rpm-measured display :N 30) (rpm-displayed display :N 30)
						  (rpm-slope DAQ :N 1) (rpm-intercept DAQ :N 1))
				    ;we need to convert the rpm-displayed back to raw data scale
				    (if (and rpm-intercept rpm-slope)
					(setf rpm-displayed (mapcar (lambda (y) (/ (- y rpm-intercept) rpm-slope)) rpm-displayed)))
				    ;then grab the updated slope and intercept, and push them on to the slope/intercept daq channel
				    (when (> (length rpm-displayed) 5)
				      (multiple-value-setq (rpm-slope rpm-intercept) (linear-regression rpm-measured rpm-displayed))
				      (push rpm-slope (get-channel rpm-slope DAQ))
				      (push rpm-intercept (get-channel rpm-intercept DAQ)))))))
  
  ;send the calibrated rpm signal to the display
  (add-output :agent display 
	      :name RPM
	      :value (get-channel RPM DAQ :N 1)
	      :quota 1)
  
  ;send the raw rpm signal to the display as well
  (add-output :agent display
	      :name RPM-Raw
	      :value (get-channel RPM-Raw DAQ :N 1)
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
       (while (and (< (incf i) 120) bsd-socket (sb-bsd-sockets:socket-open-p bsd-socket))
	 (update-agents)))
     ;telling the os x client to exit
     (uni-send-string bsd-stream (format nil "[QUIT]~%"))
     ;ox x will receive the exit signal, and send one back (letting the server know that it's going to kill itself)
     ;we wait until the client sends the exit signal back, and then exit the server
     (while (and bsd-socket (sb-bsd-sockets:socket-open-p bsd-socket))
       (funcall (get-socket display))))))



	



