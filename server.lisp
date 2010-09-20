(load "uni-files.lisp")

;DEVS (discreet event simulator) that executes functions at the specified time, 
;when active, you can pass functions to execute at each iteration (at quot)
;and after each time the quota is reached (at quota)
;the functioned returned is pandoric, so you can access/change state variables
(defmacro define-manager (name quota atQuota &optional (atQuot))
  `(let ((quota ,quota)
	 (quot 0)
	 (name ,name)
	 (active t))
     (plambda () (name quot quota active)
       (if active
	   (aif ,atQuot (funcall it)))
       (incf quot)
       (when (eq quot quota)
	 (if active
	     (aif ,atQuota (funcall it)))
	 (setf quot 0)))))

;container to store a collection of managers (in a hash table)
;returns a pandoric function that will loop through all the managers and 
;execute the functions that have been latched to each manager
(defmacro define-managers (name)
  `(let ((managers (make-hash-table :test #'equalp)))
     (setf (symbol-function ',name)
	   (plambda () (managers)
	     (loop for manager being the hash-values of managers
		do (funcall manager))))))

(define-managers update-display) ;container that holds all functions that update the display (that is, the lisp->os x bridge)
(define-managers update-data) ;container that holds all functions that update the data (that is, the DAQ->lisp bridge)

(defmacro print-managers (from)
  "print all the managers in the pandoric function 'from'"
  `(with-pandoric (managers) #',from
     (loop for manager being the hash-values of managers
	do (with-pandoric (name active quot quota) manager
	     (format t "from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%" ',from name active quot quota)))))

(defmacro! add-manager (o!manager to)
  "adds manager o!manager to pandoric 'to'; errors if already present"
  `(let ((,g!name (get-pandoric ,g!manager 'name)))
     (assert (not (manager-present-p ,g!name ,to)) nil "tried to add manager ~a that is already present~%" ,g!name)
     (with-pandoric (managers) #',to
       (setf (gethash ,g!name managers) ,g!manager))))
		     
(defmacro! remove-manager (o!name from)
  "removes manager o!name from pandoric 'from'; errors if not already present"
  `(progn
     (assert (manager-present-p ,g!name ,from) nil "tried to remove manager ~a that is not present~%" ,g!name)
     (with-pandoric (managers) #',from
       (remhash ,g!name managers))))

(defmacro! manager-present-p (o!name in)
  "if manager o!name is present in pandoric 'in'"
  `(with-pandoric (managers) #',in
     (key-present ,g!name managers)))

(defmacro! activate-manager (o!name from)
  "activates manager o!name from pandoric 'from'; errors if not present, or present and already active"
  `(progn
     (assert (manager-present-p ,g!name ,from) nil "tried to activate manager ~a that is not present" ,g!name)
     (with-pandoric (managers) #',from
       (with-pandoric (active) (gethash ,g!name managers)
	 (assert (not active) nil "tried to activate manager ~a that is already activated~%" ,g!name)
	 (setf active t)))))

(defmacro! deactivate-manager (o!name from)
  "deactivates manager o!name from pandoric 'from'; errors if not present, or present and not already active"
  `(progn
     (assert (manager-present-p ,g!name ,from) nil "tried to deactivate manager ~a that is not present" ,g!name)
     (with-pandoric (managers) #',from
       (with-pandoric (active) (gethash ,g!name managers)
	 (assert active nil "tried to deactivate manager ~a that is already deactivated~%" ,g!name)
	 (setf active nil)))))

(defmacro! manager-active-p (o!name from)
  "returns t/nil if manager o!name from pandoric 'from' is active; errors if not present"
  `(progn
     (assert (manager-present-p ,g!name ,from) nil "tried to check active flag for manager ~a that is not present" ,g!name)
     (with-pandoric (managers) #',from
       (get-pandoric (gethash ,g!name managers) 'active))))

(defmacro! trim-data (data N)
  "trims the ends off all channels in the data hash table, so that no channel is > N in length"
  `(loop for ,g!channel being the hash-values of ,data
      do (if (> (length ,g!channel) ,N)
	     (setf ,g!channel (nbutlast ,g!channel)))))

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

;defines a stream that is a pandoric function
;the function has an inner loop that processes all lines currently on strm!
;if a line is "[QUIT]", it will close the stream
;if a line is of the form "a=b", it will push the evaled value 'b' onto the channel 'a' in data
;otherwise, it will eval the line in place
(defmacro define-stream (name)
  `(let (;stream that will be used to send & receive messages between the lisp backend & the stream
	 (strm!)
	 (sock!)
         ;data storing any received messages
	 (data (make-hash-table :test #'equalp)) 
         ;maximum length of a channel in the data hash table
	 (N 100)) 
     (setf (symbol-function ',name)
	   (plambda () (strm! sock! data N)
	     (trim-data data N)
	     (let ((line))
	       ;update all of the raw data
	       (while (handler-case (listen strm!) 
			(error (condition) (declare (ignore condition)) nil))
		 (setf line (uni-socket-read-line strm!))
		 (acond ((string-equal line "[QUIT]")
			 (if strm! (sb-bsd-sockets::close strm!))
			 (if sock! (sb-bsd-sockets::socket-close sock!)))
			((line2element line)
			 (push (eval (read-from-string (cdr it))) (gethash (car it) data)))
			(t
			 (eval (read-from-string line))))))))))

;update-OSX is responsible for storing all of the raw data from the OSX stream
(define-stream update-OSX)

;update-DAQ is responsible for storing all of the raw data from the DAQ,
;as well as storing all of the transformed data derived from the raw data
(define-stream update-DAQ)

(defmacro get-datum (stream channelName)
  "gets most recent datapoint in channel 'channel' in data hash table for the stream 'stream'"
  `(with-pandoric (data) #',stream
     (car (gethash ,channelName data))))

(defmacro get-channel (stream channelName)
  "gets channel 'channel' in data hash table for the stream 'stram'"
  `(gethash ,channelName (get-pandoric #',stream 'data)))

(defmacro! add-to-from-that (to from o!name quota)
  "attaches a channel from 'from' data to 'to' stream"
  `(add-display ,g!name ,quota
		(lambda ()
		  (with-pandoric (strm!) #',to
		    (aif (get-datum ,from ,g!name)
			 (uni-send-string strm! (format nil "~a=~a~%" ,g!name it)))))))

(defmacro! add-transform (origin o!to quota o!from xfer-fn)
  "adds a channel to data who's value is a function of another channel; xfer-fn defines the transfer function (f(x); x is from, f(x) is to)"
  `(add-data ,g!to ,quota
	     (lambda ()
               ;anaphor b/c we're injecting a xfer-fn that has an 'x' defined
	       (let ((x (get-datum ,origin ,g!from))) 
		 (if x
		     (push ,xfer-fn (get-channel ,origin ,g!to)))))))
              
;;;you should only need to access functions/macros below to do stuff
(defmacro add-display (&rest rest)
  "adds a display manager"
  `(add-manager
    (define-manager ,@rest)
    update-display))

(defmacro add-data (&rest rest)
  "adds a data manager"
  `(add-manager
    (define-manager ,@rest)
    update-data))

(defmacro remove-display (name)
  "removes display manager 'name'"
  `(remove-manager ,name update-display))

(defmacro remove-data (name)
  "removes data manager 'name'"
  `(remove-manager ,name update-data))

(defmacro print-display ()
  "prints display managers"
  `(print-managers update-display))

(defmacro print-data ()
  "prints data managers"
  `(print-managers update-data))

(defmacro activate-display (name)
  "activates display manager 'name'"
  `(activate-manager ,name update-display))

(defmacro activate-data (name)
  "activates data manager 'name'"
  `(activate-manager ,name update-data))

(defmacro deactivate-display (name)
  "deactivates display manager 'name'"
  `(deactivate-manager ,name update-display))

(defmacro deactivate-data (name)
  "deactivates data manager 'name'"
  `(deactivate-manager ,name update-data))

(defmacro display-active-p (name)
  "returns t/f display manager 'name' is active"
  `(manager-active-p ,name update-display))

(defmacro data-active-p (name)
  "returns t/f data manager 'name' is active"
  `(manager-active-p ,name update-data))

(defmacro display-present-p (name)
  "returns t/f display manager 'name' is present"
  `(manager-present-p ,name update-display))

(defmacro data-present-p (name)
  "returns t/f data manager 'name' is present"
  `(manager-present-p ,name update-data))

(defmacro add-display-from-data (name quota)
  "attaches a data channel to the display stream"
  `(add-to-from-that update-OSX update-DAQ ,name ,quota))

(defmacro add-data-from-display (name quota)
  "attaches a display channel to the data stream"
  `(add-to-from-that update-DAQ update-OSX ,name ,quota))

(defmacro add-display-transform (to quota from xfer-fn)
  "transforms a display channel"
  `(add-transform update-OSX ,to ,quota ,from ,xfer-fn))

(defmacro add-data-transform (to quota from xfer-fn)
  "transforms a data channel"
  `(add-transform update-DAQ ,to ,quota ,from ,xfer-fn))

(defmacro get-display-datum (channel)
  "gets most recent datapoint in channel 'channel' in data hash table for the display (OSX) stream"
  `(get-datum update-OSX ,channel))

(defmacro get-data-datum (channel)
  "gets most recent datapoint in channel 'channel' in data hash table for the data (DAQ) stream"
  `(get-datum update-DAQ ,channel))

(defmacro print-all ()
  "prints all managers currently present"
  `(progn
     (print-display)
     (print-data)))

(defmacro update-all ()
  "updates all managers currently present"
  `(progn
     (update-data)
     (update-display)))

;top-level function that runs the lisp backend server
(defun run-server ()
  ;we need to somehow initialize the DAQ-strm
  ;this isn't quite correct yet, b/c the actual numbers that are sent from the DAQ are very raw (only numbers, no labels)
  ;we'll need to set up a separate strm that is connected to the DAQ, that processes the raw numbers, and then reroutes
  ;the results to this strm, in the "channelName=value~%" format
  (with-pandoric (strm!) #'update-DAQ
    (setf strm! 
	  (make-string-input-stream
	   (with-output-to-string (out)
	     (format out "RPM-Raw=5~%") ;typical look of a single line in the DAQ strem
	     (format out "(print \"hello world\")~%") ;but, you can also send lisp code to get evaled in place
	     (format out "[QUIT]~%") ;and, when you want to close the channel, just send this string
	     ))))
  ;initialize the OSX-strm
  (with-pandoric (strm! sock!) #'update-OSX
    (multiple-value-setq (strm! sock!) (uni-prepare-socket "127.0.0.1" 9557)))
    
  ;;;define the events for the data manager
  
  ;first the DAQ incomings wil be updated
  (add-data "update-DAQ" 1 #'update-DAQ)
  ;chain on any other functions to further process the raw data
  ;for kicks, lets do a linear transformation on one of the channels in the DAQ
  (add-data-transform "RPM" 4 "RPM-Raw" (+ x (* x 100) 5))
  ;this pattern can take care of calibration signals sent from OSX 
  (add-data-transform "RPM-Special" 2 "RPM-Raw" (* x (aif (get-display-datum "RPM-xfer-m") it 1)))



  ;then initialize any DAQ outgoings
  ;;;add-data-from-display ...


  ;;;define the events for the display manager

  ;first the OSX incomings will be updated
  (add-display "update-OSX" 1 #'update-OSX)
  ;chain on any other functions to further process the raw data
  ;add-display-transform...

  ;then initialize any OSX outgoings
  (add-display-from-data "RPM" 1)
  (add-display-from-data "RPM-Raw" 2)
  (add-display-from-data "RPM-Special" 1)

  
  (print-all) ;print the data and display managers that will be called each time 'update-call' is called

  (time
   (with-pandoric (sock! strm!) #'update-OSX
     ;just looping through N times for now; eventually, this will be looped until os x signals an exit
     (let ((i 0))
       (while (and (< (incf i) 5000) sock! (sb-bsd-sockets::socket-open-p sock!))
	 (update-all)))
     ;telling the os x client to exit
     (uni-send-string strm! (format nil "[QUIT]~%"))
     ;ox x will receive the exit signal, and send one back (letting the server know that it's going to kill itself)
     ;we wait until the client sends the exit signal back, and then exit the server
     (while (and sock! (sb-bsd-sockets::socket-open-p sock!))
       (funcall #'update-OSX)))))



	



