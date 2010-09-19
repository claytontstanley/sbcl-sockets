(load "uni-files.lisp")

;finite state machine that executes functions at the specified time, when active
;you can pass functions to execute at each iteration (at quot)
;and after each time the quota is reached (at quota)
(defmacro define-manager (name quota atQuota &optional (atQuot))
  `(let ((quota ,quota)
	 (quot 0)
	 (name ,name)
	 (active t))
     (plambda () (name quot quota active)
       (if active
	   ,(if atQuot `(funcall ,atQuot)))
       (incf quot)
       (when (eq quot quota)
	 (if active
	     ,(if atQuota `(funcall ,atQuota)))
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

;print all the managers in the pandoric function 'from'
(defmacro print-managers (from)
  `(with-pandoric (managers) #',from
     (loop for manager being the hash-values of managers
	do (with-pandoric (name active quot quota) manager
	     (format t "from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%" ',from name active quot quota)))))

(defmacro! manager-present-p (o!name in)
"if manager o!name is present in pandoric 'in'"
  `(with-pandoric (managers) #',in
     (key-present ,g!name managers)))

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

;update-DAQ is responsible for storing all of the raw data from the DAQ,
;as well as storing all of the transformed data derived from the raw data
(let (;stream that the data will be grabbed from
      (strm!)
      (sock!)
      ;hash table storing all of the data
      (data (make-hash-table :test #'equalp))
      ;maximum length of a channel (number of stored points) in the data hash table
      (N 100)) 
  (setf (symbol-function 'update-DAQ)
	(plambda () (strm! sock! data N)
	  ;update all of the raw data
	  (push (random 5) (gethash "RPM-Raw" data)))))

;update-OSX is responsible for storing all of the raw data from the OSX stream,
;as far as I can tell, the data from the OSX stream can just be 'evaled' in place,
;so we shouldn't have to store much here
(let (;stream that will be used to send & receive messages between the lisp backend & the OS X GUI
      (strm!)
      (sock!)
      ;data storing any received messages from the GUI
      (data (make-hash-table :test #'equalp)) 
      ;maximum length of a channel in the data hash table
      (N 100)) 
  (setf (symbol-function 'update-OSX)
	(plambda () (strm! sock! data N)
	  ;update all of the raw data
	  ;for these, if there's anything new on the strm, we just eval it
	  (while (and sock! (sb-bsd-sockets::socket-open-p sock!) (listen strm!))
	    (let ((line (uni-socket-read-line strm!)))
	      (if (string-equal line "[[QUIT]]")
		  (progn
		    (sb-bsd-sockets::close strm!)
		    (sb-bsd-sockets::socket-close sock!))
		  (eval (read-from-string line))))))))
              
;;;you should only need to access functions/macros below to do stuff

(defmacro add-display (&rest rest)
  `(add-manager
    (define-manager ,@rest)
    update-display))

(defmacro add-data (&rest rest)
  `(add-manager
    (define-manager ,@rest)
    update-data))

(defmacro remove-display (name)
  `(remove-manager ,name update-display))

(defmacro remove-data (name)
  `(remove-manager ,name update-data))

(defmacro print-display ()
  `(print-managers update-display))

(defmacro print-data ()
  `(print-managers update-data))

(defmacro activate-display (name)
  `(activate-manager ,name update-display))

(defmacro activate-data (name)
  `(activate-manager ,name update-data))

(defmacro deactivate-display (name)
  `(deactivate-manager ,name update-display))

(defmacro deactivate-data (name)
  `(deactivate-manager ,name update-data))

(defmacro data-active-p (name)
  `(manager-active-p ,name update-data))

(defmacro display-active-p (name)
  `(manager-active-p ,name update-display))

(defmacro print-all ()
  `(progn
     (print-display)
     (print-data)))

(defmacro update-all ()
  `(progn
     (update-data)
     (update-display)))

(defmacro! add-display-from-data (o!name quota)
  "attaches a channel from data to the display stream"
  `(add-display ,g!name ,quota
		(lambda ()
		  (with-pandoric (strm!) #'update-OSX
		    (with-pandoric (data) #'update-DAQ
		      (let ((,g!x (car (gethash ,g!name data))))
			  (if ,g!x
			      (uni-send-string strm! (format nil "~a=~a~%" ,g!name ,g!x)))))))))

(defmacro! add-data-transform (o!to quota o!from xfer-fn)
  "adds a channel to data who's value is a function of another channel; xfer-fn defines the transfer function (f(x); x is from, f(x) is to)"
  `(add-data ,g!to ,quota
	     (lambda ()
	       (with-pandoric (data) #'update-DAQ
		 (let ((x (car (gethash ,g!from data))))
		   (if x
		       (push ,xfer-fn (gethash ,g!to data))))))))

;top-level function that runs the lisp backend server
(defun run-server ()
  ;we need to somehow initialize the DAQ-strm
  (with-pandoric (strm!) #'update-DAQ
    (setf strm! t))
  ;initialize the OSX-strm
  (with-pandoric (strm! sock!) #'update-OSX
    (multiple-value-setq (strm! sock!) (uni-prepare-socket "127.0.0.1" 9557)))
    
  ;first all raw DAQ outgoings will be updated
  (add-data "update-DAQ" 1 #'update-DAQ)

  ;chain on any other functions to further process the raw data
  ;for kicks, lets do a linear transformation on one of the channels in the DAQ
  (add-data-transform "RPM" 4 "RPM-Raw" (+ x (* x 100) 5))
  (add-data-transform "RPM-Special" 2 "RPM" (* x (* x (* x 100))))
  ;last thing we should do is trim all of the data
  (add-data "trim" 1 (lambda () (with-pandoric (data N) #'update-DAQ (trim-data data N))))

  ;first all raw OSX incomings will be updated; these just get evaled
  (add-display "update-OSX" 1 #'update-OSX)
  ;then initialize all OSX outgoings
  (add-display-from-data "RPM" 1)
  (add-display-from-data "RPM-Raw" 2)
  (add-display-from-data "RPM-Special" 1)
  (add-display "trim" 1 (lambda () (with-pandoric (data N) #'update-OSX (trim-data data N))))

  (print-all) ;print the data and display managers that will be called each time 'update-call' is called

  (time
   (with-pandoric (sock! strm!) #'update-OSX
     ;just looping through N times for now; eventually, this will be looped until os x signals an exit
     (let ((i 0))
       (while (and (< (incf i) 5000) sock! (sb-bsd-sockets::socket-open-p sock!))
	 (update-all)))
     ;telling the os x client to exit
     (uni-send-string strm! (format nil "[[QUIT]]~%"))
     ;ox x will receive the exit signal, and send one back (letting the server know that it's going to kill itself)
     ;we wait until the client sends the exit signal back, and then exit the server
     (while (and sock! (sb-bsd-sockets::socket-open-p sock!))
       (funcall #'update-OSX)))))



	



