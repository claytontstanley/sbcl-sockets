(defun run-server ()
  "top-level function that runs the lisp main server"

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
		   :calibrated-channel (RPM DAQ)
		   :raw-channel (RPM-raw DAQ))
    
  ;send the calibrated rpm signal to the display
  (add-output-event :output-channel (RPM display) 
		    :trigger-channel (RPM DAQ))
  
  ;send the raw rpm signal to the display as well
  (add-output-event :output-channel (RPM-Raw display)
		    :trigger-channel (RPM-Raw DAQ))
  
  ;display all agents and their jobs that will be called each time 'update-call' is called
  (print-agents)

  ;run until all agents have been killed
  (run))
