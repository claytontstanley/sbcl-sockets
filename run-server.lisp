(defun run-server ()
  "top-level function that runs the lisp backend server"

  ;agent in charge of all jobs concerning the DAQ (that is, the DAQ->lisp bridge)
  (define-agent :name DAQ
    :host 10.0.1.4
    :port 9556
    ;:type client
    )

  ;agent in charge of all jobs concerning the display (that is, the lisp->OSX bridge)
  (define-agent :name display
    :host 10.0.1.4
    :port 9557)

  ;agent in charge of monitoring the server; 
  ;agent can send the server messages, query, execute remote procedure calls (RPCs) etc.
  (define-agent :name monitor
    :host 10.0.1.4
    :port 9558)

  ;add a job that queries the DAQ for the current RPM-Raw value, and places it on the RPM-Raw channel 
  (add-channel-job :channel (RPM-Raw DAQ)
		   :updates/second 1
		   :value 1)
		   ;:value (read-register (get-bsd-socket DAQ) 0)

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

  ;update-agents keeps getting called until there are no agents left to update
  ;agents can be removed by evaling (kill-agent agent), or (kill-agents)
  ;currently, the monitor sends a "(kill-agents)" RPC message to the server, which
  ;clears all the agents, which then causes this while loop to exit
  (while (get-pandoric 'agents 'agents)
    (with-time (/ 1 *updates-per-second*) ;updating 60x/second
      (update-agents))))
