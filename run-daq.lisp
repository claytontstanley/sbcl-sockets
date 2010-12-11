(defvar *cnt* 0)

(defun run-daq ()
  "top-level function that runs the lisp daq server"

  ;agent in charge of all jobs that communicate with the DAQ through the modbus socket
  (define-modbus-agent :name modbus-DAQ
    :host 127.0.0.1
    :port 9555
    :type client)

  ;agent in charge of all jobs that relay modbus-DAQ values to the lisp server
  (define-agent :name DAQ
    :host 127.0.0.1
    :port 9556
    :type client)

  ;agent in charge of monitoring the system; 
  ;agent can send the server messages, query, execute remote procedure calls (RPCs) etc.
  (define-agent :name monitor
    :host 127.0.0.1
    :port 9554)

  (add-channel-job :channel (RPM-Raw modbus-DAQ)
		   :updates/second 60
		   :value (aif (get-channel read-registers-fn modbus-DAQ) 
			       (funcall it (mod (incf *cnt*) 10))))
  
  (add-output-event :output-channel (RPM-Raw DAQ)
		    :trigger-channel (RPM-Raw modbus-DAQ))

  (add-event :trigger-channel (send-to-modbus-DAQ DAQ)
	     :Fn (lambda () (aif (get-channel write-registers-fn modbus-DAQ)
				 (funcall it 1 (get-channel send-to-modbus-DAQ DAQ :N 1)))))

  ;display all agents and their jobs that will be called each time 'update-call' is called
  (print-agents)

  ;run until all agents have been killed
  (run))