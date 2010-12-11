(define-symbol-macro SCAN
    "sending DAQ attributes")

(defun run-labjack ()
  "top-level function that runs the lisp labjack server"

  ;agent in charge of all jobs that relay modbus-DAQ values to the lisp server
  (define-agent :name modbus-DAQ
    :host 127.0.0.1
    :port 9555)
    
  ;display all agents and their jobs that will be called each time 'update-call' is called
  (print-agents)

  ;run until all agents have been killed
  (run))
