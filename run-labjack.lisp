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

  ;update-agents keeps getting called until there are no agents left to update
  ;agents can be removed by evaling (kill-agent agent), or (kill-agents)
  ;currently, the monitor sends a "(kill-agents)" RPC message to the server, which
  ;clears all the agents, which then causes this while loop to exit
  (while (get-pandoric 'agents 'agents)
    (with-time (/ 1 *updates-per-second*) ;updating 60x/second
      (update-agents))))
