(defun run-display ()
  "connects a display agent to the server"
  (let ((bsd-socket) (bsd-stream) (line) (host *server-ip*) (port 9557))
    (multiple-value-setq (bsd-stream bsd-socket) (uni-connect host port))
    (while (uni-stream-active-p bsd-stream)
      (with-time *seconds-per-update*
	(while (and (uni-stream-active-p bsd-stream) (listen bsd-stream))
	  (setf line (uni-read-line bsd-stream))
	  (format t "received on ~a:~a: ~a~%" host port line)
	  (aif (line2element line)
	       (if (equalp (car it) "rpm-raw")
		   (uni-send-string bsd-stream (format nil "rpm-measured=~f" 
						       (+ (parse-float (cdr it)) 
							  (- (/ (random 1000) 10000) .05))))))
	  (when (string-equal line "[QUIT]")
	    (uni-send-string bsd-stream "[QUIT]")
	    (uni-close bsd-stream bsd-socket)))))))