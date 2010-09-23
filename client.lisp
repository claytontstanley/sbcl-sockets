(load "uni-files.lisp")

(defvar sock nil)
(defvar strm nil)
(defvar line nil)

(defvar cnt 0)

(defun run-client ()
  (multiple-value-setq (strm sock) (uni-make-socket "127.0.0.1" 9557))
  (while (and sock (sb-bsd-sockets:socket-open-p sock))
    (format t "socket open~%")
    (while (and sock (sb-bsd-sockets:socket-open-p sock) (listen strm))
      (format t "listening~%")
      (format t "~a~%" (setf line (read-line strm)))
      (uni-send-string strm (format nil "RPM-xfer-m=~a~%" (random 10)))
      (when (string-equal line "[QUIT]")
	(uni-send-string strm (format nil "[QUIT]~%"))
	(sb-bsd-sockets::close strm)
	(sb-bsd-sockets:socket-close sock)))))
      
  







