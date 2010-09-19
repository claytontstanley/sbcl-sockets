(deftest test-define-manager ()
  (macrolet ((test-lambdas (quota name)
	       `(let* ((quotaStr (format nil "atQuota~%"))
		       (quotStr (format nil "atQuot~%"))
		       (quotaFun (lambda () (format t quotaStr)))
		       (quotFun (lambda () (format t quotStr)))
		       (fun (define-manager ,name ,quota quotaFun quotFun)))
		  (with-pandoric (quot quota) fun
		      (check (equal quota ,quota)))
		  (check
		   (string-equal
		    (with-output-to-string (*standard-output*)
		      (dotimes (i ,quota) 
			(funcall fun)))
		    (fast-concatenate (format nil "~{~a~}" (make-list ,quota :initial-element quotStr)) quotaStr)))
		  (with-pandoric (quot) fun
		    (dotimes (i ,quota)
		      (check (equal quot i))
		      (with-output-to-string (*standard-output*) (funcall fun))))
		  (with-pandoric (active name quot quota) fun
		    (check
		     (equal quot 0)
		     (equal quota ,quota)
		     active
		     (string-equal name ,name))))))
    (test-lambdas 1 "hello")
    (test-lambdas 2 "world")
    (test-lambdas 40 "with a space")))

(deftest test-define-managers ()
  (macrolet ((test-definition (names strings)
	       `(progn
		  (define-managers the-managers)
		  ,@(mapcar (lambda (name string)
			      `(add-manager (define-manager ,name 1 (lambda () (format t "~a~%" ,string))) the-managers))
			    names strings)
		  (check
		   (string-equal
		    (with-output-to-string (*standard-output*)
		      (the-managers))
		    (format nil "~{~a~%~}" ',strings))))))
    (test-definition ("name" "name2") ("hello" "hello2"))
    (test-definition ("name") ("hello2"))
    (test-definition ("name1" "name2" "name3") ("output1" "output2" "output3"))
    (test-definition nil nil)))

(defun test-server ()
  (let ((result
	 (runtests 
	  (test-define-manager)
	  (test-define-managers))))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))