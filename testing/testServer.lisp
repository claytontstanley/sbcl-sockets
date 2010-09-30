(define-agent :name the-managers)

(defmacro get-agent (names &optional (strings))
  (if (not strings) (setf strings (mapcar #'symbol-name names)))
  `(progn
     (define-agent :name the-managers)
     ,@(mapcar (lambda (name string)
		 `(add-job :agent the-managers :job (define-job :name ,name :Fn (lambda () (format t "~a~%" ,string)))))
		 names strings)))

(defmacro! errors-p (form)
  `(handler-case
       (progn
	 ,form
	 nil)
     (error (,g!condition) (declare (ignore ,g!condition)) t)))

(defmacro check-for-error (names form should-error)
  `(progn
     (get-agent ,names)
     (check (equal (errors-p ,form) ,should-error))))

(deftest test-define-job ()
  (macrolet ((test-lambdas (quota name)
	       `(let* ((quotaStr (format nil "atQuota~%"))
		       (quotaFun (lambda () (format t quotaStr)))
		       (fun (define-job :name ,name :quota ,quota :Fn quotaFun)))
		  (with-pandoric (quota) fun
		    (check (equal quota ,quota)))
		  (check
		   (string-equal
		    (with-output-to-string (*standard-output*)
		      (dotimes (i (* ,quota 10)) 
			(funcall fun)))
		    (format nil "~{~a~}" (make-list 10 :initial-element quotaStr))))
		  (with-pandoric (quot) fun
		    (dotimes (i ,quota)
		      (check (equal quot i))
		      (with-output-to-string (*standard-output*) (funcall fun))))
		  (with-pandoric (active name quot quota) fun
		    (check
		     (equal quot 0)
		     (equal quota ,quota)
		     active
		     (equal name ',name))))))
    (test-lambdas 1 hello)
    (test-lambdas 2 world)
    (test-lambdas 40 with-space)))

(deftest test-add-job ()
  (macrolet ((check-added (names)
	       `(progn
		  (get-agent ,names)
		  ,@(mapcar (lambda (name) `(check (job-present-p ,name the-managers))) names)))
	     (check-for-error (names should-error)
	       `(check
		 (equal ,should-error
			(errors-p (get-agent ,names))))))
    (check-added (test))
    (check-added ())
    (check-added (name2 name))
    (check-for-error (name name) t)
    (check-for-error (name name2) nil)
    (check-for-error (name name2) nil)))

(deftest test-remove-job ()
  (macrolet ((check-removed (remove names)
	       `(progn
		  (get-agent ,names)
		  (remove-job ,remove the-managers)
		  (check (not (job-present-p ,remove the-managers))))))
    (check-removed name (name name2))
    (check-removed name2 (name2))
    (check-for-error () (remove-job name the-managers) t)
    (check-for-error (name) (remove-job name2 the-managers) t)
    (check-for-error (name1 name3) (remove-job name3 the-managers) nil)))

(deftest test-job-present-p ()
  (macrolet ((check-present-p (present names present-p)
	       `(progn
		  (get-agent ,names)
		  (check (equal 
			  (job-present-p ,present the-managers) 
			  ,present-p)))))
    (check-present-p name (name name2) t)
    (check-present-p name (name2 name3 name) t)
    (check-present-p name (name2 name3) nil)
    (check-present-p name () nil)))

(deftest test-activate-deactivate-job ()
  (macrolet ((check-activate-deactivate (names)
	       `(progn
		  (get-agent ,names)
		  ,@(mapcar (lambda (name) `(check (equal (job-active-p ,name the-managers) t))) names)
		  ,@(mapcar (lambda (name) `(deactivate-job ,name the-managers)) names)
		  ,@(mapcar (lambda (name) `(check (equal (job-active-p ,name the-managers) nil))) names))))
    (check-activate-deactivate (name name2 name3))
    (check-activate-deactivate ())
    (check-activate-deactivate (name))
    (check-for-error (name name2) (deactivate-job name2 the-managers) nil)
    (check-for-error (name name2) (deactivate-job name3 the-managers) t)
    (check-for-error (name name2) (progn
				    (deactivate-job name2 the-managers)
				    (deactivate-job name2 the-managers)) t)
    (check-for-error (name name2) (activate-job name the-managers) t)
    (check-for-error (name name2) (activate-job name3 the-managers) t)))

(deftest test-job-active-p ()
  (macrolet ((check-active-p (names form active-p)
	       `(progn
		  (get-agent ,names)
		  (check (equal ,active-p ,form)))))
    (check-active-p (name name2) (job-active-p name the-managers) t)
    (check-active-p (name name2) (progn (deactivate-job name the-managers)
					(job-active-p name the-managers)) nil)
    (check-for-error (name name2) (job-active-p name the-managers) nil)
    (check-for-error (name) (job-active-p name2 the-managers) t)))
     
(deftest test-run-job ()
  (macrolet ((check-run-job (name)
	       `(progn
		  (get-agent (,name))
		  (check (equal (with-output-to-string (*standard-output*)
				  (run-job ,name the-managers))
				(format nil "~a~%" ,(symbol-name name)))))))
    (check-run-job name)
    (check-run-job name2)
    (check-run-job name3)
    (check-for-error (name name2) (run-job name2 the-managers) nil)
    (check-for-error () (run-job name3 the-managers) t)
    (check-for-error (name name2) (run-job name3 the-managers) t)))

(deftest test-define-agent ()
  (macrolet ((test-definition (names strings)
	       `(progn
		  (get-agent ,names ,strings)
		  (check
		   (string-equal
		    (with-output-to-string (*standard-output*)
		      (the-managers))
		    (format nil "~{~a~%~}" ',strings))))))
    (test-definition (name name2) ("hello" "hello2"))
    (test-definition (name) ("hello2"))
    (test-definition (name1 name2 name3) ("output1" "output2" "output3"))
    (test-definition nil nil)))

(deftest test-print-agent ()
  (macrolet ((test-single (names strings)
	       `(progn
		  (get-agent ,names ,strings)
		  (check
		   (string-equal
		    (with-output-to-string (*standard-output*)
		      (print-agent the-managers))
		    (format nil "~{from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%~}" 
			    (flatten 
			     (mapcar (lambda (a b c d e) (list a b c d e))
				     (make-list 2 :initial-element 'the-managers)
				     ',names
				     (make-list 2 :initial-element t)
				     (make-list 2 :initial-element 0)
				     (make-list 2 :initial-element 1)))))))))
    (test-single (name1 name2) ("output1" "output2"))
    (test-single (name1) ("output"))
    (test-single () ())))

(deftest test-print-agents ()
  (macrolet ((check-print-agents (names)
	       `(progn
		  (with-pandoric (agents) 'agents
		    (setf agents nil))
		  (get-agent ,names)
		  (check
		   (equal
		    ,(length names)
		    (length (get-lines (with-output-to-string (*standard-output*) (print-agents)))))))))
    (check-print-agents (one two three four))
    (check-print-agents ())
    (check-print-agents (hello))))
	       

(defun test-server ()
  (let ((result
	 (runtests 
	  (test-define-job)
	  (test-define-agent)
	  (test-print-agent)
	  (test-add-job)
	  (test-remove-job)
	  (test-job-present-p)
	  (test-activate-deactivate-job)
	  (test-job-active-p)
	  (test-run-job)
	  (test-print-agents)
	  )))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))