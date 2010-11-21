(define-agent :name the-managers)

(defmacro get-agent (names &key (strings) (agent `the-managers))
  (if (not strings) (setf strings (mapcar #'symbol-name names)))
  `(progn
     (define-agent :name ,agent)
     ,@(mapcar (lambda (name string)
		 `(add-job :agent ,agent :job (define-job :name ,name :Fn (lambda () (format t "~a~%" ,string)))))
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
		  (get-agent ,names :strings ,strings)
		  (check
		   (equal
		    (sort
		     (get-lines
		      (with-output-to-string (*standard-output*)
			(the-managers)))
		     #'string<)
		    (sort
		     (get-lines
		      (format nil "~{~a~%~}" ',strings))
		     #'string<))))))
    (test-definition (name name2) ("hello" "hello2"))
    (test-definition (name) ("hello2"))
    (test-definition (name1 name2 name3) ("output1" "output2" "output3"))
    (test-definition nil nil)))

(deftest test-print-agent ()
  (macrolet ((test-single (names strings)
	       (let ((size (+ 1 (length names))))
		 `(progn
		    (get-agent ,names :strings ,strings)
		    (check
		     (string-equal
		      (with-output-to-string (*standard-output*)
			(print-agent the-managers))
		      (format nil "~{from: ~a, name: ~a, active: ~a, quot/quota: ~a/~a~%~}" 
			      (flatten 
			       (mapcar #'list
				       (make-list ,size :initial-element 'the-managers)
				       '(the-managers ,@names)
				       (make-list ,size :initial-element t)
				       (make-list ,size :initial-element 0)
				       (make-list ,size :initial-element 1))))))))))
    (test-single (name1 name2) ("output1" "output2"))
    (test-single (name1) ("output"))
    (test-single () ())))

(deftest test-print-agents ()
  (macrolet ((check-print-agents (names)
	       (let ((size (+ 1 (length names)))) ;+1 for the socket job that's implicitly created
		 `(progn
		    (with-pandoric (agents) 'agents
		      (setf agents nil))
		    (get-agent ,names)
		    (check
		     (equal
		      ,size
		      (length (get-lines (with-output-to-string (*standard-output*) (print-agents))))))))))
    (check-print-agents (one two three four))
    (check-print-agents ())
    (check-print-agents (hello))))

(deftest test-update-agent ()
  (macrolet ((check-update-agent (names)
	       `(progn
		  (get-agent ,names)
		  (format t "~%~%~%")
		  (check
		   (equal
		    (sort
		     (get-lines
		      (with-output-to-string (*standard-output*)
			(update-agent the-managers)))
		     #'string<)
		    (sort
		     (get-lines
		      (format nil "~{~a~%~}" (list ,@(mapcar #'symbol-name names))))
		     #'string<))))))
    (check-update-agent (one two three four))
    (check-update-agent (four three one))
    (check-update-agent ())
    (check-update-agent (hello))))

(deftest test-update-agents ()
  (macrolet ((check-update-agents (names)
	       `(progn
		  (with-pandoric (agents) 'agents
		    (setf agents nil))
		  (get-agent ,names)
		  (check
		   (equal 
		    ,(length names)
		    (length (get-lines (with-output-to-string (*standard-output*) (update-agents)))))))))
    (check-update-agents (one two three four))
    (check-update-agents ())
    (check-update-agents (hello))))

(deftest test-kill-agent ()
  (macrolet ((check-kill-agent (agents agent)
	       `(progn
		  (with-pandoric (agents) 'agents
		    (setf agents nil))
		  ,@(mapcar (lambda (agent) `(get-agent (one two three four) :agent ,agent)) agents)
		  ,@(mapcar (lambda (agent) `(check (fboundp ',agent))) agents)
		  (kill-agent ,agent)
		  (check (not (fboundp ',agent)))
		  (check
		   (equal
		    (sort (mapcar #'symbol-name (get-pandoric 'agents 'agents)) #'string<)
		    (sort (mapcar #'symbol-name (remove-if
						 (lambda (x) (equal x ',agent))
						 (list ,@(mapcar (lambda (x) `',x) agents))))
			  #'string<)))
		  (kill-agents))))
    (check-kill-agent (agent1 agent2 agent3) agent1)
    (check-kill-agent (agent1 agent2) agent2)
    (check-kill-agent (agent2) agent2)
    (check (errors-p (check-kill-agent (agent2) agent1)))
    (kill-agents)
    (check (errors-p (check-kill-agent () agent2)))
    (kill-agents)
    (check (errors-p (check-kill-agent (agent1 agent2) agent3)))
    (kill-agents)))

(deftest test-kill-agents ()
  (macrolet ((check-kill-agents (agents)
	       `(progn
		  (with-pandoric (agents) 'agents
		    (setf agents nil)
		    ,@(mapcar (lambda (agent) `(get-agent (one two) :agent ,agent)) agents)
		    (kill-agents)
		    (check (equal nil (get-pandoric 'agents 'agents)))
		    ,@(mapcar (lambda (agent) `(check (not (fboundp ',agent)))) agents)))))
    (check-kill-agents (agent1 agent2 agent3))
    (check-kill-agents ())
    (check-kill-agents (agent4))))

(deftest test-trim-data ()
  (macrolet ((check-trim-data (size N trimmedSize)
	       `(let ((data (make-hash-table))
		      (keys))
		  (dotimes (i ,(length size))
		    (push (gensym) keys))
		  (mapcar (lambda (key lngth) (setf (gethash key data) (make-list lngth :initial-element 1))) 
			  keys (list ,@size))
		  (trim-data data ,N)
		  (check
		   (equal (print (mapcar (lambda (key) (length (gethash key data))) keys))
			  (print (list ,@trimmedSize))))
		  )))
    (check-trim-data (1 2 5 6 11 3 2) 10 (1 2 5 6 10 3 2))
    (check-trim-data (1) 10 (1))
    (check-trim-data (9 99 11111 1 12) 11 (9 11 11 1 11))
    (check-trim-data (9 10 11 10 9 8 7 12) 10 (9 10 10 10 9 8 7 10))))

(deftest test-linear-regression ()
  (macrolet ((within-tolerance (val1 val2 tol)
	       `(> ,tol (abs (- ,val1 ,val2))))
	     (check-linear-regression (xs ys slope intercept)
	       `(progn
		  (multiple-value-bind (m b) (linear-regression (list ,@xs) (list ,@ys))
		    (check (within-tolerance m ,slope .0001))
		    (check (within-tolerance b ,intercept .0001))))))
    (check-linear-regression (1 2 3) (1 2 3) 1 0)
    (check-linear-regression (1 2 3) (4 4 4) 0 4)
    (check-linear-regression (3 2 1) (1 2 3) -1 4)
    (check (errors-p (check-linear-regression (1 1 1) (1 2 3) 0 0)))))

(deftest test-parse-float ()
  (macrolet ((check-parse-float (str float)
	       `(multiple-value-bind (val ind) (parse-float ,str)
		  (check (equal val ,float))
		  (check (equal (length ,str) ind)))))
    (check-parse-float "121.1" 121.1)
    (check-parse-float "1" 1.0)
    (check-parse-float ".23" .23)
    (check-parse-float "192" 192.0)
    (check (errors-p (check-parse-float "192." 192.0)))
    (check (errors-p (check-parse-float "192.a" 192.0)))
    (check (errors-p (check-parse-float "19a.2" 192.0)))))
	       
    
		 
	      
(defun test-server ()
  (let ((result
	 (runtests 
	  (test-define-job)
	  (test-add-job)
	  (test-remove-job)
	  (test-job-present-p)
	  (test-activate-deactivate-job)
	  (test-job-active-p)
	  (test-run-job)
	  (test-define-agent)
	  (test-print-agent)
	  (test-print-agents)
	  (test-update-agent)
	  (test-update-agents)
	  (test-kill-agent)
	  (test-kill-agents)
	  (test-trim-data)
	  (test-linear-regression)
	  (test-parse-float)
	  )))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))