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

(define-agent :name the-managers)

(defmacro get-agent (names strings)
  `(progn
     (define-agent :name the-managers)
     ,@(mapcar (lambda (name string)
		 `(add-job :agent the-managers :job (define-job :name ,name :Fn (lambda () (format t "~a~%" ,string)))))
		 names strings)))

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
		      (print-agent 'the-managers))
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

(deftest test-add-job ()
  (macrolet ((check-added (names strings)
	       `(progn
		  (get-agent ,names ,strings)
		  ,@(mapcar (lambda (name) `(check (job-present-p ,name the-managers))) names)))
	     (check-for-error (names strings should-error)
	       `(check
		 (equal ,should-error
			(handler-case
			    (progn 
			      (get-agent ,names ,strings)
			      nil)
			  (error (condition) (declare (ignore condition)) t))))))
    (check-added (test) ("output"))
    (check-added () ())
    (check-added (name2 name) ("output2" "output"))
    (check-for-error (name name) ("output" "output2") t)
    (check-for-error (name name2) ("output" "output") nil)
    (check-for-error (name name2) ("output" "output2") nil)))

(deftest test-remove-job ()
  (macrolet ((check-removed (remove names)
	       `(progn
		  (get-agent ,names ,(mapcar #'symbol-name names))
		  (remove-job ,remove the-managers)
		  (check (not (job-present-p ,remove the-managers)))))
	     (check-for-error (remove names should-error)
	       `(progn
		  (get-agent ,names ,names)
		  (check
		   (equal ,should-error
			  (handler-case
			      (progn
				(remove-job ,remove the-managers)
				nil)
			    (error (condition) (declare (ignore condition)) t)))))))
    (check-removed name (name name2))
    (check-removed name2 (name2))
    (check-for-error name () t)
    (check-for-error name2 (name) t)
    (check-for-error name3 (name1 name3) nil)))





	     

(defun test-server ()
  (let ((result
	 (runtests 
	  (test-define-job)
	  (test-define-agent)
	  (test-print-agent)
	  (test-add-job)
	  (test-remove-job)
	  )))
    (format t "~%overall: ~:[FAIL~;pass~]~%" result)))