(in-package :yaclml)

(defun funcall-with-tag (tag-spec thunk) 
  (let ((%yaclml-code% nil) 
	(%yaclml-indentation-depth% 0))
    (declare (special %yaclml-code%))
    ;; build tag's body
    (dolist (i (fold-strings
		(nreverse
		 (funcall (gethash (car (ensure-list tag-spec)) *expanders*)
			  (append (cdr tag-spec) (list
						  thunk))))))
      (if (functionp i)
	  (funcall i)
	  (write-string i *yaclml-stream*)))))