(in-package :lisp-on-lines)

(defmacro defdescription (name super-descriptions attributes &rest arguments)
  "Create a description and any lines specified."
  ;; Remove any existing lines
  `(progn 
     (dolist (method (remove-if
		      (lambda (method)
			(when (eql (contextl::get-layered-function-definer-name 'line-in)
				   (closer-mop:generic-function-name
				    (closer-mop:method-generic-function method)))))
		      (closer-mop:specializer-direct-methods (find-class ',name))))
       (remove-method (symbol-function (contextl::get-layered-function-definer-name 'line-in))
		      method))
     ;; Create any attributes
     (let ((occurence (find-occurence ',name)))
       (initialize-occurence-for-instance occurence (make-instance ',name))
       ,@(mapcar #'(lambda (x)
		     `(ensure-attribute occurence :name ',(car x) ,@(cdr x)))
		 attributes)
       ;; Add any layered lines specified.
       ,@(when t #+ (or) (ignore-errors (find-class name))
	       (loop for arg in arguments
		  when (eql (car arg) :in-layer)
		  collect `(defline line-in ((self ,name) :in-layer  ,(second arg))
			     (list ,@(cddr arg))))))))