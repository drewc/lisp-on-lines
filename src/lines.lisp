(in-package :lisp-on-lines)

(defmacro defline (name (specializer &rest layers-and-combination-keywords) &body docstring-and-body)
  `(progn
    ,(eval-when
      (:compile-toplevel :load-toplevel :execute)
      (unless (fboundp (contextl::get-layered-function-definer-name name)) 
	`(define-layered-function ,name (arg)
	  (:method-combination append))))
    (define-layered-method
	,name
      ,@layers-and-combination-keywords
      ,@(unless
	 (or (third layers-and-combination-keywords)
	     (and layers-and-combination-keywords
		  (null (cdr layers-and-combination-keywords)))) 
	 '(APPEND))
      (,specializer)
	,(when (cdr docstring-and-body)
	       (car docstring-and-body))
	
	,(or (cdr docstring-and-body) (car docstring-and-body)))))

(defun line-out (component object &rest args &key (line #'line-in) &allow-other-keys )
  (apply #'display component object (append args (funcall line object))))

(defline line-in (thing)
  '())


(defmacro call-line (from line &rest args)
  (with-unique-names (lines object)
    `(multiple-value-bind (,lines ,object)
	 (funcall ,line)
       (call-display-with-context ,from ,object nil (append ,args ,lines)))))

