(in-package :lisp-on-lines)

(define-layered-function line-in (name)
  (:method-combination append)
  (:method append (thing)
    '()))

(defmacro defline (name (specializer &rest layers-and-combination-keywords) &body docstring-and-body)
  `(progn
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


