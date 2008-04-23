(in-package :lisp-on-lines)

(define-layered-method description-of ((symbol symbol))
  (find-description 'symbol))

(define-description symbol ()
  ((identity :label nil)
   (name 
    :function #'symbol-name
    :label "Name")
   (value 
    :label "Value" 
    :function 
    (lambda (symbol)
      (if (boundp symbol)
	  (symbol-value symbol)
	  "<UNBOUND>")))
   (package :function #'symbol-package
	    :label "Package")
   (function :label "Function"
    :function     	     
    (lambda (symbol)
     (if (fboundp symbol)
	 (symbol-function symbol)
	 "<UNBOUND>")))))