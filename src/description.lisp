(in-package :lisp-on-lines)

(define-description description ())

(defgeneric find-description-class (name &optional errorp)	   
  (:method ((name (eql t)) &optional errorp)
    (declare (ignore errorp))
    (find-class 'description t))
  (:method ((name symbol) &optional errorp)
    (or (find-class (defining-description name) errorp)
	(find-description-class t)))
  (:method ((description description) &optional errorp)
    (declare (ignore errorp))
    (class-of description)))

(defun find-description (name)
   (slot-value (find-description-class name) 'instance))

(defun description-attributes (description)
  (closer-mop:class-slots (find-description-class description)))

(define-layered-function attributes (description))

(define-layered-method attributes (description)
 (description-attributes description))
		       
;;;!-- TODO: This is a prime candidate for optimization
(defun find-attribute (description attribute-name)
  (find attribute-name (description-attributes description) :key #'attribute-name))

(define-display ((description description))
  (format *display* "~{~A~%~}" 
	  (mapcar 
	   (lambda (attribute)
	     (with-output-to-string (*display*)
	       (display-attribute attribute)))
	   (attributes description))))

(define-layered-method description-of (object)
  (find-description 't))			      

(define-layered-method description-of ((symbol symbol))
  (find-description 'symbol))

(define-description symbol ()
  ((identity :label "Symbol:")
   (name 
    :function #'symbol-name
    :label "Name:")
   (value 
    :label "Value:" 
    :function 
    (lambda (symbol)
      (if (boundp symbol)
	  (symbol-value symbol)
	  "<UNBOUND>")))
   (package :function #'symbol-package
	    :label "Package:")
   (function :label "Function:"
    :function     	     
    (lambda (symbol)
     (if (fboundp symbol)
	 (symbol-function symbol)
	 "<UNBOUND>")))))


		      
  




  
  
  
