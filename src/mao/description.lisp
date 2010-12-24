(in-package :lisp-on-lines)

(defdynamic described-object nil)
(defdynamic description nil)

;;backwards-compat hacks
(define-symbol-macro *object* (dynamic described-object))
(define-symbol-macro *description* (dynamic description))

;; forward compat hacks

(defun current-description ()
  (dynamic description))

(define-layered-function description-of (thing)
  (:method (thing)
    (find-description 't)))

(defun description-print-name (description)
  (description-class-name (class-of description)))

(defun description-attributes (description)
  (alexandria:hash-table-values (description-class-attributes (class-of description))))

(defun description-current-attributes (description)
  	  (remove-if-not 
	   (lambda (attribute)
	     (and 		     
	      (some #'layer-active-p 
			(mapcar #'find-layer 
				(slot-definition-layers 
				 (attribute-effective-attribute-definition attribute))))))
	   (description-attributes description)))

(defun description-active-attributes (description)
  	  (remove-if-not 
	   #'attribute-active-p
	   (description-attributes description)))


 
(define-layered-function description-active-descriptions (description)
  (:method ((description t))
    (attribute-value (find-attribute description 'active-descriptions)))
  (:method ((description attribute))
    (attribute-active-descriptions description)))

(define-layered-function description-inactive-descriptions (description)
  (:method ((description t))
    (attribute-value (find-attribute description 'inactive-descriptions)))
  (:method ((description attribute))
    (attribute-inactive-descriptions description)))

(define-layered-function attributes (description)
  (:method (description)
    (let* ((active-attributes 
	    (find-attribute description 'active-attributes))
	   (attributes (when active-attributes
			 (ignore-errors (attribute-value active-attributes)))))
      (remove-if-not 
       (lambda (attribute)
	 (and attribute
	      (attribute-active-p attribute)		     
	      (some #'layer-active-p 
		    (attribute-layers attribute))))
       (if attributes
	   (mapcar (lambda (spec)		    
		     (find-attribute 
		      description
		      (if (listp spec)
			  (car spec)
			  spec)))
		   attributes)
	   (description-attributes description))))))
	  
(defun funcall-with-described-object (function object description &rest args)
  (setf description (or description (description-of object)))
  (dynamic-let ((description description)
		(object  object))
    (dletf (((described-object description) object))
	(funcall-with-layer-context
	 (modify-layer-context (adjoin-layer description (current-layer-context))
	  :activate (description-active-descriptions description)
	  :deactivate (description-inactive-descriptions description))
	 (lambda () 
	   (with-special-symbol-access  
	     (contextl::funcall-with-special-initargs  
	      (without-special-symbol-access 
		(loop 
		   :for (key val) :on args :by #'cddr
		   :collect (list (find key (description-attributes description) 
					:key #'attribute-keyword)
				  :value val)))
	      (lambda () 		     
		(contextl::funcall-with-special-initargs  
		 (without-special-symbol-access 
		   (let ((attribute (ignore-errors (find-attribute description 'active-attributes))))	
		     (when attribute
		       (loop for spec in (attribute-value attribute)
			  if (listp spec)
			  collect (cons (or 
					 (find-attribute description (car spec))
					 (error "No attribute matching ~A" (car spec)))
					(cdr spec))))))
		 (lambda ()
		   (without-special-symbol-access 
		     (funcall  function))))))))))))

(defmacro with-described-object ((object &optional description)
				 &body body)
  (once-only (object)
    `(funcall-with-described-object (lambda (),@body) ,object ,(or description
							      `(description-of ,object)))))








			      



		      
  




  
  
  
