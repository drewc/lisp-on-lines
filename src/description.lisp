(in-package :lisp-on-lines)

(defvar *object* nil)
(defvar *description*)

(define-layered-function description-of (thing)
  (:method (thing)
    (find-description 't)))

(defun description-print-name (description)
  (description-class-name (class-of description)))

(defun description-attributes (description)
  (description-class-attributes (class-of description)))

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

(defun find-attribute (description attribute-name &optional (errorp t))
  (or (find attribute-name (description-attributes description)
	    :key #'attribute-name)
      (when errorp (error "No attribute named ~A found in ~A describing ~A" attribute-name description (described-object description)))))

(define-layered-function description-active-descriptions (description)
  (:method ((description standard-description-object))
    (attribute-value (find-attribute description 'active-descriptions)))
  (:method ((description attribute))
    (attribute-active-descriptions description)))

(define-layered-function description-inactive-descriptions (description)
  (:method ((description standard-description-object))
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
		    (mapcar #'find-layer 
			    (slot-definition-layers 
			     (attribute-effective-attribute-definition attribute))))))
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
  (let ((*description* description)
	(*object*  object))
    (dletf (((described-object *description*) object))
	(funcall-with-layer-context
	 (modify-layer-context 
	  (if (standard-description-p *description*)
	      (adjoin-layer *description* (current-layer-context))
	      (current-layer-context))
	  :activate (description-active-descriptions *description*)
	  :deactivate (description-inactive-descriptions *description*))
	 (lambda () (contextl::funcall-with-special-initargs  
		     (loop 
			:for (key val) :on args :by #'cddr
			:collect (list (find key (description-attributes *description*) 
					     :key #'attribute-keyword)
				       :value val))
		     (lambda ()
		       (contextl::funcall-with-special-initargs  
			(let ((attribute (ignore-errors (find-attribute *description* 'active-attributes))))	
			  (when attribute
			    (loop for spec in (attribute-value attribute)
			       if (listp spec)
			       collect (cons (or 
					      (find-attribute *description* (car spec))
					      (error "No attribute matching ~A" (car spec)))
					     (cdr spec)))))
			function))))))))



		   
(defmacro define-description (name &optional superdescriptions &body options)
  (let ((description-name (defining-description name)))     
    (destructuring-bind (&optional slots &rest options) options
      (let ((description-layers (cdr (assoc :in-description options))))
	(if description-layers
	    `(progn ;eval-when (:compile-toplevel :load-toplevel :execute)
	       ,@(loop 
		    :for layer 
		    :in description-layers
		    :collect `(define-description 
				  ,name ,superdescriptions ,slots
				  ,@(acons 
				    :in-layer (defining-description layer)
				    (remove :in-description options :key #'car)))))
	    `(progn ;eval-when (:compile-toplevel :load-toplevel :execute)
					;  `(progn
	       (defclass ,description-name 
		   ,(append (mapcar #'defining-description 
				    superdescriptions) 
			    (unless (or (eq t name)    
					(assoc :mixinp options))
			      (list (defining-description t))))
		 ,(if slots slots '())
		 ,@options
		 ,@(unless (assoc :metaclass options)
			   '((:metaclass standard-description-class))))
	       (initialize-descriptions)
	       (find-description ',name)))))))







			      



		      
  




  
  
  
