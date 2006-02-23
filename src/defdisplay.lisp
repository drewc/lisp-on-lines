(in-package :lisp-on-lines)

(define-layered-function display-using-description (description object component)
  (:documentation
   "Render the object in component, 
    using DESCRIPTION, which is an occurence, an attribute, or something else entirely."))

(define-layered-method
    display-using-description (d o c)
    (<:as-html "default :" o))

(defmethod find-layer-for-type (type)
  type)


(define-layered-function display (component object &rest args)
  (:documentation
   "Displays OBJECT in COMPONENT."))

(define-layered-method display ((component t) (object t)
				&rest properties
				&key type
				&allow-other-keys)
    "The default display calls out via FUNCALL-WITH-LAYERS to tche DISPLAY-USING-DESCRIPTION method."

    (let* ((occurence (find-occurence object))
	   (description (or (find-display-attribute
			     occurence
			     (setf type (or type (description.type occurence))))
			   occurence)))
      (if description
	  (dletf (((description.type occurence) type)
		  ((description.layers description) (append `(+

							      ;;find-layer-for-type is a
							      ;; backwards compat thing
							   ,(find-layer-for-type
							     type))
							 (description.layers description)))
		  ((attributes description) (or
					     (attributes description)
					     (list-slots object))))
	    (funcall-with-description
	     description properties
	     #'display-using-description description object component))
	  (error "no description for ~A" object))))

;;;;; Macros
;;;; TODO: " should really be a funcall-with function with a small wrapper."

(defun funcall-with-description (description properties function &rest args)
  (if description
      (dletf* (((description.type description) (or
						(getf properties :type)
						(description.type description)))
	    
	       ((description.layers description) (append 
							 (description.layers description)
							 (getf properties :layers)))
	       ((description.properties description) properties))
	(funcall-with-layers 
	 (description.layers description)
	 #'(lambda ()
	     (funcall-with-special-initargs
	      description properties
	      #'(lambda ()
		  (apply function args))))))
      (apply function args)))



(defmacro with-description ((description &rest properties) &body body)
  `(funcall-with-description ,description (if ',(cdr properties)
					       (list ,@properties)
					       ,(car properties))
    #'(lambda ()
	,@body)))

(defmacro do-attributes ((var description &optional (attributes `(attributes ,description))) &body body)
  (with-unique-names (att properties type)
    `(dolist* (,att  ,attributes)
      (let* ((,att (ensure-list ,att))
                (,properties (rest ,att))
                (,type (getf ,properties :type))
                (,var (let ((a (find-attribute ,description (first ,att))))
			(if ,type
			    (apply #'make-attribute :name (first ,att) :type ,type ,properties)
			    (if a a (make-attribute :name (first ,att) :slot-name (first ,att)))))))
	(funcall-with-description ,var ,properties
	  #'(lambda () ,@body))))))

(defmacro with-component ((component) &body body)
  `(let ((self ,component))
    (declare (ignorable self))
    (flet ((display* (thing &rest args)
	     (apply #'display ,component thing args))
	   (display-attribute (attribute obj &optional props)
	     (if props
		 (funcall-with-description
		  attribute props
		  #'display-using-description attribute obj ,component)
		 (display-using-description attribute obj ,component))))
      (declare (ignorable #'display* #'display-attribute))
      ,@body)))

(defmacro defdisplay (&body body)
  (loop with in-layerp = (eq (car body) :in-layer)
	with layer = (if in-layerp (cadr body) 't)
	for tail on (if in-layerp (cddr body) body)
	until (listp (car tail))
	collect (car tail) into qualifiers
	finally
	(when (member :in-layer qualifiers)
	  (error "Incorrect occurrence of :in-layer in defdisplay. Must occur before qualifiers."))
	(return
	  (destructuring-bind (description object &optional component) (car tail) 
	    (with-unique-names (d c)
	      (let (standard-description-p)
		`(define-layered-method
		  display-using-description
		  :in-layer ,layer
		  ,@qualifiers
		
		  (,(cond
		     ((listp description)
		      (setf d (car description))
		      description)
		     (t
		      (setf d description)
		      (setf standard-description-p t)
		      `(,d description)))
		   ,object
		   ,(cond
		     ((null component)
		      `(,c component))
		     ((listp component)
		      (setf c (car component))
		      component)
		     (t
		      (setf c component)
		      `(,c component))))
		  (with-component (,c)  
			 ,@(cdr tail)))))))))


