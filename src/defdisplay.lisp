(in-package :lisp-on-lines)

(define-layered-function display-using-description (description object component)
  (:method-combination wrapping-standard)
  (:documentation
   "Render the object in component, 
    using DESCRIPTION, which is an occurence, an attribute, or something else entirely."))

(defun make-display-function (component object
			      &rest properties
			      &key  (line #'line-in)
			      &allow-other-keys)
  "returns a function that expects a 3 argument function as its argument

The function argument (which is usually display-using-description) will be called with the proper environment for display all set up nice n pretty like."

  (lambda (function)
    (let* ((description (find-occurence object)))
      (if description
	  (dletf (((attributes description) 
		   (or
		    (attributes description)
		    (list-attributes description))))
	    ;; apply the default line to the description
	    (funcall-with-description
	     description
	     (funcall line object)
	     ;; apply the passed in arguments and call display-using-description
	     #'(lambda ()		 
		 (funcall-with-description
		  description
		  properties
		  function description object component))))
	  (error "no description for ~A" object)))))

(define-layered-function display (component object &rest args)
  (:documentation
   "Displays OBJECT in COMPONENT."))

(define-layered-method display ((component t) (object t)
				&rest properties)
  " The default display dispatch method

  DISPLAY takes two required arguments, 
  COMPONENT : The component to display FROM (not neccesarily 'in')
  OBJECT : The 'thing' we want to display... in this case it's the component

  DISPLAY also takes keywords arguments which modify the DESCRIPTION,
  that is to say the parameters that come together to create the output.

The default display calls out via FUNCALL-WITH-LAYERS to tche DISPLAY-USING-DESCRIPTION method."
  (funcall (apply 'make-display-function component object properties)
	   'display-using-description))

;;;;; Macros

(defun funcall-with-layers (layers thunk)
  (let ((context (current-layer-context)))
    (loop :for (op layer) 
       :on layers :by #'cddr
       :do (setf context 
		 (case op
		   (+ (adjoin-layer layer context))
		   (- (remove-layer layer context)))))
    (funcall-with-layer-context context thunk)))
		  

(defun funcall-with-description (description properties function &rest args)  
  (if description
      (dletf* (((description-type description) 
		(or
		 (getf properties :type)
		 (description-type description)))
	    
	       ((description-layers description) 
		(append 
		 (description-layers description)
		 (getf properties :layers)))
	       ((description-properties description) (append (description-properties description) properties)))
	(funcall-with-layers 
	 (description-layers description)
	 (lambda ()
	   (contextl::funcall-with-special-initargs
	    (list (cons description properties))
	    #'(lambda ()
		(apply function args))))))
      (apply function args)))

(defmacro with-description ((description &rest properties) &body body)
  `(funcall-with-description ,description (if ',(cdr properties)
					       (list ,@properties)
					       ,(car properties))
    #'(lambda ()
	,@body)))

(define-layered-function find-do-attributes (desc))

(define-layered-method find-do-attributes ((description description))

  (loop
     :for att
     :in (attributes description)
     :collect (let ((default (find (car (ensure-list att))
				   (default-attributes description)
				   :key #'car)))
		(or default att))))

(defmacro do-attributes ((var description &optional (attributes `(find-do-attributes ,description))) &body body)
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
				   #'(lambda ()
				       ,@body))))))

(defmacro with-component ((component) &body body)
  `(let ((self ,component))
    (declare (ignorable self))
    (flet ((display* (thing &rest args)
	     (apply #'display ,component thing args))
	   (display-attribute (attribute obj &rest
					 props)
	     (if props
		 (funcall-with-description
		  attribute props
		  #'display-using-description attribute obj ,component)
		 (display-using-description attribute obj ,component))))
      (declare (ignorable #'display* #'display-attribute))
      ,@body)))




