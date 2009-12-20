(in-package :lol)

(defclass define-description-class (display-description-class)
  ())

(define-layered-class define-description-attribute (display-attribute) ())

(define-layered-method attribute-function ((attribute define-description-attribute))
		       (call-next-method)
)

(defgeneric eval-property-initarg (att initarg)
  (:method ((attribute standard-attribute) initarg)
    nil)
  (:method ((attribute standard-attribute) (initarg (eql :function)))
    t)
  (:method ((attribute standard-attribute) (initarg (eql :value)))
    t))

(defun prepare-initargs (att args)
  (loop 
     :for (key arg) 
     :on args :by #'cddr 
     :nconc (list key 
		  (if (eval-property-initarg att key)
		      (eval arg)
		      arg))))

(defmethod initialize-attribute-for-description :around (description (attribute define-description-attribute) layer &rest args)
  (apply #'call-next-method description attribute layer (prepare-initargs attribute args)))

(defmethod description-class-attribute-class ((class display-description-class))
  'define-description-attribute)

(defmacro define-description (name &optional superdescriptions &body options)
  (destructuring-bind (&optional slots &rest options) options
    `(let ((%dn ',name))
       (declare (special %dn)) 
       (defdescription ,name ,superdescriptions
	 ,(if slots slots '())
	 ,@options
	 ,@(unless (assoc :metaclass options)
		   '((:metaclass define-description-class)))))))

(defmethod initialize-instance :around ((class display-description-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs)
	   (special %dn))
  (prog1
      (if (or (and (boundp '%dn) (eql %dn t)) 
	      (loop for direct-superclass in direct-superclasses
		 thereis (ignore-errors (subtypep direct-superclass (class-of (find-description t))))))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (class-of (find-description 't))))
	       initargs))))


(defmethod reinitialize-instance :around ((class display-description-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs)
	   (special %dn))
;  (warn "CLASS ~A ARGS ~A:" class initargs)
  (prog1
      (if (or (not direct-superclasses-p)
	      (and (boundp '%dn) (eql %dn t))
	      (loop for direct-superclass in direct-superclasses
		 thereis (ignore-errors (subtypep direct-superclass (class-of (find-description t))))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (class-of (find-description 't))))
		 initargs))))
