(in-package :lisp-on-lines)

(define-layered-class direct-attribute-definition-class 
  (special-layered-direct-slot-definition 
   contextl::singleton-direct-slot-definition)
  ((attribute-properties 
    :accessor direct-attribute-properties
    :documentation "This is an plist to hold the values of 
    the attribute's properties as described by this direct 
    attribute definition.")))

(defmethod initialize-instance 
    :after ((attribute direct-attribute-definition-class) 
	    &rest initargs)
  (setf (direct-attribute-properties attribute) initargs))

(define-layered-class effective-attribute-definition-class 
    (special-layered-effective-slot-definition) 
  ((direct-attributes 
    :accessor attribute-direct-attributes)
   (attribute-object 
    :accessor slot-definition-attribute-object)
   (attribute-object-initargs 
    :accessor attribute-object-initargs)))

(defvar *function-access* nil
  "set/get a place's property function instead of its symbol value
   when this is set to a non-nil value")

(defmacro with-function-access (&body body)
  "executes body in an environment with *function-access* set to t"
  `(let ((*function-access* t))
     ,@body))

(defmacro without-function-access (&body body)
  "executes body in an environment with *function-access* set to nil"
  `(let ((*function-access* nil))
     ,@body))

(define-layered-function property-access-function (description attribute-name property-name)
  (:method  (description attribute-name property-name)
    (ensure-layered-function 
     (defining-description 
	 (intern (format nil "=PROPERTY-ACCESS-FUNCTION-FOR-~A->~A.~A=" 
			 (description-print-name description)
			 attribute-name
			 property-name)))
	 :lambda-list '(description))))


(defvar *init-time-description* nil)

(defmethod attribute-description :around (attribute)
  (handler-case (call-next-method)
    (unbound-slot () 
      (or 
       *init-time-description*
       (call-next-method)))))

(define-layered-class attribute ()
 ((description :initarg :description 
	       :accessor attribute-description)
  (name 
   :layered-accessor attribute-name 
   :initarg :name)
  (effective-attribute-definition 
    :initarg effective-attribute
    :accessor attribute-effective-attribute-definition)
  (attribute-class 
   :accessor attribute-class 
   :initarg :attribute-class 
  :initform 'standard-attribute)
  (keyword
   :layered-accessor attribute-keyword
   :initarg :keyword
   :initform nil
   :layered t)
  (object 
   :layered-accessor attribute-object
   :accessor described-object
   :special t)))


(define-layered-class standard-attribute (attribute)
 ((label 
   :layered-accessor attribute-label 
   :initarg :label
   :initform nil
   :layered t
   :special t)
  (label-formatter 
   :layered-accessor attribute-label-formatter
   :initarg :label-formatter
   :initform  nil 
   :layered t
   :special t)
  (function 
   :initarg :function 
   :layered-accessor attribute-function
   :layered t
   :special t)
  (value 
   :layered-accessor attribute-value 
   :initarg :value
   :layered t
   :special t)
  (value-formatter 
   :layered-accessor attribute-value-formatter
   :initarg :value-formatter
   :initform nil
   :layered t
   :special t)
  (activep 
   :layered-accessor attribute-active-p
   :initarg :activep ;depreciated
   :initarg :active
   :initform t
   :layered t
   :special t
   :documentation
   "Can be T, NIL or :WHEN. In the latter case, attribute is only active if the attribute value is non-null.")
  (active-attributes :layered-accessor attribute-active-attributes
		       :initarg :attributes
		       :layered t
		       :special t)
  (active-descriptions :layered-accessor attribute-active-descriptions
		       :initarg :activate
		       :initform nil
		       :layered t
		       :special t)
  (inactive-descriptions :layered-accessor attribute-inactive-descriptions
		       :initarg :deactivate
		       :initform nil
		       :layered t
		       :special t)))

(define-layered-method attribute-active-p :around (attribute)		       
 (let ((active? (call-next-method)))
   (if (eq :when active?)
       (not (null (attribute-value attribute)))
       active?)))
		       
(define-layered-method attribute-label-formatter :around (attribute)
   (or (slot-value attribute 'label-formatter) 
       (attribute-value (find-attribute (attribute-description attribute) 'label-formatter))
       (error "No Formatter .. fool!")))

(define-layered-method attribute-value-formatter :around (attribute)
		       
   (or (slot-value attribute 'value-formatter) 
       (attribute-value (find-attribute (attribute-description attribute) 'value-formatter))
       (error "No Formatter .. fool!")))
		       


(define-layered-method attribute-object ((attribute standard-attribute))
 (if (slot-boundp attribute 'object)
     (call-next-method)
     (described-object (attribute-description attribute))))


(define-layered-function attribute-value-using-object (object attribute))
(define-layered-function (setf attribute-value-using-object) (value object attribute))

(define-layered-method attribute-value ((attribute standard-attribute))
 (attribute-value-using-object (attribute-object attribute) attribute))

(define-layered-method attribute-value-using-object (object attribute)
 (let ((fn (handler-case (attribute-function attribute)
	     (unbound-slot () nil))))
   (if fn 
       (funcall fn object)
       (slot-value attribute 'value))))

(define-layered-method (setf attribute-value) (value (attribute standard-attribute))
 (setf (attribute-value-using-object (attribute-object attribute) attribute) value))

(define-layered-method (setf attribute-value-using-object) (value object attribute)
 (error "No (SETF ATTRIBUTE-VALUE-USING-OBJECT) for ~A ~A and we are not editable"
	object attribute))


(defun ensure-access-function (class attribute property)
  (with-function-access 
    (if (slot-definition-specialp property)
	(let ((slot-symbol 
	       (with-symbol-access
		 (slot-value-using-class 
		  class attribute property))))
	  (if (fboundp slot-symbol)
	      (symbol-function slot-symbol)
	      (setf (symbol-function slot-symbol)
		    (property-access-function
		     (attribute-description attribute)
		     (attribute-name attribute)
		     (slot-definition-name property)))))
	(if (slot-boundp-using-class class attribute property)
	    (slot-value-using-class class attribute property)
	    (setf (slot-value-using-class class attribute property)
		  (property-access-function
		   (attribute-description attribute)
		   (attribute-name attribute)
		   (slot-definition-name property)))))))

(define-layered-method slot-boundp-using-layer  
  :in-layer (layer t)
  :around (class (attribute standard-attribute) property reader)

; (dprint "Checking boundp ~A ~A" (attribute-name attribute)
	; (slot-definition-name property))

  (if (or *symbol-access* *function-access*)
      (call-next-method)
      (or (when (slot-definition-specialp property)
	    (with-function-access
	   (slot-boundp-using-class class attribute property)))
	  (if (generic-function-methods 
	       (ensure-access-function class attribute property))
	      T
	      NIL))))

(define-layered-method (setf slot-value-using-layer)
  :in-layer (context t)
  :around
  (new-value class (attribute standard-attribute) property writer)
  
;;  (dprint "Setting ~A ~A to : ~A" attribute property new-value)

  (if (or *symbol-access* *function-access*)
      (call-next-method)
	     
      (if (and (slot-definition-specialp property)
	       (with-function-access
		 (without-symbol-access (slot-boundp-using-class class attribute property))))
	  (with-function-access
	    (call-next-method))
	  (let ((layer
		 ;;FIXME: this is wrong for so many reasons
		 (find-layer (first (remove nil (closer-mop::class-precedence-list (class-of context))
					    :key #'class-name))))
		(boundp (slot-boundp-using-class class attribute property))
		(fn  (ensure-access-function class attribute property)))

	    (when (not boundp)
	      ;; * This slot has never been set before.
	      ;; create a method on property-accessor-function
	      ;; so subclasses can see this new property.
	      (ensure-layered-method 
	       (layered-function-definer 'property-access-function)
	       `(lambda (description attribute property)
		  (declare (ignore description attribute property))
		  ,fn)
	       :in-layer layer
	       :specializers  
	       (list (class-of  
		      (attribute-description attribute))
		     (closer-mop:intern-eql-specializer 
		      (attribute-name attribute))
		     (closer-mop:intern-eql-specializer 
		      (closer-mop:slot-definition-name property)))))

	    ;; specialize this property to this description.
	    ;;(dprint "actrually specializering")
	    (ensure-layered-method 
	     fn
	     `(lambda (description)
		(funcall ,(lambda()
				 new-value)))
	     :in-layer layer 
	     :specializers (list (class-of (attribute-description attribute))))

	    ;;	and return the set value as is custom
	    new-value))))
		      
(define-layered-method slot-value-using-layer 
  :in-layer (layer t)
  :around (class (attribute standard-attribute) property reader)
  
;  ;(dprint "Getting the slot value of ~A" property)   
  (if (or *symbol-access* *function-access*)
      (call-next-method)
      (let ((fn (ensure-access-function class attribute property)))

	(unless (slot-boundp-using-class class attribute property)
	  (slot-unbound class attribute (slot-definition-name property)))

	(if (slot-definition-specialp property)
	    (if (with-function-access
		  (slot-boundp-using-class class attribute property))
		(with-function-access 
		  (slot-value-using-class class attribute property))
		(funcall fn layer (attribute-description attribute)))
	    (handler-case (funcall fn layer (attribute-description attribute))
	      (error ()
		(warn "Error calling ~A" fn)))))))
		    
	      




(defmethod print-object ((object standard-attribute) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "ATTRIBUTE ~A" (or (ignore-errors (attribute-name object)) "+unnamed-attribute+"))))

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


(defun attribute-value* (attribute)
  (attribute-value *object* attribute))

(defmacro with-attributes (names description &body body)
  `(let ,(loop for name in names collect 
	      (list name `(find-attribute ,description ',name)))
     ,@body))q









		       
	


