(in-package :lisp-on-lines)

(define-layered-class direct-attribute-definition-class 
 (special-layered-direct-slot-definition contextl::singleton-direct-slot-definition)
  ((attribute-properties :accessor direct-attribute-properties
		     :documentation "This is an plist to hold the values of the attribute's properties as described by this direct attrbiute definition.")))

(defmethod initialize-instance :after ((attribute direct-attribute-definition-class) &rest initargs)
  (setf (direct-attribute-properties attribute) initargs))

(define-layered-class effective-attribute-definition-class (special-layered-effective-slot-definition) 
  ((direct-attributes :accessor attribute-direct-attributes)
   (attribute-object :accessor attribute-object
		     :documentation "")
   (attribute-object-initargs :accessor attribute-object-initargs)))


(define-layered-function attribute-value (object attribute))

(define-layered-method attribute-value (object attribute)
		       
 (let ((fn (handler-case (attribute-function attribute)
	     (unbound-slot () nil))))
   (if fn 
       (funcall fn object)
       (%attribute-value attribute))))

(defmethod attribute-description (attribute)
  ;(break "description for ~A is (slot-value attribute 'description-name)")
  (find-layer (slot-value attribute 'description-class))
#+nil  (let ((name (slot-value attribute 'description-name)))
    (when name 
      (find-description name))))


(define-layered-class standard-attribute ()
		      
  ((effective-attribute-definition :initarg effective-attribute
				   :accessor attribute-effective-attribute-definition)
   (description-name)
   (description-class :initarg description-class)
   (initfunctions :initform nil)
   (attribute-class :accessor attribute-class 
		    :initarg :attribute-class 
		    :initform 'standard-attribute)
   (name :layered-accessor attribute-name 
	  :initarg :name)
   (label :layered-accessor attribute-label 
	  :initarg :label
	  :initform nil
	  :layered t
	  ;:special t
	  )
   (function 
    :initarg :function 
    :layered-accessor attribute-function
    :layered t)
   (value :layered-accessor %attribute-value 
	  :initarg :value
	  :layered t)))



(defmethod print-object ((object standard-attribute) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "ATTRIBUTE ~A" (or (ignore-errors (attribute-name object)) "+unnamed-attribute+"))))

(defvar *bypass-property-layered-function* nil)

(define-layered-function property-layered-function (description attribute-name property-name)
  (:method  (description attribute-name property-name)
    ;(dprint "First Time PLFunction for ~A ~A ~A" description attribute-name property-name)
    (ensure-layered-function 
     (defining-description (intern (format nil "~A-~A-~A" 
		    (description-print-name description)
		     attribute-name
		     property-name)))

     :lambda-list '(description))))

(define-layered-method (setf slot-value-using-layer)
  :in-layer (context t)
  (new-value class (attribute standard-attribute) property writer)

  (when (or *bypass-property-layered-function*
	    (not (slot-definition-layeredp property)))
    (return-from slot-value-using-layer (call-next-method)))

  
  ;;FIXME: this is wrong for so many reasons.
  (let ((layer
	 (find-layer (first (remove nil (closer-mop::class-precedence-list (class-of context))
		     :key #'class-name)))))

    
    (flet ((do-set-slot()

	     (let ((fn 
	      (let ((*bypass-property-layered-function* t))
		(if (slot-boundp-using-class class attribute property)
		    (slot-value-using-class class attribute property)
		    (setf (slot-value-using-class class attribute property)
			  (property-layered-function 
			   (attribute-description attribute)
			   (attribute-name attribute)
			   (closer-mop:slot-definition-name property)))))))
	 ;(dprint "We are setting the fn ~A " fn)
	 (when (not (generic-function-methods fn))
	  ; (dprint "... there are no methods on it ever")
	   ;; * This slot has never been set before.
	   ;; create a method on property-layered-function
	   ;; so subclasses can see this new property.
	   (ensure-layered-method 
	    (layered-function-definer 'property-layered-function)
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
	     
	   
	 ;; finally, specialize this property to this description.
	 (ensure-layered-method 
	  fn
	  `(lambda (description)
	     ,new-value)
	  :in-layer layer 
	  :specializers (list (class-of (attribute-description attribute)
				       ))))))
      
      (if (slot-boundp attribute 'description-class)
	  (do-set-slot)
	  (push (lambda () (do-set-slot)) 
		(slot-value attribute 'initfunctions))))))


(define-layered-method slot-value-using-layer 
  :in-layer (layer t)
  :around (class (attribute standard-attribute) property reader)
  ;(dprint "Getting the slot value of ~A" property)
  
  (when (not (slot-boundp-using-class class attribute property))
    ;; If the slot is unbound, we search for its layered-function
    
    (let ((fn (property-layered-function 
	       (attribute-description attribute)

			(attribute-name attribute)
			(closer-mop:slot-definition-name property))))
      (dprint ".. not bound yet, have function ~A" fn)
      (if (generic-function-methods fn)
	  (let ((*bypass-property-layered-function* t))
	   ; (dprint " This shit has been bound!. We gona set the _real_ slot to the generic function like.")
	    (setf (slot-value-using-class class attribute property) fn))
	  (progn 
	    ;(dprint "This shit aint never been bound nowhere! checking for initfunction...")
	    (when (slot-definition-initfunction property)
	      ;(dprint "At least we have an initfunction. sweeet")
	      (let ((*bypass-property-layered-function* nil))
		(setf (slot-value attribute (slot-definition-name property)) 
		    (funcall (slot-definition-initfunction property)))))))))

  ;(dprint "If we're here, the slot should be bound")
  
    
   (if (and 
       (contextl::slot-definition-layeredp property)
       (not *bypass-property-layered-function*))
      (let ((fn (call-next-method)))
	;(dprint "... using fn ~A to get value" fn)
      (funcall fn layer  (attribute-description attribute)))
      (call-next-method)))




(defun slot-boundp-using-property-layered-function (class attribute property)
  (when (not 
	 (let ((*bypass-property-layered-function* t))
	   (slot-boundp-using-class class attribute property)))
    ;; If the slot is unbound, we search for its layered-function

    (let ((fn (property-layered-function 
	       (attribute-description attribute)

			(attribute-name attribute)
			(closer-mop:slot-definition-name property))))
      (if (generic-function-methods fn)
	  (let ((*bypass-property-layered-function* t))
	    (setf (slot-value-using-class class attribute property) fn))
	  NIL))))
    
#+nil(define-layered-method slot-boundp-using-layer  
  :in-layer (layer t)
  :around (class (attribute standard-attribute) property reader)
  (if *bypass-property-layered-function*
      (call-next-method)
      (slot-boundp-using-property-layered-function class attribute property)))
        
(defun attribute-value* (attribute)
  (attribute-value *object* attribute))

(defmacro with-attributes (names description &body body)
  `(with-slots ,names ,description ,@body))  

(defun display-attribute (attribute)
  (display-using-description attribute *display* *object*))

(define-layered-method display-using-description 
  ((attribute standard-attribute) display object &rest args)
  (declare (ignore args))
  (when (attribute-label attribute)
    (format display "~A " (attribute-label attribute)))
  (format display "~A" (attribute-value object attribute)))







		       
	


