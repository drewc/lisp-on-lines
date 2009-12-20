(in-package :lisp-on-lines)

(defstruct unbound-slot-value (s))

(defvar +unbound-slot+ (make-unbound-slot-value))

(defmethod print-object ((object unbound-slot-value) stream)
  (print-unreadable-object (object stream)
    (format stream "UNBOUND")))

(define-description standard-object ()
  ((editp :value t)
   (class-slots :label "Slots" 
		:function (compose 'class-slots 'class-of))))

(define-description standard-object ()
  ((editp :value t)
   (class-slots :label "Slots" 
		:function (compose 'class-slots 'class-of)))
  (:in-description editable))

(define-layered-class slot-definition-attribute (define-description-attribute)
 ((slot-name :initarg :slot-name 
	     :accessor attribute-slot-name
	     :layered t)))


(define-layered-method attribute-active-p :around ((attribute slot-definition-attribute))		       
 (let ((active? (slot-value attribute 'activep)))
   (if (and (eq :when active?)
	    (unbound-slot-value-p (attribute-value attribute)))
       NIL
       
       (call-next-method))))

(define-layered-method attribute-active-p 
 :in-layer #.(defining-description 'editable) 
 :around ((attribute slot-definition-attribute))		       
 (let ((active? (slot-value attribute 'activep)))
   (if (and (eq :when active?)
	    (unbound-slot-value-p (attribute-value attribute)))
       t      
       (call-next-method))))

(defmethod shared-initialize :around ((object slot-definition-attribute) 
				      slots &rest args)
  (with-active-descriptions (editable) 
    (prog1 (call-next-method)
      (unless (attribute-setter object)
	(setf (attribute-setter object) 
	      (lambda (v o)
		(setf (slot-value o (attribute-slot-name object)) v)))))))
		  

(define-layered-method attribute-value-using-object (object (attribute slot-definition-attribute))
  (if (slot-boundp object (attribute-slot-name attribute))
		       
      (slot-value object (attribute-slot-name attribute))
      +unbound-slot+))

(defun attribute-slot-makunbound (attribute)
  (slot-makunbound (attribute-object attribute) (attribute-slot-name attribute)))

(defun ensure-description-for-class (class &key attributes (name (intern (format nil "DESCRIPTION-FOR-~A" (class-name class)))) 
				     direct-superclasses direct-slot-specs)

  (let* ((super-descriptions
	  (mapcar #'class-of 
			      (delete nil (mapcar (rcurry #'find-description nil) 
						  (mapcar #'class-name direct-superclasses)))))
	 (desc-class 
	  (ensure-layer (defining-description name) 
		:direct-superclasses (or super-descriptions (list (class-of (find-description 'standard-object))))
		:direct-slots 
		(loop 
		   :for slot in (class-slots class)
		   :collect 
		   (let ((direct-spec 
			  (find (slot-definition-name slot) 
				direct-slot-specs
				:key (rcurry 'getf :name))))
		     (if direct-spec 
			 (append (alexandria:remove-from-plist direct-spec 
							       :initfunction
							       :initform
							       :initargs
							       :readers
							       :writers)
				 (unless 
				     (getf direct-spec :attribute-class)
				   (list :attribute-class 'slot-definition-attribute))
				 (unless 
				     (getf direct-spec :label)
				   (list :label (format nil 
							"~@(~A~)" (substitute #\Space #\- (symbol-name (slot-definition-name slot))))))
				 (list :slot-name (slot-definition-name slot)))
			 `(:name ,(slot-definition-name slot) 
				 :attribute-class slot-definition-attribute
				 :slot-name ,(slot-definition-name slot)
				 :label ,(format nil 
						 "~@(~A~)" (substitute #\Space #\- (symbol-name (slot-definition-name slot)))))))
		   :into slots
				 :collect (slot-definition-name slot) :into names
				 :finally (return (cons `(:name active-attributes
							  :value ',(or attributes names))
							slots)))	
		:metaclass 'define-description-class)))    
    (unless (ignore-errors (find-description (class-name class)))
      (find-layer  (ensure-layer (defining-description (class-name class))
				 :direct-superclasses (list desc-class)
				 :metaclass 'define-description-class)))))


(defclass described-class ()
  ((direct-slot-specs :accessor class-direct-slot-specs)
   (attributes :initarg :attributes :initform nil)))

(defmethod ensure-class-using-class :around ((class described-class) name &rest args)
  
  (call-next-method))

(defmethod direct-slot-definition-class ((class described-class) &rest initargs)
  (let ((slot-class (call-next-method))) 
    (make-instance (class-of slot-class) :direct-superclasses (list slot-class (find-class 'described-class-direct-slot-definition)))))

(defclass described-class-direct-slot-definition ()
  ())

(defmethod shared-initialize :around ((class described-class-direct-slot-definition) slot-names &key &allow-other-keys)
  (call-next-method))
  
(defmethod validate-superclass
           ((class described-class)
            (superclass standard-class))
  t)

(defmethod initialize-instance :after ((class described-class) &rest initargs &key (direct-superclasses '()) direct-slots)
  (declare (dynamic-extent initargs))
  (finalize-inheritance class)
  (ensure-description-for-class class :direct-slot-specs direct-slots 
				      :direct-superclasses  direct-superclasses
				      :attributes (slot-value class 'attributes)))

(defmethod reinitialize-instance :after ((class described-class) &rest initargs &key (direct-superclasses '()) direct-slots)
  (declare (dynamic-extent initargs))
  (finalize-inheritance class)
  (ensure-description-for-class class :direct-slot-specs direct-slots 
				      :direct-superclasses direct-superclasses
				      :attributes (slot-value class 'attributes)))

(defclass described-standard-class (described-class standard-class ) ())

(defmethod validate-superclass
    ((class described-standard-class)
     (superclass standard-class))
  t)

(define-layered-method description-of ((object standard-object))
  (or (ignore-errors (find-description (class-name (class-of object))))
      (find-description 'standard-object)))



