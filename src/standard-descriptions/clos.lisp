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

(define-layered-class slot-definition-attribute (standard-attribute)
 ((slot-name :initarg :slot-name :accessor attribute-slot-name)))

(defmethod shared-initialize :around ((object slot-definition-attribute) 
				      slots &rest args)
  (prog1 (call-next-method)
    (unless (attribute-setter object)
      (setf (attribute-setter object) 
	    (lambda (v o)
	      (setf (slot-value o (attribute-slot-name object)) v))))))
		  

(define-layered-method attribute-value (object (attribute slot-definition-attribute))
  (if (slot-boundp object (attribute-slot-name attribute))
		       
      (slot-value object (attribute-slot-name attribute))
      (gensym "UNBOUND-SLOT-")))

(defun ensure-description-for-class (class &optional (name (intern (format nil "DESCRIPTION-FOR-~A" (class-name class)))))
  (let ((desc-class 
	 (ensure-class (defining-description name) 
		:direct-superclasses (list (class-of (find-description 'standard-object)))
		:direct-slots (loop :for slot in (class-slots class)
				 :collect `(:name ,(slot-definition-name slot) 
					    :attribute-class slot-definition-attribute
					    :slot-name ,(slot-definition-name slot)
					    :label ,(slot-definition-name slot))
				 :into slots
				 :collect (slot-definition-name slot) :into names
				 :finally (return (cons `(:name active-attributes
							  :value ,names)
							slots)))	
		:metaclass 'standard-description-class)))
    
    (unless (ignore-errors (find-description (class-name class)))
      (ensure-class (defining-description (class-name class))
		    :direct-superclasses (list desc-class)
			:metaclass 'standard-description-class))
  (find-description name)))

(defclass described-class ()
  ())

(defmethod validate-superclass
           ((class described-class)
            (superclass standard-class))
  t)

(defmethod initialize-instance :after ((class described-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (finalize-inheritance class)
  (ensure-description-for-class class))


(defmethod reinitialize-instance :after ((class described-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (finalize-inheritance class)
  (ensure-description-for-class class))


  
  
(define-layered-method description-of ((object standard-object))
  (or (ignore-errors (find-description (class-name (class-of object))))
      (find-description 'standard-object)))
		      
		       
  

