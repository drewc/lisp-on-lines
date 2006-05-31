(in-package :lisp-on-lines)

;;;; Validation Conditions

(define-condition validation-condition ()
  ((message :accessor message :initarg :message :initform "Invalid value")
   (value :accessor value :initarg :value :initform (warn "condition was not given a value"))))

;;;; ** Attributes
(define-condition attribute-validation-condition (validation-condition)
  ((attribute :accessor attribute :initarg :attribute :initform nil)))

(defgeneric validate-attribute (instance attribute &optional value)
  (:documentation "
Returns T if the ATTRIBUTE-VALUE in INSTANCE passes all the validation functions. Otherwise, returns (values nil conditions) where CONDITIONS is a list of conditions representing the validation errors the slot.")
  (:method (instance attribute &optional (value nil value-provided-p))
    (let ((val (if value-provided-p
		     value
		     (attribute-value instance attribute)))
	  (conditions))
      (handler-bind ((attribute-validation-condition
		      #'(lambda (c)
			  (setf conditions (cons c conditions))
			  (signal c))))
	
	  (dolist (f (find-validation-functions instance attribute))
	    (funcall f instance attribute val)))
      (if conditions
	  (values nil conditions)
	  t))))


(defmethod find-validation-functions (instance (attribute standard-attribute))
  (let ((foo  (validate-using attribute)))
    (warn "validation?~A " foo)
    foo))


;;;; ** Instances
(define-condition instance-validation-condition (validation-condition)
  ((instance :accessor instance :initarg instance :initform nil)
   (conditions :accessor conditions :initarg :conditions :initform nil)))

(defmethod invalid-instance-p (instance attributes)
  (let (condition)
    (handler-bind ((instance-validation-condition
		  #'(lambda (c)
		      (setf condition c))))
      (validate-instance instance attributes))
    condition))
  
(defmethod validate-instance (instance attributes)
  (let (all-conditions)
    (dolist (att attributes)
      (multiple-value-bind (is-valid-p conditions)
	  (validate-attribute instance att)
	(unless is-valid-p
	  (setf all-conditions (nconc conditions all-conditions)))))
    (if all-conditions
	(progn (signal 'instance-validation-condition
		       :message "Invalid Instance"
		       :instance instance
		       :conditions all-conditions)
	       (values nil all-conditions))
	
	t)))


;;;; Attribute Validation Functions
;;;; I have not quite figured all this out yet.
;;;; A generic validation system needs more thought than i've given it, but this is a start.

(defun validate-string-exists (instance attribute value)
    (if (or
	 (not (stringp value))
	 (not (< 0 (length value))))
	(signal 'attribute-validation-condition
		:message (format nil "You must enter a value for ~A."
				 (label attribute))
		:attribute attribute)))


(defun validate-true (instance attribute value)
    (unless value 
      (signal 'attribute-validation-condition
	      :message (format nil "~A is required."
			       (label attribute))
	      :attribute attribute)))