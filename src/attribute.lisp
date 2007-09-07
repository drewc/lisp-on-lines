(in-package :lisp-on-lines)


(define-layered-class attribute ()
  ())

(defgeneric eval-attribute-initarg (attribute initarg)
  (:method (a i)
    nil))

(defmethod eval-attribute-initarg (attribute (initarg (eql :function)))
  t)
(define-layered-function attribute-value (object attribute))


	       
(deflayer LISP-ON-LINES)
(ensure-active-layer 'lisp-on-lines)

(defvar *standard-direct-slot-initarg-symbols*
    '(:layered :class :in-layer :name :readers :writers :initargs :allow-other-keys :special))

(define-layered-function special-slot-values (description slot-name)
  (:method-combination append))

(define-layered-class attribute-special-layered-direct-slot-definition 
  (attribute contextl::special-layered-direct-slot-definition) 
  (initargs))

(defmethod shared-initialize :around ((instance attribute-special-layered-direct-slot-definition) slots &rest initargs )
  (setf (slot-value instance 'initargs) 
	(apply #'arnesi:remove-keywords initargs *standard-direct-slot-initarg-symbols*))
  (call-next-method))

(define-layered-class standard-attribute 
  (attribute contextl::layered-effective-slot-definition-in-layers) 
  ((direct-slots)
   (description 
    :layered-accessor description-of)
   (label 
    :initarg :label 
    :layered-accessor attribute-label
    :layered t
    :initform nil)
   (function 
    :initarg :function 
    :layered-accessor attribute-function
    :layered t)
   (value 
    :initarg :value
    :layered t)))

(define-layered-method attribute-value (object attribute)
 (funcall (attribute-function attribute) object))

(defmethod shared-initialize :around ((attribute standard-attribute) slots &rest initargs)
  (declare (ignore initargs))
    (setf (attribute-function attribute) 
	(lambda (object)
	  (slot-value attribute 'value)))
  (call-next-method))

(defun attribute-name (attribute)
  (closer-mop:slot-definition-name attribute))

(define-layered-method slot-value-using-layer 
;  :in-layer lisp-on-lines
  :around (class (attribute standard-attribute) slot reader)
  (loop for (key var) on (special-slot-values (slot-value attribute 'description) 
						     (attribute-name attribute))
	      :if (eq (closer-mop:slot-definition-name slot) key)
	      :do (return-from slot-value-using-layer var))
  (call-next-method))
	
(define-layered-method display-using-description 
  ((attribute standard-attribute) display object &rest args)
 (declare (ignore args))
 (format display "~@[~A ~]~A" (attribute-label attribute) 
	 (attribute-value object attribute)))





		       
	


