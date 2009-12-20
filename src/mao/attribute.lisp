
(in-package :lisp-on-lines)

(define-layered-class attribute ()
 ())

(define-layered-class standard-attribute (simple-plist-attribute)
 ((attribute-layers :accessor attribute-layers :initform nil)
  (name 
   :layered-accessor attribute-name 
   :initarg :name)
  (effective-attribute-definition 
    :initarg effective-attribute
    :accessor attribute-effective-attribute-definition)
#+nil  (attribute-class 
   :accessor attribute-class 
   :initarg :attribute-class 
  :initform 'standard-attribute)
  (keyword
   :layered-accessor attribute-keyword
   :initarg :keyword
   :initform nil
   :layered t)
  (activep 
   :layered-accessor attribute-active-p
   :initarg :activep ;deprecated
   :initarg :active
   :initform t
   :layered t
   :special t
   :documentation
   "Can be T, NIL or :WHEN. In the latter case, attribute is only active if the attribute value is non-null.")
  (value 
   :layered-accessor attribute-value 
   :initarg :value
   :layered t
   :special t)
  (function 
   :initarg :function 
   :layered-accessor attribute-function
   :layered t
   :special t)
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
		       :special t)
  ))

(defmethod attribute-description ((attribute standard-attribute))
  (find-layer (attribute-description-class attribute)))

(define-layered-function attribute-object (attribute))
(define-layered-method attribute-active-p :around (attribute)		       
 (let ((active? (call-next-method)))
   (if (eq :when active?)
       (not (null (attribute-value attribute)))
       active?)))
		       

(define-layered-method attribute-object ((attribute standard-attribute))
  (described-object (dynamic description)))

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

(defmethod print-object ((object standard-attribute) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "ATTRIBUTE ~A" (or (ignore-errors (attribute-name object)) "+unnamed-attribute+"))))