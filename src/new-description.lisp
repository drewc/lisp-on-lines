(in-package :lisp-on-lines)

(setf (find-class 'simple-attribute nil) nil)

(define-layered-class simple-attribute ()
  ((%property-access-function 
    :initarg property-access-function)))

(defun ensure-property-access-function (attribute)
  (if (slot-boundp attribute '%property-access-function)
      (slot-value attribute '%property-access-function)
      (let ((fn-name (gensym))) 
	(ensure-layered-function fn-name :lambda-list '() :method-combination '(append))
	(setf (slot-value attribute '%property-access-function) fn-name))))

(defconstant +property-not-found+ '=lisp-on-lines-property-not-found-indicator=)

(define-layered-method 
    contextl:slot-value-using-layer (class (attribute simple-attribute) slotd reader)
  (if (or *symbol-access*  
	  (eq (slot-definition-name slotd) 
	      '%property-access-function)
	  (not (slot-definition-layeredp slotd)))
      (call-next-method)
      (let ((value (getf (funcall (ensure-property-access-function attribute))
			 (slot-definition-name slotd)
			 +property-not-found+)))
	(if (eq value +property-not-found+)
	    (call-next-method)
	    value))))

(defvar *test-attribute-definitions*
  `((t :label "foo" :value "foo")
    (simple-test-layer :label "BAZ" :value "BAZ")))

(defmethod initialize-attribute-for-layer (attribute layer-name &rest args)
  (let* ((class (class-of attribute))
	 (slotds (class-slots class)))
    
    (ensure-layered-method 
     (ensure-property-access-function attribute)
     `(lambda ()
	',(loop 
		     :for (key val) :on args :by #'cddr 
		     :nconc (list 
			     (loop :for slotd :in slotds 
				:do (when (find key (slot-definition-initargs slotd))
				      (return  (slot-definition-name slotd))))
			     val))) 
     :qualifiers '(append)
     :in-layer layer-name)))



(define-layered-class simple-standard-attribute (simple-attribute)
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


(define-layered-class direct-attribute-slot-definition-class 
  (special-layered-direct-slot-definition 
   contextl::singleton-direct-slot-definition)
  ((attribuite-properties
    :accessor slot-definition-attribute-properties
    :documentation "Holds the initargs passed to the slotd")))

(defmethod initialize-instance 
    :after ((slotd direct-attribute-slot-definition-class) 
	    &rest initargs)
  (setf (slot-definition-attribute-properties slotd) initargs))

(defmethod reinitialize-instance 
    :after ((slotd direct-attribute-slot-definition-class) 
	    &rest initargs)
  (setf (slot-definition-attribute-properties slotd) initargs))

(define-layered-class effective-attribute-slot-definition-class 
    (special-layered-effective-slot-definition) 
    ((attribute-object 
      :accessor slot-definition-attribute-object)))

(define-layered-class description-access-class (standard-layer-class contextl::special-layered-access-class)
  ((defined-in-descriptions :initarg :in-description)
   (class-active-attributes-definition :initarg :attributes)
   (mixin-class-p :initarg :mixinp)))

(defmethod direct-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'direct-attribute-slot-definition-class))

(defmethod effective-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'effective-attribute-slot-definition-class))
(fmakunbound 'initialize-slot-definition-attribute)
(defmethod initialize-slot-definition-attribute ((slotd effective-attribute-slot-definition-class) name direct-slot-definitions)
  (let ((tbl (make-hash-table))
	(attribute (make-instance 'simple-standard-attribute :name name)))
    (loop for ds in direct-slot-definitions 
       :do (setf (gethash (slot-definition-layer ds) tbl)
		 (append (gethash (slot-definition-layer ds) tbl '()) 
			 (slot-definition-attribute-properties ds))))
    (maphash (lambda (layer properties)
	       (apply #'initialize-attribute-for-layer attribute layer properties))
	     tbl)
    (setf (slot-definition-attribute-object slotd) attribute)))

(defmethod compute-effective-slot-definition
           ((class description-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slotd (call-next-method)))
    (initialize-slot-definition-attribute slotd) 
    slotd))

(defclass standard-description-class (description-access-class layered-class)
  ((attributes :accessor description-class-attributes :initform (list)))
  (:default-initargs :defining-metaclass 'description-access-class))

(defmethod validate-superclass
           ((class standard-description-class)
            (superclass standard-class))
  t)

(define-layered-class standard-description-object (standard-layer-object) 
  ((described-object :accessor described-object 
		     :special t)))

(defun initialize-description-class-attribute (description attribute initargs)
  )

(defmethod initialize-instance :around ((class standard-description-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (prog1
      (if (loop for direct-superclass in direct-superclasses
		thereis (ignore-errors (subtypep direct-superclass 'standard-description-object)))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (find-class 'standard-description-object)))
	       initargs))
    (break "initializing ~A ~A" class initargs)))


(defmethod reinitialize-instance :around ((class standard-description-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
;  (warn "CLASS ~A ARGS ~A:" class initargs)
  (prog1
      (if (or (not direct-superclasses-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass 'standard-description-object))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (find-class 'standard-description-object)))
		 initargs))
    (break "RE-initializing ~A ~A" class initargs)))

(defmethod finalize-inheritance :after ((class standard-description-class))
  (break "Finalizing ~S" (class-name  class)))

;;;; A simpler implementation of descriptions based on plists



