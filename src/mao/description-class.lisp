(in-package :lisp-on-lines)

;;;; SLOT-DEFINITION META-OBJECTS
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
    ((direct-slots :accessor slot-definition-direct-slots) 
     (attribute-object 
      :accessor slot-definition-attribute-object)))

;;;; DESCRIPTION-ACCESS-CLASS, the PARTIAL-CLASS defining class for DESCRIPTIONs
(define-layered-class description-access-class 
 (standard-layer-class contextl::special-layered-access-class)
  ((defined-in-descriptions :initarg :in-description)
   (class-active-attributes-definition :initarg :attributes)
   (mixin-class-p :initarg :mixinp)
   (description-name :initarg original-name
               :initform nil
               :reader description-original-name)))

(defmethod direct-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'direct-attribute-slot-definition-class))

(defmethod effective-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'effective-attribute-slot-definition-class))


;;;;STANDARD-DESCRIPTION
(defclass standard-description-class (description-access-class layered-class)
  ((attributes :accessor description-class-attributes :initform (make-hash-table :test #'eq)))
  (:default-initargs :defining-metaclass 'description-access-class))

(defclass standard-description-object
    (standard-layer-object)
    ((described-object :accessor described-object 
		       :special t
		       :function 'identity)
     (ACTIVE-ATTRIBUTES :LABEL "Attributes" :VALUE NIL :ACTIVEP NIL
			:KEYWORD :ATTRIBUTES)
     (ACTIVE-DESCRIPTIONS :LABEL "Active Descriptions" :VALUE NIL
			  :ACTIVEP NIL :KEYWORD :ACTIVATE)
     (INACTIVE-DESCRIPTIONS :LABEL "Inactive Descriptions" :VALUE NIL
			    :ACTIVEP NIL :KEYWORD :DEACTIVATE))
    (:METACLASS description-access-class)
    (ORIGINAL-NAME . STANDARD-DESCRIPTION-OBJECT))


(defgeneric find-attribute (description-designator attribute-name &optional errorp)
  (:method ((description standard-description-class) attribute-name &optional (errorp t))
    (or (gethash attribute-name (description-class-attributes description))
	(when errorp
	  (when errorp (error "No attribute named ~A found in class ~A" attribute-name description)))))
  (:method ((description standard-description-object) attribute-name &optional (errorp t))
    (find-attribute (class-of description) attribute-name errorp))
  (:method ((description symbol) attribute-name &optional (errorp t))
    (find-attribute (find-description description) attribute-name errorp)))

(defgeneric (setf find-attribute) (value description attribute-name)
  (:method (value (description standard-description-class) attribute-name)
    (setf (gethash attribute-name (description-class-attributes description)) value)))

(defmethod description-class-attribute-class (description)
  'standard-attribute)


(defmethod initialize-slot-definition-attribute 
    (class (slotd effective-attribute-slot-definition-class) 
     name direct-slot-definitions)
  (let ((tbl (make-hash-table)))
    (loop for ds in direct-slot-definitions
       :when (typep ds 'direct-attribute-slot-definition-class)
       :do (setf (gethash (slot-definition-layer ds) tbl)
		 (append (gethash (slot-definition-layer ds) tbl '()) 
			 (slot-definition-attribute-properties ds))))

    (let* ((attribute-class (or (block nil  
				  (maphash (lambda (k v)
					     (let ((class (getf v :attribute-class)))
					       (when class (return class))))
					   tbl))
				(description-class-attribute-class class)))
	   (attribute (apply #'make-instance attribute-class :name name 'description-class class (gethash t tbl))))
      (maphash (lambda (layer properties)
		 (pushnew layer (attribute-layers attribute))
		 (apply #'initialize-attribute-for-description class attribute layer properties))
	       tbl)
      (setf (slot-definition-attribute-object slotd) attribute)
      (setf (find-attribute class name) attribute))))

(defmethod compute-effective-slot-definition
           ((class standard-description-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slotd (call-next-method)))
    (setf (slot-definition-direct-slots slotd) direct-slot-definitions)
    (when (class-finalized-p class)
      (initialize-slot-definition-attribute class slotd name direct-slot-definitions)) 
    slotd))

(defmethod finalize-inheritance :after ((class standard-description-class))
  (dolist (slotd (compute-slots class))
    (initialize-slot-definition-attribute class slotd (slot-definition-name slotd) (slot-definition-direct-slots slotd))))

(defmethod validate-superclass
           ((class standard-description-class)
            (superclass standard-class))
  t)

(defmacro defdescription (name &optional superdescriptions &body options)
  (destructuring-bind (&optional slots &rest options) options
    `(let ((description-name ',name))
       (declare (special description-name)) 
       (deflayer ,(defining-description name) ,(mapcar #'defining-description superdescriptions)
	 ,(if slots slots '())
	 ,@options
	 ,@(unless (assoc :metaclass options)
		   '((:metaclass standard-description-class)))
	 ,@(let ((in-description (assoc :in-description options)))
	    (when in-description
	      `((:in-layer . ,(defining-description (cadr in-description))))))
	 
	 (original-name . ,name)))))



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
	       initargs))))

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
		 initargs))))

(defun find-description (name &optional (errorp t))
  (find-layer (defining-description  name)  errorp))

(defun description-class-name (description-class)
  (ignore-errors  (description-original-name (first (class-direct-superclasses description-class)))))

(defmethod print-object ((class standard-description-class) stream)
       (print-unreadable-object (class stream :type nil :identity t)
	 (format stream "DESCRIPTION-CLASS ~A" (description-class-name class))))

(defun description-name  (description)
  (description-class-name  (class-of description)))

(defmethod print-object ((object standard-description-object) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "DESCRIPTION ~A" (description-name object))))