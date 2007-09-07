(in-package :lisp-on-lines)

;;;; * Occurences
;;;; Occurences can be thought of as the class of a description. 
;;;; Most of the occurence stuff is depreciated now.

     "an occurence holds the attributes like a class holds slot-definitions.
Attributes are the yetadata used to display, validate, and otherwise manipulate actual values stored in lisp objects."

(defun find-or-create-occurence (name)
  "Returns the occurence associated with this name."
  (let ((description (find-description name)))
    (if description
	(class-of description)
	(class-of (ensure-description name)))))

(defun clear-occurence (occurence)
  "removes all attributes from the occurence"
  (setf (attribute-map occurence) (make-hash-table)))

(defgeneric find-occurence (name)
  (:method (thing)
    nil)
  (:method ((name symbol))
    (find-or-create-occurence name))
  (:method ((instance standard-object))
    (multiple-value-bind (occ new?)
	(find-or-create-occurence (class-name-of instance))
      (if new?
	  (initialize-occurence-for-instance occ instance)
	  occ))))

(defun list-attributes (occurence)
  (let (res)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k res))
	     (attribute-map occurence))
    res))


(defmethod make-attribute-using-slot-definition (slotd)
  (make-attribute
   :name (closer-mop:slot-definition-name slotd)
   :type-spec (closer-mop:slot-definition-type slotd)
   :type (first (remove-if (lambda (item)
			     (or
			      (eql item 'or)
			      (eql item 'null)
			      (eql item nil)))
			   (ensure-list (closer-mop:slot-definition-type slotd))))))

(defmethod initialize-occurence-for-instance (occurence instance)
  (let ((slots (closer-mop:class-slots (class-of instance))))
    (dolist (s slots)
      (let ((att (make-attribute-using-slot-definition s)))
	(setf (find-attribute occurence (attribute-name att)) att)))
    occurence))




;;;; * Attributes

(define-layered-class
    attribute (description)
    ((attribute-name :layered-accessor attribute-name
	   :initarg :name
	   :initform (gensym "ATTRIBUTE-")
	   :special t)
     (occurence :accessor occurence :initarg :occurence :initform nil)
     (label :initarg :label :layered-accessor label :initform nil :special t)))


(defmethod print-object ((self attribute) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (attribute-name description-type) self
      (format stream "~A ~A" description-type attribute-name))))

(define-layered-class
    standard-attribute (attribute)
    ((setter :accessor setter :initarg :setter :special t :initform nil)
     (getter :accessor getter :initarg :getter :special t :initform nil)
     (value :accessor value :initarg :value :special t)
     (slot-name :accessor slot-name :initarg :slot-name :special t :initform nil)
     (typespec :accessor type-spec :initarg :type-spec :initform nil))
    (:documentation "Attributes are used to display a part of a thing, such as a slot of an object, a text label, the car of a list, etc."))

(define-layered-method label :around ((attribute standard-attribute))
 (or (call-next-method) (attribute-name attribute)))

(defmacro defattribute (name supers slots &rest args)
  (let* (
	(type-provided-p (second (assoc :type-name args)))
	(type (or type-provided-p name))
	(layer (or (second (assoc :in-layer args)) nil))
	(properties (cdr (assoc :default-properties args)))
	(cargs (remove-if #'(lambda (key)
		   (or (eql key :type-name)
		       (eql key :default-properties)
		       (eql key :default-initargs)
		       (eql key :in-layer)))
			 args
	       :key #'car)))
    
    `(progn
      (define-layered-class
	  ;;;; TODO: fix the naive way of making sure s-a is a superclass
	  ;;;; Need some MOPey goodness.
	  ,name ,@ (when layer `(:in-layer ,layer)),(or supers '(standard-attribute))
	  ,(append slots (properties-as-slots properties)) 
	  #+ (or) ,@ (cdr cargs)
	  ,@cargs
	  (:default-initargs :properties (list ,@properties)
	    ,@ (cdr (assoc :default-initargs args))))

      ,(when (or
	     type-provided-p
	     (not (find-attribute-class-for-type name)))
	 `(defmethod find-attribute-class-for-type ((type (eql ',type)))
	   ',name)))))

(defun clear-attributes (name)
  "removes all attributes from an occurance"
  (clear-occurence (find-occurence name)))

(defmethod find-attribute-class-for-type (type)
  nil)

(defun make-attribute (&rest args &key type &allow-other-keys)
  (apply #'make-instance
	 (or (find-attribute-class-for-type type)
	     'standard-attribute)
	 :properties args
	 args)) 

(defmethod ensure-attribute ((occurence description) &rest args &key name &allow-other-keys)
  "Creates an attribute in the given occurence"
  (let ((attribute (apply #'make-attribute :occurence occurence args)))
    (setf (find-attribute occurence name) attribute)))

(defmethod find-attribute ((occurence null) name)
  nil)

(defmethod find-attribute ((occurence description) name)
  (or (gethash name (attribute-map occurence))
      (let* ((class (ignore-errors (find-class (name occurence))))
	     (class-direct-superclasses
	      (when class
		(closer-mop:class-direct-superclasses
		 class))))
	(when class-direct-superclasses 
	  (let ((attribute
		 (find-attribute
		  (find-occurence (class-name
				   (car
				    class-direct-superclasses)))
		  name)))
	    attribute)))))

(defmethod find-all-attributes ((occurence description))
  (loop for att being the hash-values of (attribute-map occurence)
     collect att))

(defmethod ensure-attribute (occurence-name &rest args &key name type &allow-other-keys)
  (declare (ignore name type))
  (apply #'ensure-attribute
   (find-occurence occurence-name)
   args)) 

;;;; The following functions make up the public interface to the
;;;; MEWA Attribute Occurence system.

(defmethod find-all-attributes (occurence-name)
  (find-all-attributes (find-occurence occurence-name)))

(defmethod find-attribute (occurence-name attribute-name)
  "Return the ATTRIBUTE named by ATTRIBUTE-NAME in OCCURANCE-name"
  (find-attribute (find-occurence occurence-name) attribute-name))

(defmethod (setf find-attribute) ((attribute-spec list) occurence-name attribute-name)
  "Create a new attribute in the occurence.
ATTRIBUTE-SPEC: a list of (type name &rest initargs)"
  (apply #'ensure-attribute occurence-name :name attribute-name :type (first attribute-spec) (rest attribute-spec)))

(defmethod (setf find-attribute) ((attribute standard-attribute) occurence attribute-name)
  "Create a new attribute in the occurence.
ATTRIBUTE-SPEC: a list of (type name &rest initargs)"
  (setf (gethash attribute-name (attribute-map occurence))
	  attribute))

(defmethod (setf find-attribute) ((attribute null) occurence attribute-name)
  "Create a new attribute in the occurence.
ATTRIBUTE-SPEC: a list of (type name &rest initargs)"
  (setf (gethash attribute-name (attribute-map occurence))
	  attribute))

(defmethod find-attribute ((attribute-with-occurence attribute) attribute-name)
  (find-attribute (occurence attribute-with-occurence) attribute-name))

(defmethod set-attribute-properties ((occurence-name t) attribute properties)
  (setf (description-properties attribute) (plist-nunion
					    properties
					    (description-properties attribute)))
  (loop for (initarg value) on (description-properties attribute) 
	      by #'cddr
	      with map = (initargs.slot-names attribute)
	      do (let ((s-n (assoc-if #'(lambda (x) (member initarg x)) map)))
		   
		   (if s-n
		       (progn
			 (setf (slot-value attribute
					   (cdr s-n))
			       value))
		       (warn "Cannot find initarg ~A in attribute ~S" initarg attribute)))
	      finally (return attribute)))

(defmethod set-attribute (occurence-name attribute-name attribute-spec &key (inherit t))
  "If inherit is T, sets the properties of the attribute only, unless the type has changed.
otherwise, (setf find-attribute)"
  (let ((att (find-attribute occurence-name attribute-name)))
    (if (and att inherit (or (eql (car attribute-spec)
			      (description-type att))
			     (eq (car attribute-spec) t)))
	(set-attribute-properties occurence-name att (cdr attribute-spec))
	(setf (find-attribute occurence-name attribute-name)
	      (cons  (car attribute-spec)
		     (plist-nunion
		      (cdr attribute-spec) 
		      (when att (description-properties att))))))))

(defmethod perform-define-attributes ((occurence-name t) attributes)
  (loop for attribute in attributes
	do (destructuring-bind (name type &rest args)
		  attribute
	     (cond ((not (null type))
		    ;;set the type as well
		    (set-attribute occurence-name name (cons type args)))))))
		       
(defmacro define-attributes (occurence-names &body attribute-definitions)
  `(progn
    ,@(loop for occurence-name in occurence-names
	    collect `(perform-define-attributes (quote ,occurence-name) (quote ,attribute-definitions)))))




;;"Unused???"
(defmethod setter (attribute)
  (warn "Setting ~A in ~A" attribute *context*)
  (let ((setter (getf (description-properties attribute) :setter))
	(slot-name (getf (description-properties attribute) :slot-name)))
    (cond (setter
	   setter)
	  (slot-name
	   #'(lambda (value object)
	       (setf (slot-value object slot-name) value)))
	  (t
	   #'(lambda (value object)
	       (warn "Can't find anywere to set ~A in ~A using ~A" value object attribute))))))
    

(define-layered-function attribute-value (instance attribute)
  (:documentation " Like SLOT-VALUE for instances, the base method calls GETTER."))

(defmethod attribute-slot-value (instance attribute)
  "Return (VALUES slot-value-or-nil existsp boundp

If this attribute, in its current context, refers to a slot,
we return slot-value-or nil either boundp or not."
  (let (existsp boundp slot-value-or-nil)
    (cond 
      ((and (slot-boundp attribute 'slot-name) (slot-name attribute))
	 (when (slot-exists-p instance (slot-name attribute))
	   (setf existsp t)
	   (when (slot-boundp instance (slot-name attribute))
	     (setf boundp t
		   slot-value-or-nil (slot-value
				      instance
				      (slot-name attribute))))))
	((and (slot-exists-p instance (attribute-name attribute)))
	 (setf existsp t)
	 (when (slot-boundp instance (attribute-name attribute))
	   (setf boundp t
		 slot-value-or-nil (slot-value
				    instance
				    (attribute-name attribute))))))
    (VALUES slot-value-or-nil existsp boundp)))
  
(define-layered-method attribute-value (instance (attribute standard-attribute))
 "return the attribute value or NIL if it cannot be found"		       
    (with-slots (getter value) attribute
      (when (slot-boundp attribute 'value)
	(setf getter (constantly value)))
      (if (and (slot-boundp attribute 'getter) getter)
	  ;;;; call the getter
	  (funcall getter instance)
	  ;;;; or default to the attribute-slot-value
	  (attribute-slot-value instance attribute))))

(define-layered-function (setf attribute-value) (value instance attribute))

(define-layered-method
    (setf attribute-value) (value instance (attribute standard-attribute))
  (with-slots (setter slot-name) attribute 
    (cond ((and (slot-boundp attribute 'setter) setter)
	   (funcall setter value instance))
	  ((and (slot-boundp attribute 'slot-name) slot-name)
	   (setf (slot-value instance slot-name) value))
	  ((and (slot-exists-p instance (attribute-name attribute)))
	   (setf (slot-value instance (attribute-name attribute)) value))
	  (t
	   (error "Cannot set ~A in ~A" attribute instance)))))



;;;; ** Default Attributes
;;;; TODO: This is mosty an ugly hack and should be reworked.
;;;; 
;;;; The default mewa class contains the types use as defaults.
;;;; maps meta-model slot-types to slot-presentation

(defvar *default-attributes-class-name* 'default)

(defmacro with-default-attributes ((occurence-name) &body body)
  `(let ((*default-attributes-class-name* ',occurence-name))
    ,@body))

(define-attributes (default)
  (boolean boolean)
  (string string)
  (number currency)
  (integer   integer)
  (currency  currency)
  (clsql:generalized-boolean boolean)
  (foreign-key has-a))

(defun attribute-to-definition (attribute)
  (nconc (list (attribute-name attribute)
	       (description-type attribute))
	 (description-properties attribute)))

(defun find-default-presentation-attribute-definitions ()
  nil)

(defun gen-ptype (type)
  (let* ((type (if (consp type) (car type) type))
	 (possible-default (find-attribute *default-attributes-class-name* type))
	 (real-default (find-attribute 'default type)))
    (cond
      (possible-default
	(description-type possible-default))
       (real-default
	(description-type real-default))
       (t type))))

(defun gen-presentation-slots (instance)
  (mapcar #'(lambda (x) (gen-pslot (cadr x) 
				   (string (car x)) 
				   (car x))) 
	  (meta-model:list-slot-types instance)))


(defun gen-pslot (type label slot-name)
  (copy-list `(,(gen-ptype type) 
	       :label ,label
	       :slot-name ,slot-name))) 

;; This software is Copyright (c) Drew Crampsie, 2004-2005.
