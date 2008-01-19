(in-package :lisp-on-lines)

;;;; * DESCRIPTIONS
;;;; A description is an object which is used 
;;;; to describe another object.

;;; HACK:
;;; Since i'm not using deflayer, ensure-layer etc, 
;;; There are a few places where contextl gets confused 
;;; trying to locate my description layers.

;;; TODO: investigate switching to deflayer!

(defun contextl::prepare-layer (layer)
  (if (symbolp layer)
      (if (eq (symbol-package layer)
	  (find-package :description-definers))
	  layer
	  (contextl::defining-layer layer))
      
      layer))

(defmethod find-layer-class :around ((layer symbol) &optional errorp environment)
  (if (eq (symbol-package layer)
	  (find-package :description-definers))
      (find-class layer)
      (call-next-method)))

;;; #+HACK
;;; I'm having some 'issues' with 
;;; compiled code and my initialization.
;;; So this hack initializes the world.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *defined-descriptions* nil))

(defclass description-access-class (standard-layer-class contextl::special-layered-access-class )
  ((defined-in-descriptions :initarg :in-description)
   (mixin-class-p :initarg :mixinp)))

(defmethod direct-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'direct-attribute-definition-class))

(defmethod effective-slot-definition-class
           ((class description-access-class) &key &allow-other-keys)
  (find-class 'effective-attribute-definition-class))

(defmethod compute-effective-slot-definition
           ((class description-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((attribute (call-next-method)))
    (setf (attribute-direct-attributes attribute) direct-slot-definitions)
    (setf (attribute-object-initargs attribute)  
	  ;; This plist will be used to init the attribute object
          ;; Once the description itself is properly initiated.
	  (list :name name 
		'effective-attribute attribute
		'description-class class))
    attribute))
    

(defclass standard-description-class (description-access-class layered-class)
  ()
  (:default-initargs :defining-metaclass 'description-access-class))

(defmethod validate-superclass
           ((class standard-description-class)
            (superclass standard-class))
  t)

(defclass standard-description-object (standard-layer-object) ())

(defun description-class-name  (description-class)
    (read-from-string (symbol-name (class-name description-class))))
  
(defun initialize-description-class (class)

;;; HACK: initialization does not happ   en properly 
;;; when compiling and loading or something like that.
;;; Obviously i'm not sure why.
;;; So we're going to explicitly initialize things.
;;; For now. --drewc

  (pushnew class *defined-descriptions*)
  
;;; ENDHACK.

  (let* ((description (find-layer class)) 
	 (attribute-objects 
	  (mapcar 
	   (lambda (slot)
	     (setf (attribute-object slot)
		   (apply #'make-instance 
			  'standard-attribute
			  (attribute-object-initargs slot))))
	   (class-slots (class-of description))))
	 (defining-classes (partial-class-defining-classes (class-of description))))

    (loop 
       :for (layer class) 
       :on  defining-classes :by #'cddr 
       :do (funcall-with-layer-context 
	    (adjoin-layer (find-layer layer) (current-layer-context))
	    (lambda ()
	      (loop :for direct-slot :in (class-direct-slots class) 
		 :do (let ((attribute 
			    (find (slot-definition-name direct-slot) 
				  attribute-objects 
				  :key #'attribute-name)))
		       (dprint "Re-initing")
		       (apply #'reinitialize-instance attribute 
			      (print (direct-attribute-properties direct-slot)))
		       (when (not (eq (find-class (attribute-class attribute))
				  (class-of attribute)))
			   
			   (apply #'change-class attribute  (attribute-class attribute) 
				  (direct-attribute-properties direct-slot)))
		       

		       (setf (slot-value description (attribute-name attribute))
			     attribute))))))))

;;;; HACK: run this at startup till we figure things out.
(defun initialize-descriptions () 
  (map nil #'initialize-description-class 
       (setf *defined-descriptions* 
	     (remove-duplicates *defined-descriptions*))))

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
    (initialize-description-class class)))


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
    (initialize-description-class class)))
		      
		      
(defmethod print-object ((object standard-description-object) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (format stream "DESCRIPTION ~A" (ignore-errors (description-print-name object)))))

(defmethod print-object ((object standard-description-class) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ  (ignore-errors (description-print-name (find-layer object))) stream)))

(defun find-description (name)
  (find-layer (find-class (defining-description name))))






