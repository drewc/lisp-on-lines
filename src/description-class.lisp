(in-package :lisp-on-lines)

;;; * The Description Meta-Meta-Super class.

(defclass description-special-layered-access-class 
    (contextl::special-layered-access-class)
 ((original-name :initarg original-name)
  (description-layer :initarg description-layer)
  (instance)))
	   
(defmethod closer-mop:direct-slot-definition-class 
    ((class description-special-layered-access-class) 
     &key &allow-other-keys)
  (find-class 'attribute-special-layered-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class 
    ((class description-special-layered-access-class) 
     &key name &allow-other-keys)
    (declare (ignore name))
  (find-class 'standard-attribute))

(defmethod closer-mop:compute-effective-slot-definition :around
    ((class description-special-layered-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slotd (call-next-method)))
    (setf (slot-value slotd 'direct-slots) direct-slot-definitions)
    
    (apply #'shared-initialize slotd nil (slot-value 
					  (find t direct-slot-definitions 
						:test #'eq 
						:key #'slot-definition-layer )
					  'initargs))

    slotd))
       					   
;;; * The Description Meta-Meta class.
(defclass description-class (description-special-layered-access-class layered-class) 
  ()
  (:default-initargs :defining-metaclass 'description-special-layered-access-class))

(defun initialize-description-class (class)
  (let ((description (make-instance class)))
    (setf (slot-value class 'instance) description)
    (dolist (slotd (closer-mop:class-slots class))
      (setf (slot-value slotd 'description) description)
      (dolist (slot (slot-value slotd 'direct-slots))
	(setf (slot-value slot 'initargs) 
	      (loop 
		 :for (initarg value) 
		 :on (slot-value slot 'initargs)
		 :by #'cddr
		 :nconc (list initarg
			      (if (eval-attribute-initarg slotd initarg)
				  (eval value)
				  value))))
	(ensure-layered-method  
	 'special-slot-values
	 `(lambda (description attribute)
	    (list ,@(loop 
		 :for (initarg value) 
		 :on (slot-value slot 'initargs)
		 :by #'cddr
		 :nconc (list (list 'quote (or (find-slot-name-from-initarg 
				   (class-of slotd) initarg) initarg))
			      
				  value))))
	 :in-layer (slot-definition-layer slot) 
	 :qualifiers '(append)
	 :specializers (list class (closer-mop:intern-eql-specializer (closer-mop:slot-definition-name slotd))))))))

(defmethod closer-mop:finalize-inheritance :after ((class description-class))
  (initialize-description-class class))

(define-layered-class description ()
  ((identity :function #'identity))
  (:metaclass description-class)
  (description-layer t))

(eval-when (:load-toplevel :execute)
 (closer-mop:finalize-inheritance (find-class 'description)))

;;; The layer itself. 
#+nil(deflayer description ()
  ()
  (:metaclass description))

#+nil (defmethod print-object ((object description) stream)
  (call-next-method))

(defgeneric find-description-class (name &optional errorp)	   
  ;; !-- Sometimes it gets inited, sometimes it don't.
  (:method :around (name &optional errorp)
	   (let ((class (call-next-method)))
	     (unless (slot-boundp class 'instance)
	       (initialize-description-class class)) 
	     class))
  (:method ((name (eql t)) &optional errorp)
    (declare (ignore errorp))
    (find-class 'description t))
  (:method ((name symbol) &optional errorp)
    (or (find-class (defining-description name) errorp)
	(find-description-class t)))
  (:method ((description description) &optional errorp)
    (declare (ignore errorp))
    (class-of description)))

;;; A handy macro.
(defmacro define-description (name &optional superdescriptions &body options)
  (let ((description-name (defining-description name)))
     
    (destructuring-bind (&optional slots &rest options) options
      `(prog1
	   (defclass ,description-name ,(append (mapcar #'defining-description superdescriptions) '(description))
	     ,(if slots slots '())
	     ,@options
	     ,@(unless (assoc :metaclass options)
		       '((:metaclass description-class)))
	     (original-name . ,name))
	 (initialize-description-class (find-description-class ',description-name))))))



