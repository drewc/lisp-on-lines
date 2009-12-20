(in-package :lisp-on-lines)

(defclass display-description-class (standard-description-class)
  ())

(defmethod description-class-attribute-class ((class display-description-class))
  'display-attribute)

(defun label-for-object (object)
  (format nil "~@(~A~)" 
	  (substitute #\Space #\- 
		      (symbol-name 
		       (class-name (class-of 
				    object))))))
#+nil(defdescription t ()
  ((label :label nil 
	  :function label-for-object)
   (identity :label nil :function identity)
   (type :label "Type" :function type-of)
   (class :label "Class" :function class-of)
   (attribute-delimiter :label "Attribute Delimiter"
			:value "~%"
			:activep nil
			:keyword :delimter)

   (label-formatter :value princ-to-string
		    :activep nil)
   (value-formatter :value princ-to-string
		    :activep nil))
  (:metaclass standard-description-class))

#+nil(defmethod initialize-instance :around ((class display-description-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (prog1
      (if (loop for direct-superclass in direct-superclasses
		thereis (ignore-errors (subtypep direct-superclass (class-of (find-description t)))))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (class-of (find-description 't))))
	       initargs))))


#+nil(defmethod reinitialize-instance :around ((class display-description-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
;  (warn "CLASS ~A ARGS ~A:" class initargs)
  (prog1
      (if (or (not direct-superclasses-p)
		(loop for direct-superclass in direct-superclasses
		      thereis (ignore-errors (subtypep direct-superclass (class-of (find-description t))))))
	  (call-next-method)
	  (apply #'call-next-method
		 class
		 :direct-superclasses
		 (append direct-superclasses
			 (list (class-of (find-description 't))))
		 initargs))))