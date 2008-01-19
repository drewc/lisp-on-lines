(in-package :lisp-on-lines)

(define-description standard-object ()
  ((class-slots :label "Slots" 
		:function (compose 'class-slots 'class-of))))

(define-layered-class slot-definition-attribute (standard-attribute)
 ((slot-name :initarg :slot-name :accessor attribute-slot-name)))

(define-layered-method attribute-value (object (attribute slot-definition-attribute))
  (if (slot-boundp object (attribute-slot-name attribute))
		       
      (slot-value object (attribute-slot-name attribute))
      (gensym "UNBOUND-SLOT-")))

(defmacro define-description-for-class (class-name &optional (name (intern (format nil "DESCRIPTION-FOR-~A" class-name))))
  `(progn 
     (define-description ,name (standard-object)
       ,(loop :for slot in (class-slots (find-class class-name))
	  :collect `(,(slot-definition-name slot) 
		    :attribute-class slot-definition-attribute
		    :slot-name ,(slot-definition-name slot)
		    :label ,(slot-definition-name slot)))
       (:mixinp t))
     (unless (ignore-errors (find-description ',class-name))
       (define-description ,class-name (,name) ()))))
    
		       
		      
(define-layered-method description-of ((object standard-object))
  (or (ignore-errors (find-description (class-name (class-of object))))
      (find-description 'standard-object)))
		      
		       
  

