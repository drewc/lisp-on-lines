(in-package :lisp-on-lines)

(define-description standard-object ()
  ((class-slots :label "Slots" 
		:function (compose 'class-slots 'class-of))))

(define-layered-class slot-definition-attribute (standard-attribute)
 ((slot-name :initarg :slot-name :accessor attribute-slot-name)))

(define-layered-method attribute-value (object (attribute slot-definition-attribute))
  (slot-value object (attribute-slot-name attribute)))
		       
		      
(define-layered-method description-of ((object standard-object))
 (find-description 'standard-object))


		       
  

