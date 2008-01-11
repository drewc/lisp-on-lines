(in-package :lisp-on-lines)

(define-description standard-object ()
  ((class-slots :label "Slots" 
		:function (compose 'class-slots 'class-of))))

(define-layered-method description-of ((object standard-object))
 (find-description 'standard-object))


		       
  

