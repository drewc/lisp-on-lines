(in-package :lisp-on-lines)

(define-description inline ())

(define-description t ()
  ((identity :label nil)
   (active-attributes :value '(identity))
   (attribute-delimiter :value ", ")
   (label-formatter :value (curry #'format nil "~A: "))
   (value-formatter :value (curry #'format nil "~A")))
  (:in-description inline))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'inline)
  ()
  ())


(define-display :in-description inline ((description t))		
		(call-next-method))
