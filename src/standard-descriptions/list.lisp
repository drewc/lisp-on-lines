(in-package :lisp-on-lines)

(define-description cons ()
  ((car :label "First" :function #'car)
   (cdr :label "Rest"  :function #'cdr)))

(define-description cons ()
  ((editp :value t :editp nil)
   (car :setter #'rplaca)
   (cdr :setter #'rplacd))
  (:in-description editable))

(define-layered-method description-of ((c cons))
 (find-description 'cons))
		       





