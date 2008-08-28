(in-package :lisp-on-lines)


(define-layered-class list-attribute (standard-attribute)
 ((item-args :initform nil :initarg :item :layered t :special t)))

(define-layered-method display-attribute-value 
  ((attribute list-attribute))
  (arnesi:dolist* (item (attribute-value attribute))
    (apply #'display *display* item (slot-value attribute 'item-args))))

(define-description list ()
 ((list :attribute-class list-attribute
	 :function #'identity
	 :attributes nil)))

(define-description cons (list)
  ((car :label "First" :function #'car)
   (cdr :label "Rest"  :function #'cdr)
   ))

(define-description cons ()
  ((editp :value t :editp nil)
   (car :setter #'rplaca)
   (cdr :setter #'rplacd))
  (:in-description editable))

(define-description cons ()
  ((active-attributes :value '(list)))
  (:in-description inline))

(define-layered-method description-of ((c cons))
 (find-description 'cons))
		       





