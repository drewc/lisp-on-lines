(in-package :lisp-on-lines)


(define-layered-class list-attribute (define-description-attribute)
 ((item-args :initform nil :initarg :item :layered t :special t)))

(define-layered-method display-attribute-value 
  ((attribute list-attribute))
  (generic-format *display* "(")
  (let ((list (attribute-value attribute)))
    
    (loop 
       :for cons :on list
       :do (let ((item (first cons
))) 
	     (dletf (((attribute-object attribute) item))
	       (apply #'display *display* item (slot-value attribute 'item-args))
	       (unless (endp (cdr cons))
		 (generic-format *display* " "))))))
  (generic-format *display* ")"))
	   
       
 
	   


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
		       





