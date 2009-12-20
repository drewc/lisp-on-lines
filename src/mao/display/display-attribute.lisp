(in-package :lisp-on-lines)

(define-layered-class display-attribute (standard-attribute)
 ((label 
   :layered-accessor attribute-label 
   :initarg :label
   :initform nil
   :layered t
   :special t)
  (label-formatter 
   :layered-accessor attribute-label-formatter
   :initarg :label-formatter
   :initform  nil 
   :layered t
   :special t)
  (value-formatter 
   :layered-accessor attribute-value-formatter
   :initarg :value-formatter
   :initform nil
   :layered t
   :special t)

))

(define-layered-method attribute-label-formatter :around (attribute)
   (or (slot-value attribute 'label-formatter) 
       (attribute-value (find-attribute (attribute-description attribute) 'label-formatter))
       (error "No Formatter .. fool!")))

(define-layered-method attribute-value-formatter :around (attribute)
		       
   (or (slot-value attribute 'value-formatter) 
       (attribute-value (find-attribute (attribute-description attribute) 'value-formatter))
       (error "No Formatter .. fool!")))