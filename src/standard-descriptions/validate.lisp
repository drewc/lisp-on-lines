(in-package :lisp-on-lines)

(defclass #.(defining-description 'validate) () 
  ((invalid-object-condition-map :layered t :special t ))
  (:metaclass standard-description-class))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'validate)
  ()
  ((validators 
    :initform nil 
    :layered-accessor attribute-validators
    :initarg :validate
    :layered t
    :special t)))

(define-condition validation-condition ()
  ((format-string :accessor validation-condition-format-string
		  :initarg :format-string)
   (format-args :accessor validation-condition-format-args
		  :initarg :format-args)
   (object :accessor validation-condition-object
	   :initarg :object)
   (attribute :accessor validation-condition-attribute
	   :initarg :attribute)))

(define-layered-method (setf attribute-value) 
  :in-layer #.(defining-description 'validate) 
  :around (value attribute)
  (prog1 value (when (validate-attribute-value attribute value)
		 (call-next-method))))

(defun validate-attribute-value (attribute value)
  (every #'identity (mapcar (lambda (validator-name)
			      (let ((validator (find-validator validator-name)))

				(if validator 
				    (funcall validator attribute value) 
				    (prog1 t (warn "Unkown Validator ~A" validator-name)))))
			    (attribute-validators attribute))))


(defstruct validation-info (invalid-objects))

(defvar *invalid-objects*)

(defvar *validators* (make-hash-table))

(defun register-validator (name fn)
  (setf (gethash name *validators*) fn))

(defun find-validator (name)
   (gethash name *validators*))

(register-validator 'boundp 
 (lambda (a v)
   (if (unbound-slot-value-p v)
       (prog1 nil
	 (signal (make-condition 'validation-condition 
				 :format-string "You must provide a value for ~A"
				 :format-args (list  (attribute-label a))
				 :attribute a
				 :object (attribute-object a))))
       t)))


(defun validp (object)
  (with-described-object (object nil)
    (every #'identity (mapcar (lambda (attribute)
				(validate-attribute-value attribute (attribute-value attribute)))
			      (attributes (description-of object))))))

(define-layered-method lol::display-attribute-editor 
  :in-layer #.(defining-description 'validate)
  :after (attribute)
  (let ((conditions (remove-if-not 
		     (lambda (a)
		       (eq a attribute)) 
			(gethash 
			 (attribute-object attribute) 
			 lol::*invalid-objects*)
			:key #'car)))
    (dolist (c conditions)
      (<:div :style "color:red"
	      (<:as-html 
	       (apply #'format nil (validation-condition-format-string (cdr  c))
		      (validation-condition-format-args (cdr  c))))))))



(defmethod validate-object ((description standard-description-object) object)
  (let (invalid-object?)
    (handler-bind ((validation-condition 
		    (setf invalid-object? t))))))



