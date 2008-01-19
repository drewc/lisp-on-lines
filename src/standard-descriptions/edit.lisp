(in-package :lisp-on-lines)


(define-description editable ()
  ()
  (:mixinp t))

(define-description T ()
  ((editp :label "Edit by Default?"
	  :value nil 
	  :editp nil)
   (identity :editp nil)
   (type :editp nil)
   (class :editp nil))
  (:in-description editable))

(define-layered-function (setf attribute-value) (v o a)
  (:method (value object attribute)
    (let ((setter (attribute-setter attribute)))
      (if setter
	  (funcall setter value object)
	  (error "No setter in ~A for ~A" attribute object)))))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'editable)
  ()
  ((edit-attribute-p 
    :initform :inherit 
    :accessor %attribute-editp
    :initarg :editp
    :layered t)
   (setter
    :initarg :setter
    :layered t
    :accessor attribute-setter
    :initform nil)))

(define-layered-function attribute-editp (object attribute)
  (:method (object attribute) nil))

(define-layered-method attribute-editp 
  :in-layer #.(defining-description 'editable)
  (object (attribute standard-attribute))
		       
  (if (eq :inherit (%attribute-editp attribute))
      (attribute-value object (find-attribute 
			       (attribute-description attribute) 
			       'editp))
      (%attribute-editp attribute)))
		       

(define-layered-method display-using-description 
  :in-layer #.(defining-description 'editable)
  ((attribute standard-attribute) display object &rest args)
  
  (declare (ignore args))
  (format t "Editable? ~A ~A" (attribute-label attribute) (attribute-editp object attribute)))


		       