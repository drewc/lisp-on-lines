(in-package :lisp-on-lines)

(defvar *description*)
(defvar *display*)
(defvar *object* nil)

(define-layered-function display-using-description (description display object &rest args)
  (:documentation
   "Displays OBJECT via description using/in/with/on display"))



(defun modify-layer-context (context &key activate deactivate)
  (dolist (d deactivate)
    (setf context (remove-layer (find-description d)
				context)))
  (dolist (d activate context)
    (setf context (adjoin-layer (find-description d)
				context))))
  



(defun display (display object &rest args &key deactivate activate &allow-other-keys)
  (funcall-with-layer-context 
   (modify-layer-context (current-layer-context) 
			 :activate activate 
			 :deactivate deactivate)
   (lambda () 
     (apply #'display-using-description (description-of object) display object args))))

(define-layered-method display-using-description 
  :around (description display object &rest args)
  (declare (ignorable args))
  (let ((*description* description)
	(*display* display)
	(*object*  object))
;    (<:as-html " " description "Layer Active?: "  (layer-active-p (defining-description 'maxclaims::link-to-viewer)))
    (dletf (((described-object description) object))
      (flet ((do-display ()
	       (contextl::funcall-with-special-initargs  
		(loop 
		   :for (key val) :on args :by #'cddr
		   :collect (list (find key (description-attributes description) 
					:key #'attribute-keyword)
				  :value val))
		(lambda ()
		  (contextl::funcall-with-special-initargs  
		   (let ((attribute (ignore-errors (find-attribute description 'active-attributes))))	
		     (when attribute
		       (loop for spec in (attribute-value attribute)
			  if (listp spec)
			  collect (cons (or 
					 (find-attribute description (car spec))
					 (error "No attribute matching ~A" (car spec)))
					(cdr spec)))))
		   (lambda () (call-next-method)))))))
	(funcall-with-layer-context
	 (modify-layer-context 
	  (if (standard-description-p description)
	      (adjoin-layer description (current-layer-context))
	      (current-layer-context))
	  :activate (description-active-descriptions description)
	  :deactivate (description-inactive-descriptions description))
	 (function do-display))))))





(defun display/d (&rest args)
  (apply #'display-using-description args))

(define-layered-method display-using-description (description display object &rest args)
 (error "No DISPLAY-USING-DESCRIPTION methods are specified for: ~%  DESCRIPTION: ~A ~%  DISPLAY: ~A ~%  OBJECT: ~A ~%  ARGS: ~S

OMGWTF! If you didn't do this, it's a bug!" description display object args))

(defmacro define-display (&body body)
  (loop with in-descriptionp = (eq (car body) :in-description)
	with description = (if in-descriptionp (cadr body) 't)
	for tail on (if in-descriptionp (cddr body) body)
	until (listp (car tail))
	collect (car tail) into qualifiers
	finally
	(when (member :in-description qualifiers)
	  (error "Incorrect occurrence of :in-description in defdisplay. Must occur before qualifiers."))
	(return
	  (destructuring-bind (description-spec &optional  (display-spec (gensym)) (object-spec (gensym))) 
	      (car tail) 
		`(define-layered-method
		  display-using-description
		   :in-layer ,(if (eq t description) 
				  t
				  (defining-description description))
		   ,@qualifiers
		   (,(if (listp description-spec)
		        (list (first description-spec)
		 	     (if (eq 'description (second description-spec))
				     'description
				     (defining-description (second description-spec)))))
		   ,display-spec
		   ,object-spec &rest args)
		   (declare (ignorable args))
                   ,@(cdr tail))))))



		  