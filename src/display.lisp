(in-package :lisp-on-lines)

(defvar *description*)
(defvar *display*)
(defvar *object*)

(deflayer display-layer)

(define-layered-function display-using-description (description display object &rest args)
  (:documentation
   "Displays OBJECT via description using/in/with/on display"))

(defun display (display object &rest args)
  (display-using-description (description-of object) display object args))

(define-layered-method display-using-description 
  :around (description display object &rest args)
  (declare (ignorable args))
  (let ((*description* description)
	(*display* display)
	(*object*  object))
      
    (call-next-method)))



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



		  