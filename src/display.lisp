(in-package :lisp-on-lines)

(defvar *object*)
(defvar *display*)

(define-layered-function display-using-description (description display object &rest args)
  (:documentation
   "Displays OBJECT via description using/in/with/on display"))

(defun display (display object &rest args)
  (display-using-description (description-of object) display object args))

(define-layered-method display-using-description 
  :around (description display object &rest args)
  (let ((*display* display)
	(*object*  object))
    (call-next-method)))

(define-layered-method display-using-description (description display object &rest args)
 (error "No DISPLAY-USING-DESCRIPTION methods are specified for: ~%  DESCRIPTION: ~A ~%  DISPLAY: ~A ~%  OBJECT: ~A ~%  ARGS: ~S

OMGWTF! If you didn't do this, it's a bug!" description display object args))

(defun display-attribute (attribute)
  (display-using-description attribute *display* *object*))

(defmacro define-display (&body body)
  (loop with in-layerp = (eq (car body) :in-layer)
	with layer = (if in-layerp (cadr body) 't)
	for tail on (if in-layerp (cddr body) body)
	until (listp (car tail))
	collect (car tail) into qualifiers
	finally
	(when (member :in-layer qualifiers)
	  (error "Incorrect occurrence of :in-layer in defdisplay. Must occur before qualifiers."))
	(return
	  (destructuring-bind (description-spec &optional  (display-spec (gensym)) (object-spec (gensym))) 
	      (car tail) 
		`(define-layered-method
		  display-using-description
		   :in-layer ,layer
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



		  