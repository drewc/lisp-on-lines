(in-package :lisp-on-lines)


(defvar *display*)


(define-layered-function display-using-description (description display object &rest args)
  (:documentation
   "Displays OBJECT via description using/in/with/on display"))



(defun modify-layer-context (context &key activate deactivate)
  (dolist (d deactivate)
    (setf context (remove-layer (find-description d)
				context)))
  (dolist (d activate context)
    (setf context (adjoin-layer (find-description (if (consp d) (car d) d))
				context))))

(defun funcall-with-attribute-context (attribute thunk)
  (funcall-with-layer-context 
   (modify-layer-context (current-layer-context)
			 :activate (attribute-active-descriptions attribute)
			 :deactivate (attribute-inactive-descriptions attribute))
   (lambda ()
     (with-special-symbol-access
       (contextl::funcall-with-special-initargs
	(mappend (lambda (desc)
		   (when (consp desc)
		     (let ((description (find-description (car desc))))
		       (loop 
			  :for (key val) :on (cdr desc) :by #'cddr
			  :collect (list (find key (description-attributes description) 
					       :key #'attribute-keyword)
				  :value val)))))
		 (attribute-active-descriptions attribute))
	(lambda ()
	  (without-special-symbol-access
	    (funcall thunk))))))))

(defmacro with-attribute-context ((attribute) &body body)
  `(funcall-with-attribute-context ,attribute (lambda () ,@body)))
  
  
(defun display (display object &rest args &key deactivate activate &allow-other-keys)
  (funcall-with-layer-context 
   (modify-layer-context (current-layer-context) 
			 :activate activate 
			 :deactivate deactivate)
   (lambda () 
     (apply #'display-using-description (description-of object) display object args))))

(define-layered-method display-using-description 
  :around ((description standard-description-object) display object &rest args)
  (declare (ignorable args))
#+nil  (break "Entering DISPLAY for ~A on ~A using ~A" object display description)
  (let ((*display* display))
    (apply #'funcall-with-described-object 
     (lambda ()
       (call-next-method))
     object description args)))




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
				     (contextl::defining-layer (defining-description (second description-spec))))))
		   ,display-spec
		   ,object-spec &rest args)
		   (declare (ignorable args))
                   ,@(cdr tail))))))



		  