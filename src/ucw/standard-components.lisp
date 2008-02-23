(in-package :lisp-on-lines-ucw)

(defclass described-component-class (standard-component-class described-class)
  ())

(defmacro defaction (&rest args-and-body)
  `(arnesi:defmethod/cc ,@args-and-body))

(defun make-action (lambda &rest args)
  (let ((ucw::*default-action-class* 'basic-action))
    (apply #'ucw::make-action lambda args)))

(defclass standard-application (ucw:basic-application)
  ())

(defclass standard-request-context (ucw::standard-request-context)
  ())

(defmethod ucw:request-context-class list ((application standard-application))
  'standard-request-context)

(defvar +action-compound-name-delimiter+ #\|)

(defmethod ucw::find-action-id :around ((context standard-request-context))
  (or 
   (loop

      :for (k . v) in (ucw::parameters 
		      (context.request context))
      :do(destructuring-bind (param-name &optional action-id)
	      (split-sequence:split-sequence 
	       +action-compound-name-delimiter+ k)
	    (when (and action-id 
		       (string= 
			ucw::+action-parameter-name+ param-name))
	      (return action-id))))
   (call-next-method)))

(defcomponent standard-window-component 
  (ucw:basic-window-component)
  ((body
    :initform nil
    :accessor window-body
    :component t
    :initarg :body)))

(defmethod render-html-body ((window standard-window-component))
  (ucw:render (window-body window)))
