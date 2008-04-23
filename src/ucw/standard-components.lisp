(in-package :lisp-on-lines-ucw)

(defparameter *source-component* nil)

(defclass standard-basic-action (basic-action)
  ((source-component :accessor action-source-component))
  (:metaclass mopp:funcallable-standard-class))

(defmethod shared-initialize :before ((action standard-basic-action) slots &rest args)
  (declare (ignore slots args))  
  (setf (action-source-component action) *source-component*))

(defmethod handle-action :around ((action standard-basic-action) a s f)
  (let ((*source-component* (action-source-component action)))
    (call-next-method)))

(defmethod render :around (component)
  (let ((*source-component* component))
    (call-next-method)))


(defun/cc call (name &rest args)
  (call-component *source-component* 
		  (apply #'make-instance name args)))

(defun/cc answer (&optional val)
  (answer-component *source-component* 
	  val))

(defclass described-component-class (standard-component-class described-class)
  ())

(defmacro defaction (&rest args-and-body)
  `(arnesi:defmethod/cc ,@args-and-body))

(defparameter *default-action-class* 'standard-basic-action)

(defun make-action (lambda &rest initargs &key (class *default-action-class*) &allow-other-keys)
  "Makes a new unregistered action."
  (remf-keywords initargs :class)
  (apply #'make-instance class :lambda lambda initargs))

  
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

(defmethod render-html-head ((window standard-window-component))
  (let* ((app (context.application *context*))
	 (url-prefix (application.url-prefix app)))
    (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
    (awhen (window-component.title window)
      (<:title (if (functionp it)
		   (funcall it window)
		   (<:as-html it))))
    (awhen (window-component.icon window)
      (<:link :rel "icon"
	      :type "image/x-icon"
	      :href (concatenate 'string url-prefix it)))
    (dolist (stylesheet (effective-window-stylesheets window))
      (<:link :rel "stylesheet"
	      :href stylesheet
	      :type "text/css"))))

(defmethod render-html-body ((window standard-window-component))
  (ucw:render (window-body window)))

(defcomponent info-message ()
  ((message :accessor message :initarg :message)))

(defmethod render ((m info-message))
  (<:div
   :class "info-mssage" 
   (<:as-html (message m)))
   (<lol:a :action (answer-component m nil) "Ok"))


