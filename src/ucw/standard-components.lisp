(in-package :lisp-on-lines-ucw)

(defclass lisp-on-lines-application (contextl-application)
  ()
  (:default-initargs :action-class 'lisp-on-lines-action))

(defclass lisp-on-lines-action (action-with-isolation-support contextl-action ) 
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defclass lisp-on-lines-component (contextl-component) 
  () 
  (:metaclass standard-component-class))

(defclass lisp-on-lines-component-class (standard-component-class) 
  ())


(defmethod initialize-instance :around ((class lisp-on-lines-component-class) 
					&rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
	 thereis (ignore-errors (subtypep direct-superclass 'lisp-on-lines-component)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'lisp-on-lines-component)))
	     initargs)))


(defmethod reinitialize-instance :around ((class lisp-on-lines-component-class) 
					  &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
	  (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass 'lisp-on-lines-component))))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'lisp-on-lines-component)))
	     initargs)))

(defclass described-component-class (described-class standard-component-class )
  ())




(defmethod ucw-core:handle-action :wrap-around ((action lisp-on-lines-action) application session frame)
     (let ((lol::*invalid-objects* (make-hash-table)))
       (handler-bind ((lol::validation-condition 
		       (lambda (c)
			 (let ((object (lol::validation-condition-object c))
			       (attribute (lol::validation-condition-attribute c)))


			   (setf (gethash object lol::*invalid-objects*)
				 (cons (cons attribute c)
				       (gethash object lol::*invalid-objects*)))))))
       (call-next-method))))








(defclass described-component-class (described-class standard-component-class )
  ())



;; (defcomponent standard-window-component 
;;   (ucw-standard::basic-window-component)
;;   ((body
;;     :initform nil
;;     :accessor window-body
;;     :component t
;;     :initarg :body)))

;; (defmethod render-html-head ((window standard-window-component))
;;   (let* ((app (context.application *context*))
;; 	 (url-prefix (application.url-prefix app)))
;;     (<:meta :http-equiv "Content-Type" :content (window-component.content-type window))
;;     (awhen (window-component.title window)
;;       (<:title (if (functionp it)
;; 		   (funcall it window)
;; 		   (<:as-html it))))
;;     (awhen (window-component.icon window)
;;       (<:link :rel "icon"
;; 	      :type "image/x-icon"
;; 	      :href (concatenate 'string url-prefix it)))
;;     (dolist (stylesheet (effective-window-stylesheets window))
;;       (<:link :rel "stylesheet"
;; 	      :href stylesheet
;; 	      :type "text/css"))))

;; (defmethod render-html-body ((window standard-window-component))
;;   (render (window-body window)))

;; (defcomponent info-message ()
;;   ((message :accessor message :initarg :message)))

;; (defmethod render ((m info-message))
;;   (<:div
;;    :class "info-mssage" 
;;    (<:as-html (message m)))
;;    (<ucw:a :action (answer-component m nil) "Ok"))


