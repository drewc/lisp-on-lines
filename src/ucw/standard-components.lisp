(in-package :lisp-on-lines-ucw)

(defclass lisp-on-lines-action (ucw-standard::standard-action) 
  ((layer-context :accessor action-layer-context
		  :initform nil
		  :initarg :layer-context))
  (:metaclass closer-mop:funcallable-standard-class))


(setf ucw-core::*default-action-class* 'lisp-on-lines-action)


(defmethod ucw-core:call-action :around ((action lisp-on-lines-action) application session frame)
  (let ((next-method (lambda ()
		       (layered-call-action 
			action application session frame 
			(lambda () 
			  (call-next-method))))))
    (let ((layer-context (action-layer-context action)))
      (if layer-context 
	  (contextl:funcall-with-layer-context layer-context next-method)
	  (funcall next-method)))
    ))

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


(contextl:define-layered-function layered-call-action (action application session frame next-method)
  (:method (action application session frame next-method)
    (funcall next-method)))


(contextl:define-layered-method layered-call-action 
   :in-layer #.(lol::defining-description 'lol::validate)
   :around ((action lisp-on-lines-action) application session frame next-method)
   (call-next-method)

   )



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


