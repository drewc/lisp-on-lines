(in-package :lisp-on-lines)

;;;;; Wrap a display in "back buttons"
(deflayer wrap-back-buttons)

(defvar *back-buttons-wrapped-p* nil)

(defdisplay
  :in-layer wrap-back-buttons :around
  (description object)
  (if  *back-buttons-wrapped-p*
       (call-next-method)
       (let ((*back-buttons-wrapped-p* t))
	
	 (<ucw:a :class "wiz-button previous" :action (ok self t)
		 (<:as-html "Go Back"))
	 (<:div :style "clear:both;"
		(call-next-method))
	 (<ucw:a :class "wiz-button previous" :action (ok self t)
		 (<:as-html "Go Back")))))

;;;; Wrap an object display in with a link to the object

(deflayer wrap-link)

(defvar *link-wrapped-p* nil)

(define-layered-class description
  :in-layer wrap-link ()
  ((link :initarg :link-action
	 :initarg :action
	 :initform nil :special t :accessor link-action)))

(defaction call-action-with--component-and-object ((self component) action-id object)
  (funcall (ucw::find-action (ucw::context.current-frame *context*) action-id)
	   self
	   object))

(defdisplay
    :in-layer wrap-link :around (description object)
    (let ((link (link-action description)))

      (with-inactive-layers (wrap-link)
	(if *link-wrapped-p*
	    (call-next-method)
	    (let ((*link-wrapped-p* t))
	      (<ucw:a :action (call-action-with--component-and-object
			       self
			       (ucw::make-new-action
				(ucw::context.current-frame *context*)
				(if (consp link)
				    (eval link)
				    link))
			       object)
		      (call-next-method)))))))

;;; wrap-a-form
(deflayer wrap-form)

(define-layered-class description
  :in-layer wrap-form ()
  ((form-buttons :initarg :form-buttons :initform nil :special t :accessor form-buttons)))

(defattribute form-button-attribute ()
  ((form-buttons :initarg :form-buttons :initform nil :special t :accessor form-buttons)))

(defdisplay ((description form-button-attribute) object)	    
  (macrolet ((submit (&key action value )
	       `(<ucw::simple-submit
		 :action (funcall ,action self object)
		 
		 (<:as-html ,value))))
    (loop for button in (form-buttons description)
	 do 
	 (let ((button button))
	   (with-properties (button)
	     (let ((action (.get :action)))
	       (submit :value (.get :value)
		       :action action)))))))



(defdisplay :in-layer wrap-form :around (description object)
  (<ucw:form
   :action (refresh-component self)
   (with-inactive-layers (wrap-form)
     (call-next-method)
     (with-inactive-layers (show-attribute-labels)
       (display-attribute
	(make-instance
	 'form-button-attribute
	 :form-buttons
	 (form-buttons description))
	object)))))

;;;; wrap a DIV


(deflayer wrap-div)

(define-layered-class description
  :in-layer wrap-div ()
  ((div-attributes :accessor div-attributes :initarg :div :special t :initform nil)))

(defdisplay :in-layer wrap-div :wrap-around (description object)
 (let ((args (div-attributes description)))
   (with-inactive-layers (wrap-div)
     (yaclml::funcall-with-tag
      (cons '<:div args)
      (lambda ()
	(call-next-method))))))

     