(in-package :lisp-on-lines)

;;;;; Wrap a display in "back buttons"
(deflayer wrap-back-buttons)

(defdisplay
    :in-layer wrap-back-buttons :around
    (description object)
    
    (<ucw:a :class "wiz-button previous" :action (ok component t)
	    (<:as-html "Go Back"))
    (<:div :style "clear:both;"
	   (call-next-method))
    (<ucw:a :class "wiz-button previous" :action (ok component t)
	    (<:as-html "Go Back")))

;;;; Wrap an object display in with a link to the object

(deflayer wrap-link)

(defvar *link-wrapped-p* nil)

(define-layered-class description
  :in-layer wrap-link ()
  ((link :initarg :link :initform nil :special t :accessor link)))

(defdisplay
  :in-layer wrap-link :around (description object)
  (let ((link (link description)))

    (with-inactive-layers (wrap-link)
      (if *link-wrapped-p*
	  (call-next-method)
	  (let ((*link-wrapped-p* t))
	    (<ucw:a :action (call-display self object link)
		    (call-next-method)))))))



;;; wrap-a-form
(deflayer wrap-form)

(defdisplay ((description t) (button (eql 'standard-form-buttons)))
  (<ucw:submit :action (ok self)
	       :value "Ok."))

(defdisplay :in-layer wrap-form :around (object description)
  (<ucw:form
   :action (refresh-component self)
   (with-inactive-layers (wrap-form)

     (call-next-method)
     ;(display* 'standard-form-buttons)
   )))