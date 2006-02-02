(in-package :lisp-on-lines)

;;;;; Wrap a display in "back buttons"
(deflayer wrap-back-buttons)

(defdisplay object (:in-layer
	     wrap-back-buttons
	     :combination :around)
  (<ucw:a :class "wiz-button previous" :action (ok component t)
	  (<:as-html "Go Back"))
  (<:div :style "clear:both;"
	 (call-next-method))
  (<ucw:a :class "wiz-button previous" :action (ok component t)
	  (<:as-html "Go Back")))

;;;; Wrap an object display in with a link to the object

(deflayer wrap-link)

(defdisplay object (:in-layer
	     wrap-link
	     :combination :around)
  (let ((layers  (find-display-layers object)))
    (<ucw:a :action (call-display self object
				  :type (find-display-type object)
				  :layers layers)
				 
	    (call-next-method))))