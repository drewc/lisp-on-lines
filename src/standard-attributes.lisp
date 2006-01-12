(in-package :lisp-on-lines)

(defattribute image ()
  ())

(defdisplay (:description (image image))
  (<:img
   :class (or (getp :class) "lol-image") 
   :src (arnesi:strcat
	 (or (getp :prefix) "images/")
	 (escape-as-uri
	  (attribute-value object image)))))






