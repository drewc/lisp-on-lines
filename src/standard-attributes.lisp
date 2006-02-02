(in-package :lisp-on-lines)

(defattribute image ()
  ())

(defdisplay object (:description (buttons (eql 'image-editor-buttons)))
  (<ucw:a :action (ok component object)
	  (<:as-html "select this image")))

(defdisplay object (:description (image image))
  (<:img
   :class (or (getp :css-class) "lol-image") 
   :src (arnesi:strcat
	 (or (getp :prefix) "images/")
	 (escape-as-uri
	  (attribute-value object image)))))

(defdisplay object (:description (image image)
	     :in-layer editor)
  (<:div
   :class "lol-image-thumbnails"
   
  (dolist* (i (or (getp :directory)
		  (cl-fad:list-directory (strcat *default-pathname-defaults* "wwwroot/images/"))))
    (<:div
     :style "border: 1px solid black;width:100px;"
     (<:img
      :width "90px"
      :src (strcat (or (getp :prefix) "images/")
		   (file-namestring i)))
     (display-using-description 'image-editor-buttons component (file-namestring i) properties))
    (<:p :style "clear:both;"))))






