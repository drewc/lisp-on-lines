(in-package :lisp-on-lines)

(deflayer omit-nil-attributes)

(defdisplay :in-layer omit-nil-attributes
	    :around ((attribute standard-attribute) object)
 (when (attribute-value object attribute)
   (call-next-method)))

(deflayer label-attributes)

(defdisplay :in-layer label-attributes
	    :around ((attribute standard-attribute) object)

    (<:span
     :class "lol-label"
     (<:as-html (or (label attribute) (attribute.name attribute))))
    (<:span
     :class "lol-attribute"
     (call-next-method)))

;;;; * Base Types

(defattribute base-attribute ()
  ())

(defdisplay ((base base-attribute) object)
 (<:as-html (attribute-value object base)))

;;;; Strings

(defattribute string-attribute (base-attribute)
  ()

  (:type-name string)
  (:default-properties
      :escape-html-p t
    :size nil
    :max-length nil))

(defdisplay :in-layer omit-nil-attributes
	    :around ((attribute string-attribute) object)
 (when (< 0 (length  (attribute-value object attribute)))
   (call-next-method)))


;;;; default
(defdisplay ((string string-attribute) object)
  (if (escape-html-p string)
      (<:as-html (attribute-value object string))
      (<:as-is (attribute-value object string))))


;;;; editor
(defattribute string-attribute (base-attribute)
  ()
  (:in-layer editor)
  (:default-properties
      :callback nil))

(defdisplay
   :in-layer editor :around ((string string-attribute) object)
 (dletf (((callback string) (ucw::make-new-callback
			     #'(lambda (val)
				 (setf (attribute-value object string) val)))))
   (call-next-method)))

(defdisplay :in-layer editor ((string string-attribute) object)
 (<:input
  :type "text"
  :id (id string)
  :name (callback string)
  :value (or (attribute-value object string) "")))	    

(defattribute string-search-attribute (string-attribute)
  ()
  (:default-properties
      ;; the func that find search results

      :search-action #'(lambda ()
			 (with-call/cc 
			   nil))
    ;; when chosing from a list of results, this function selects one.
    :select-function (constantly t))
  (:type-name string-search))

(defdisplay
   :in-layer editor :after ((search string-search-attribute) object)
     (IT.BESE.YACLML.TAGS:INPUT
      :TYPE "submit"
      :VALUE "search"
      :ONCLICK
      (JS:JS-INLINE*
       `(PROGN
	 (IT.BESE.UCW::SET-ACTION-PARAMETER
	  ,(IT.BESE.UCW::MAKE-NEW-ACTION
	    (IT.BESE.UCW::CONTEXT.CURRENT-FRAME *CONTEXT*)
	    (search-action search)))
	 (RETURN T)))))

;;;; textarea

(defattribute text-attribute (string-attribute)
  ()
  (:type-name text))

(defdisplay :in-layer editor ((string text-attribute) object)
 (<:textarea
  :id (id string)
  :name (callback string)
 (or (attribute-value object string) "")))



;;;; WALL-TIME

(defattribute wall-time-attribute (string-attribute)
  ()
  (:type-name clsql-sys:wall-time))

(define-layered-method attribute-value (object (attribute wall-time-attribute))
 (let ((date (call-next-method)))
   (when date (multiple-value-bind (y m d) (clsql:time-ymd date)
		(format nil "~a/~a/~a" m d y)))))
		       
(defdisplay
  ((time wall-time-attribute) object)
  (<:as-html (attribute-value object time)))



(defattribute image ()
  ())

(defdisplay ((buttons (eql 'image-editor-buttons)) object)
  (<ucw:a :action (ok component object)
	  (<:as-html "select this image")))

(defdisplay ((image image) object)
  (<:img
   :class (or (.get :css-class) "lol-image") 
   :src (arnesi:strcat
	 (or (.get :prefix) "images/")
	 (escape-as-uri
	  (attribute-value object image)))))

(defdisplay
    :in-layer editor ((image image)  object)

    (<:div
     :class "lol-image-thumbnails"
   
     (dolist* (i (or (.get :directory)
		     (cl-fad:list-directory (strcat *default-pathname-defaults* "wwwroot/images/"))))
       (<:div
	:style "border: 1px solid black;width:100px;"
	(<:img
	 :width "90px"
	 :src (strcat (or (.get :prefix) "images/")
		      (file-namestring i)))
	(display-using-description* 'image-editor-buttons (file-namestring i) (.properties)))
       (<:p :style "clear:both;"))))






