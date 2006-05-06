(in-package :lisp-on-lines)

;;;; A few layers related to attributes
(deflayer omit-nil-attributes)

(defdisplay :in-layer omit-nil-attributes
	    :around ((attribute standard-attribute) object)
 (when (attribute-value object attribute)
   (call-next-method)))

(deflayer show-attribute-labels)

(defdisplay
    :in-layer show-attribute-labels
    :around ((attribute standard-attribute) object)
    
  (<:span
   :class "lol-label"
   (<:as-html (or (label attribute) (attribute.name attribute)) " "))
  (<:span
   :class "lol-attribute"
   (call-next-method)))

(deflayer use-pretty-labels)

(define-layered-method label
   :in-layer use-pretty-labels
   :around (standard-attribute)
 (let ((label (call-next-method)))
   (when label   
     (string-capitalize
      (substitute #\Space #\- label)))))

(defattribute display ()
  ()
  (:documentation "Apply the display function to this object"))

(defdisplay ((attribute display) object)
  (apply #'display self (attribute-value object attribute)
	 (description.properties attribute)))

;;;; * Base Types

(defattribute base-attribute ()
  ())

(defdisplay ((base base-attribute) object)
 (<:as-html (attribute-value object base)))

(defattribute base-attribute (ucw::string-field)
  ()
  (:in-layer editor)
  (:default-properties
      :callback nil))

(defmethod ucw:client-value ((self base-attribute))
  (attribute-value (object self) self))

(defmethod (setf ucw:client-value) (value (attribute base-attribute))
  (setf (attribute-value (object attribute) attribute) value))


(defmethod render ((field base-attribute))
  "this can only be used within a display-using-description call in the editor context, 
 it is a hack to integrate lol with ucw's new form stuff"
  (call-next-method))

  #+ (or)
(LET ((value (attribute-value (object field) field)))
  (<:as-html "asd" value)
  (<:input
   :NAME
   (callback field)
   :VALUE (escape-as-html value)
   :TYPE
   "text"
   :ID
   (DOM-ID FIELD)
   :SIZE
   (ucw::INPUT-SIZE FIELD)))



(defdisplay
    :in-layer editor  ((string base-attribute) object)
 (render string))


(defdisplay
    :in-layer editor :around ((string base-attribute) object)
    (dletf (((callback string) (ucw::make-new-callback
				#'(lambda (val)
				    (setf (attribute-value object string) val))))
	    ((object string) object))
      (call-next-method)))

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
(defdisplay :in-layer viewer
	    ((string string-attribute) object)
  (if (escape-html-p string)
      (<:as-html (attribute-value object string))
      (<:as-is (attribute-value object string))))


;;;; editor
(defattribute string-attribute (base-attribute)
  ()
  (:in-layer editor)
  (:default-properties
      :callback nil))

	    
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
   (<:input 
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
  ()
  (:default-properties
      :css-class "lol-image"
    :prefix "images/"))

(defdisplay ((buttons (eql 'image-editor-buttons)) object)
  (<ucw:a :action (ok component object)
	  (<:as-html "select this image")))

(defdisplay ((image image) object)
  (<:img
   :class (or (css-class image) "lol-image") 
   :src (arnesi:strcat
	 (or (prefix image) "images/")
	 (escape-as-uri
	  (attribute-value object image)))))

(defdisplay
    :in-layer editor ((image image)  object)

    (<:div
     :class "lol-image-thumbnails"
     (<:as-html "imagie")))






