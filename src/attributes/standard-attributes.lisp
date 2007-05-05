(in-package :lisp-on-lines)


;TODO: get rid of this.
(defun attribute.name (attribute)
  (attribute-name attribute))


;;;; A few layers related to attributes
(deflayer omit-nil-attributes)

(defdisplay :in-layer omit-nil-attributes
	    :around ((attribute standard-attribute) object)
 (when (attribute-value object attribute)
   (call-next-method)))

;;;; Labels
(deflayer show-attribute-labels)

(defattribute attribute-label (attribute)
  ()
  (:default-properties
      :attribute nil))

(defdisplay
  ((label attribute-label) object)    
 (<:label
  :class "lol-label"
  (<:as-html (or (label (attribute label))
		 (attribute-name (attribute label)) " ")
	     "   ")))  

(defvar *attribute-label-attribute*
  (make-instance 'attribute-label))

(defdisplay
    :in-layer show-attribute-labels
    :around ((attribute standard-attribute) object)    
 (display-attribute *attribute-label-attribute* object :attribute attribute)
 (call-next-method))

(deflayer use-pretty-labels)

(define-layered-method label
   :in-layer use-pretty-labels
   :around (standard-attribute)
 (let ((label (call-next-method)))
   (when label   
     (string-capitalize
      (substitute #\Space #\- label)))))

(deflayer inspect-attributes)

(defdisplay :in-layer inspect-attributes
	    :around ((attribute standard-attribute) object)
 (call-next-method)
 (<ucw:a :action-body (ucw::call-inspector self attribute)
	  :title
	  (strcat "Inspect "
			 (attribute-name attribute) ":"
			 (description-type attribute) ":"
			 (type-of attribute))
	  (<:as-html "(i)")))

;;;; Functional attributes
(defattribute display-attribute ()
  ((display-arguments
    :accessor display-arguments
    :initarg :display
    :special t
    :initform nil))
  (:type-name display)
  (:documentation "Apply the display function to this object"))

(defdisplay ((attribute display-attribute) object)
  (apply #'display self (attribute-value object attribute)
	 (display-arguments attribute)))

(defattribute function-attribute ()
  ((function :accessor function-of
	     :initarg :function
	     :initform #'funcall
	     :special t))
  (:type-name function)
  (:documentation ""))

(defdisplay ((function function-attribute) object)
  (funcall (function-of function)
	   (attribute-value object function)))


;;;; Attribute Grouping
(defattribute attribute-group ()
  ()
  (:default-properties
   :group nil)
  (:type-name group))

(defdisplay ((group attribute-group) object)
  (apply #'display self object
	 :attributes (attributes group)
	 (group group)))


(defattribute select-attribute (display-attribute)
  ()
  (:default-properties
    :test 'meta-model::generic-equal
    :options-getter (constantly nil))
  (:type-name select))

(defdisplay ((attribute select-attribute) object)
 (<ucw:select
  :accessor (attribute-value object attribute)

  :test (test attribute)
  (dolist* (obj (funcall (options-getter attribute) object))
    (<ucw:option
     :value obj
     (apply #'display* obj (display-arguments attribute))))))

;;;; * Base Types

(defattribute base-attribute ()
  ()
  (:default-properties
      :default-value ""))

(defdisplay ((base base-attribute) object)
 (<:as-html (attribute-value object base)))

(defattribute base-attribute ()
  ()
  (:in-layer editor)
  (:default-properties 
    :callback nil
    :default-value nil
    :default-value-predicate #'null
    :dom-id (js:gen-js-name-string :prefix "_ucw_")
    :input-size nil))

(define-layered-function display-value (attribute value)
  (:method (attribute value)
    (if (funcall (default-value-predicate attribute) value)
	(default-value attribute)
	value)))

(defdisplay
  :in-layer editor ((field base-attribute) object)
  (LET ((value (attribute-value (object field) field)))
    (<:input
     :NAME
     (callback field)
     :VALUE (escape-as-html (strcat (display-value field value)))
     :TYPE
     "text"
     :ID
     (DOM-ID FIELD)
     :SIZE
     (INPUT-SIZE FIELD))))

(defdisplay
    :in-layer editor :around ((string base-attribute) object)
    (dletf (((callback string)
	     (or (callback string)
		 (ucw::register-callback
		  #'(lambda (val)
		      (setf (attribute-value object string) val)))))
	    ((object string) object))
      (call-next-method)))

;;;; Strings

(defattribute string-attribute (base-attribute)
  ()
  (:type-name string)
  (:default-properties
      :escape-html-p t
    :size nil
    :max-length nil
    :default-value ""))


#| 

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
#+nil (defattribute string-attribute (base-attribute)
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
  :id (dom-id string)
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
     (<:as-html "imagie"))) |#






