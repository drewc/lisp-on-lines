(in-package :lisp-on-lines)


;;;; The Standard Layer Hierarchy
(deflayer viewer)
(deflayer editor (viewer))
(deflayer creator (editor))

;;;; 'Mixin' Layers
(deflayer one-line)

(deflayer wrap-form)

(deflayer as-table)

(define-attributes (contextl-default)
  (:viewer viewer)
  (:editor editor)
  (:creator creator))


(defmacro with-component ((component) &body body)
  `(let ((self ,component))
    (declare (ignorable self))
    (flet ((display* (thing &rest args)
	     (apply #'display ,component thing args))
	   (display-using-description* (desc obj &optional props)
	     (display-using-description desc ,component obj props)))
      (declare (ignorable #'display* #'display-using-description*))
      ,@body)))


(define-layered-function find-display-type (object))

(define-layered-method find-display-type (object)
  'viewer)

(define-layered-function find-display-layers (object))

(define-layered-method find-display-layers (object)
  "layered function"
  nil)

(defmacro call-display (component object &rest args)
  `(call-component ,component (make-instance 'standard-display-component
			 :display #'(lambda (component)
				      (with-component (component)
					(display ,component ,object ,@args))))))



;;;; * Object displays.

;;;; We like to have a label for attributes, and meta-model provides a default.
(defdisplay label
    (:description (d (eql 'attribute-label)))
  (<:span
   :class "label"
   (<:as-html label)))


(define-layered-function display (component object &rest args)
  (:documentation
   "Displays OBJECT in COMPONENT. 

 default action is to FUNCALL-WITH-LAYERS the DISPLAY-USING-DESCRIPTION method."))

(define-layered-method display
    ((component t) (object standard-object) &rest args &key layers (type 'viewer)  &allow-other-keys)
  (let* ((occurence (find-occurence object))
	 (properties (attribute.properties
		 (find-attribute occurence (intern (format nil "~A" type) :KEYWORD))))
	 (layers (append (when type (loop for ty in (ensure-list type)
					  nconc `(+ ,ty)))
			 layers
			 (getf properties :layers))))
    (funcall-with-layers 
     layers		 
     #'display-using-description  occurence component object (plist-union args properties))))


(define-layered-method display
  ((component t) (object t) &rest args &key layers (type 'viewer) &allow-other-keys)
  (funcall-with-layers 
   layers		 
   #'display-using-description  t component object args))


(define-layered-function display-using-description (description component object properties)
  (:documentation
   "Render the object in component, using DESCRIPTION, which is an occurence, and attribute, or something else"))

(define-layered-method display-using-description (description component object properties)
  "The standard display simply prints the object"
  (declare (ignore component properties description))
  (<:as-html object))



;;;; ** The default display



;;;; ** One line
(defdisplay object (:in-layer one-line)
  "The one line presentation just displays the attributes with a #\Space between them"
  (do-attributes* (attribute)
	(display-current-attribute)
	(<:as-html " ")))
 
;;;; ** as-table

(defdisplay object (:in-layer as-table)
  (<:table
   (do-attributes* (a)
     (<:tr
      (<:td  (<:as-html (a-getp :label)))
      (<:td (display-current-attribute))))))

;;;; List Displays
(defdisplay (list list) ()
  (<:ul
   (dolist* (item list)
     (<:li  (apply #'display component item properties)))))

;;;; Attributes 
(defdisplay object (:in-layer
	     editor
 	     :description (attribute standard-attribute))
    "Legacy editor using UCW presentations"
    (warn "USING LEGACY EDITOR FOR ~A" (getf (find-properties attribute) :slot-name))
  (let ((p (lol:make-view object :type :editor)))
    (present-slot-view p (getf (find-properties attribute) :slot-name))))

(define-layered-method display-using-description
  ((attribute standard-attribute) component object properties)
  (<:as-html (attribute.type attribute) " ")
    
  (<:as-html (attribute-value object attribute)))

(defdisplay (button (eql 'standard-form-buttons))
    (:description (description t))
    (<ucw:submit :action (ok component)
		 :value "Ok."))

(defdisplay object (:in-layer wrap-form
		       :combination :around)
  (<ucw:form
   :action (refresh-component component)
   (call-next-method)
   (display component 'standard-form-buttons)))


(defcomponent standard-display-component ()
  ((display-function :accessor display-function :initarg :display)))

(defmethod render ((self standard-display-component))
  (funcall (display-function self) self))


       
				     


