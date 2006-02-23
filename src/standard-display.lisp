(in-package :lisp-on-lines)

;;;; The Standard Layers
(deflayer viewer)
(deflayer editor)
(deflayer creator)
(deflayer one-line)
(deflayer as-table)
(deflayer as-string)

(defdisplay
  :in-layer as-string (d o)
  (do-attributes (a d)
    (display-attribute a o)
    (<:as-is " ")))

(defmethod list-slots (thing)
  (list 'identity))


;;;; TODO : this doesn't work

(defaction call-display-with-context ((from component) object context &rest properties)
  (call-component self (make-instance 'standard-display-component
				      :context context
				      :object object
				      :args (if (cdr properties)
						 properties
						 (car properties)))))

(defmacro call-display (component object &rest properties)
  `(let ()
    (call-display-with-context ,component ,object nil  ,@properties)))

(defcomponent standard-display-component ()
  ((context :accessor context :initarg :context)
   (object :accessor object :initarg :object)
   (args :accessor args :initarg :args)))

(defmethod render ((self standard-display-component))
  
  (apply #'display self (object self) (args self)))


;;;; * Object displays.

;;;; We like to have a label for attributes, and meta-model provides a default.
(defdisplay ((desc (eql 'label)) label)
  (<:span
   :class "label"
   (<:as-html label)))

;;;; TODO: all lisp types should have occurences and attributes defined for them.

(defdisplay ((description t) lisp-value)
  (<:as-html lisp-value))

(defdisplay (description (object string))
  (<:as-html object))

(defdisplay (description object (component t))
  "The default display for CLOS objects"
  (print (class-name (class-of object)))
  (dolist* (slot-name (list-slots object))
  
    (let ((boundp (slot-boundp object slot-name)))
      (format t "~A~A : ~A" (strcat slot-name)
	      (if boundp
		  ""
		  "(unbound)")
	      (if boundp
		  (slot-value object slot-name) "")))))

(defdisplay ((description t) object)
  "The default display for CLOS objects in UCW components"
   (dolist* (slot-name (list-slots object))

      (let ((boundp (slot-boundp object slot-name)))
	(<:label :class "lol-label"
		 (display-attribute 'label  (strcat slot-name))
	(if boundp
	    ""
	    "(unbound)"))
      (<:as-html
       (if boundp
	   (slot-value object slot-name) "")))))

;;;; ** The default displays for objects with a MEWA occurence

(defdisplay (description object)
 (<:div
  :class "lol-display"	    
  (do-attributes (attribute description)
    (<:div
     :class "lol-attribute-row"
     (display-attribute attribute object)))))

;;;; ** One line
(defdisplay
    :in-layer one-line (description object) 
    "The one line presentation just displays the attributes with a #\Space between them"
    (do-attributes (attribute description)
      (display-attribute attribute object)
      (<:as-html " ")))
 
;;;; ** as-table

(defdisplay :in-layer as-table (description object) 
  (<:table 
   (do-attributes (a description)
     (<:tr
      (<:td  :class "lol-label" (<:as-html (label a)))
      (<:td (display-attribute a object))))))

;;;; List Displays
(defdisplay (desc (list list))
  (<:ul
   (dolist* (item list)
     (<:li  (display* item)
	    (<:as-html item)))))

;;;; Attributes 
(defdisplay
    :in-layer editor
    ((attribute standard-attribute) object)
    "Legacy editor using UCW presentations"
    
    (warn "USING LEGACY EDITOR FOR ~A" (slot-name attribute)))

(define-layered-method display-using-description
  ((attribute standard-attribute) object component)
  (with-component (component)
    (<ucw:a :action (call 'info-message :message (strcat (symbol-package (description.type attribute))":/::" (description.type attribute)))
	    (<:as-html "*" )))
  (<:as-html (attribute-value object attribute)))







       
				     


