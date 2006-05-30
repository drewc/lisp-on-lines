(in-package :lisp-on-lines)

;;;; The Standard Layers
(deflayer viewer)
(deflayer editor)

(define-layered-method label (anything)
  nil)

(defdisplay
    :in-layer editor :around (description object)
  "It is useful to remove the viewer layer when in the editing layer.
This allows us to dispatch to a subclasses editor."
  (with-inactive-layers (viewer)
    (call-next-method)))

;;;; These layers affect the layout of the object
(deflayer one-line)
(deflayer as-table)
(deflayer as-string)

(defdisplay
  :in-layer as-string (d o)
  (with-inactive-layers (editor viewer one-line as-table show-attribute-labels)
    (do-attributes (a d)
      (display-attribute a o)
      (<:as-is " "))))

(defmethod list-slots (thing)
  (list 'identity))

;;;; * Object displays.



;;;; TODO: all lisp types should have occurences and attributes defined for them.

(defdisplay ((description t) lisp-value)
  (<:as-html lisp-value))

(defdisplay (description (object string))
  (<:as-html object))

(defdisplay (description (object symbol))
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
  (when (label description)
    (<:span
     :class "title"
     (<:as-html (label description))))
  (do-attributes (attribute description)
    (<:div
     :class "attribute"
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

(deflayer list-display-layer)

(define-layered-class description
  :in-layer list-display-layer ()
  ((list-item :initarg :list-item :initform nil :special t :accessor list-item)))

(defdisplay (desc (list list))
 (with-active-layers (list-display-layer)
   (<:ul
    (dolist* (item list)
      (<:li  (apply #'display* item (list-item desc)))))))

;;;; Attributes 
(defdisplay
    :in-layer editor
    ((attribute standard-attribute) object)
    (call-next-method))

(define-layered-method display-using-description
  ((attribute standard-attribute) object component)
  (with-component (component)
    (<ucw:a :action (call 'info-message :message (strcat (symbol-package (description.type attribute))":/::" (description.type attribute)))
	    (<:as-html "*" )))
  (<:as-html (attribute-value object attribute)))







       
				     


