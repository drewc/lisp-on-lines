(in-package :lisp-on-lines)

(defaction read-instance ((self component) instance)
  "View an existing instance"
  (call 'crud-viewer :instance instance))

(defaction update-instance ((self component) instance)
  "Edit an instance, possibly a newly created one"
  (call 'crud-editor :instance instance))

(defaction create-instance ((self component) class &rest initargs)
  "Create a new instance and edit it."
  (update-instance self (apply #'make-instance class initargs)))

(defun %delete-instance-and-return-nil (instance)
  "returns nil on success"
  (handler-case (clsql:delete-instance-records instance)
    (error (x)
      (return-from %delete-instance-and-return-nil x)))
  nil)

(defun display-as-string (instance)
  (with-output-to-string (s)
    (yaclml:with-yaclml-stream s
      (display (make-instance 'component) instance
	       :layers '(+ as-string)))))

(defaction delete-instance ((self component) instance)
  (when  (call 'option-dialog
	       :message (format nil "Really Delete ~A" (display-as-string instance))
	       :options '((t "Yes, really delete it,")
			  (nil "No, i'll hold on to this one.")))
    (let ((delete-failed (%delete-instance-and-return-nil instance)))
      (if (not delete-failed)
	  (answer t)
	  (progn
	    (call 'info-message :message delete-failed)
	    (answer t))))))


(defmethod breadcrumb-name (component)
  (string-downcase (string  (class-name (class-of component)))))

(defun render-breadcrumb (self)
  (<:p :class "breadcrumb"
       (let ((count 0)
	     (trail-length 3))
	 (labels ((find-call-stack-for-crumbs (component list-of-parents)
		    (cond ((and (not (null component))
				(> trail-length count))
			   (incf count)		      
			   (find-call-stack-for-crumbs
			    (when (slot-boundp component 'ucw::calling-component)
			      (slot-value component 'ucw::calling-component))
			    (cons component list-of-parents)))
			  (t
			   list-of-parents))))
	   (loop
	      :for c
	      :on (find-call-stack-for-crumbs self nil)
	      :do (let ((c c))
		    (<:as-html " / ")
		    (if (cdr c)
			(<ucw:a
			 :action (answer-component (second c) nil)
			 (<:as-html (breadcrumb-name (first c))))
			(<:as-html (breadcrumb-name (first c))))))))))

(defcomponent crud ()
 ((instance :accessor instance :initarg :instance :initform nil))
  (:documentation "The base class for all standard crud components"))

(defmethod render ((self crud))
  "Just to show off more of LOL, we'll use its display mechanism for UCW components.

DISPLAY takes two required arguments,   
COMPONENT : The component to display FROM (not neccesarily 'in')
OBJECT : The 'thing' we want to display... in this case it's the component,

DISPLAY also takes keyword arguments that modify the DESCRIPTION at run time.

By default, the display method iterates through the ATTRIBUTES 
of the DESCRIPTION of the OBJECT. This will hopfully become clear.

In this case, we are displaying the component from itself. 
"

  (display self self))

(defun class-name-of (instance)
  (class-name (class-of instance)))

;;;; We'll use this in a string attribute to display the title.
(defgeneric find-title (crud)
  (:method (crud)
    (if (instance crud)
	(format nil "An instance of ~A" (class-name-of (instance crud)))
	"Welcome to Crud 1.0")))

;;;; ** We define an attribute for the menu
;;;; DEFATTRIBUTE is like defclass for attributes.
(defattribute crud-menu ()
  ()
  (:default-properties
      :show-back-p t)
  (:documentation
   "A Custom menu attribute"))

(defdisplay :wrapping ((menu crud-menu) object (component component))
 "Set up the menu with an optional back button

In a DEFDISPLAY form, the variable SELF is bound to the component we are displaying.
This allows it to work with UCW's CALL and ANSWER, and saves some typing as well. 
One can also provide a name (or a specializer) for the component as the third parameter 
in the defdisplay argument list, (as i did above) but this is optional.

DEFDISPLAY is really just a few macros around DISPLAY-USING-DESCRIPTION, 
which does the real work. Macroexpand if you're interested."	    
  (<:ul
   (when (show-back-p menu)
     (<:li (<ucw:a :action (answer nil)
		   (<:as-html "Go Back"))))
   (call-next-method)))

(defdisplay ((menu crud-menu) object)
  "Do nothing beyond the defalt for our standard menu

note the omitted COMPONENT argument. sugar is all."
  t)

;;;; create a new layer for some customisations.
(deflayer crud)

;;;; we don't really _have_ to do this in our own layer,
;;;; but it does give us the ability to turn the behaviour off.
(defdisplay :in-layer crud
	    :wrap-around ((attribute standard-attribute) (object crud))
  "Around every attribute of a CRUD instance, i'd like to wrap a div."
  (<:div
   :class (format nil "crud-~A" (string-downcase
				 (string (attribute-name attribute))))
   (call-next-method)))

;;;; A description contains attributes.
;;;; ATTRIBUTES are the various pieces that come together to make a display
;;;; In this case, we define parts of the 'page'.

(defdescription crud ()
  (;; use a generic function for the title attribute
   (title
    ;; attributes have types.
    ;; inspect LOL::FIND-ATTRIBUTE-CLASS-FOR-TYPE for a list. 
    :type 'string
    ;; almost all attributes have a getter and/or setter function
    ;; which is passed the object being displayed.
    ;; You can also use :SLOT-NAME 
    ;; see ATTRIBUTE-VALUE for details.
    :getter #'find-title)

   ;; our breadcrumb function renders itself,
   ;; and does not return a value. 
   (breadcrumb
    ;; the FUNCTION type calls a function
    ;; again, passing the object.
    :type 'function
    :function #'render-breadcrumb
    ;; We need to specify IDENTITY here,
    ;; as the default :GETTER calls
    ;; SLOT-VALUE on the name of the attribute.
    :getter #'identity)
   ;; So we don't need a getter in INSTANCE.
   (instance
   ;; the DISPLAY type calls DISPLAY
   ;; passing the component and the object
   ;; along with any arguments specified using the
   ;; :DISPLAY property 
    :type 'display
    :display '(:layers (+ show-attribute-labels)))
   ;; this is our menu, a custom attribute 
   (menu
    :type 'crud-menu))
  (;; now we create a LINE in the default layer.
   ;; LINES describe how an object is displayed
   ;; when that layer is active.
   :in-layer
   t
   :attributes '(breadcrumb title menu instance)
   :layers '(- show-attribute-labels + crud)))


;;;; That's the basic outline of our app, now we fill in the blanks.

;;;; ** Viewer
(defcomponent crud-viewer (crud)
  ()
  (:documentation "A component for viewing objects"))

(defdisplay ((menu crud-menu) (crud crud-viewer))
  "Allow the user to edit and delete the object"	    
  (<:li (<ucw:a :action (delete-instance crud (instance crud))
		(<:as-html "DELETE this object.")))
  (<:li (<ucw:a :action (update-instance crud (instance crud))
		(<:as-html "EDIT this object."))))

;;;; ** Editor
;;;; (use the same component for creating and editing,
;;;; with a little magic to make it all work.
(defcomponent crud-editor (crud validation-mixin)
  ())

(defaction ensure-instance ((self crud-editor))
  "This one does a little magic, see SYNC-INSTANCE"
  (meta-model::sync-instance (instance self)))

(defmethod find-title ((crud crud-editor))
  (<:as-html "Editing a "
	     (class-name (class-of (instance crud)))
	     " ")
    (unless (meta-model:persistentp (instance crud))
    (<:as-html "(new)")))

(defattribute crud-editor-attribute (display-attribute)
  ()
  (:type-name crud-editor))

(defdisplay :around ((ed crud-editor-attribute) object)
 (with-active-layers (editor show-attribute-labels wrap-form)
   (call-next-method)))


(defdescription crud-editor ()
  ((instance :type 'crud-editor))
  (:in-layer
   t
   :default-attributes
   `((instance
      :display
      (:form-buttons
      ((:value ,(if (meta-model:persistentp (instance self))
		     "Save"
		     "Create")
	 :action ,(action (self object)
		    (ensure-instance self)
		    (answer (instance self))))
	 (:value 
	  "Cancel"
	  :action 
	  ,(action (self object)
	    (setf (instance self) nil)
	    (answer nil)))))))))

;;;; ** Summary
(defcomponent crud-summary (crud)
  ((class :accessor db-class :initarg :class)
   (limit :accessor row-limit :initform 25)
   (offset :accessor row-offset :initform 0)))

(defmethod find-title ((crud crud-summary))
  (format nil "Viewing Summary of ~A" (db-class crud)))

(defun find-some (class limit offset)
  (clsql:select class :limit limit  :offset offset :flatp t))

(defmethod find-summary ((crud crud-summary))
  (find-some (db-class crud)
	     (row-limit crud)
	     (row-offset crud)))

(defdescription crud-summary ()
  ()
  (:in-layer t
   ;;; here we show :default-attributes
   ;;; the attributes themselves can vary by layer
   ;;; the same syntax is supported in an :ATTRIBUTES form
   ;;; but that also specifies which attributes to display
   :default-attributes
   `((instance
      :getter ,#'find-summary
      :display
      (:layers (+ one-line)
       :list-item
	(:layers (+ lol::wrap-link + lol::show-attribute-labels)
	 :link-action ,(action (self obj)
			(call 'crud-viewer :instance obj))))))))

(defdisplay ((menu crud-menu) (object crud-summary))
   (<:li (<ucw:a
	  :action (create-instance object (db-class object))
	  (<:as-html "(Create New " (db-class object) ")"))))

(defaction call-crud-summary ((self component) class)
  (call 'crud-summary :class class))


(defcomponent crud-database (crud)
  ())

(defdescription crud-database ()
  ((instructions
    :type 'string
    :getter (constantly "View Object Summary: "))
   (instance
    :type 'display
    :getter #'(lambda (obj)
		(declare (ignore obj))
		(meta-model::list-base-classes :clsql))
    :display `(:layers (+ one-line)
		       :list-item
		       (:layers (+ lol::wrap-link )
			:link-action ,(action (self class)
		                       (call-crud-summary self class))))))
  (:in-layer
   t
   :attributes '(title menu instructions instance)))