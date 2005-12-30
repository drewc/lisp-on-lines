(in-package :lisp-on-lines)


;;;; The Standard Layer Hierarchy
(deflayer viewer)
(deflayer editor (viewer))
(deflayer creator (editor))

;;;; 'Mixin' Layers
(deflayer one-line)

(deflayer wrap-form)

(define-attributes (contextl-default)
  (:viewer viewer)
  (:editor editor)
  (:creator creator))


(defmacro with-component ((component) &body body)
  `(let ((self ,component))
    (flet ((display* (thing &rest args)
	     (apply #'display ,component thing args)))
      ,@body)))

(defmacro call-display (object &rest args)
  `(call-component self (make-instance 'standard-display-component
			 :display #'(lambda (component)
				      (with-component (component)
					(<:as-html ,object)
					(display ,object ,@args))))))

;;;;; Macros

(defmacro do-attributes ((var occurence attributes) &body body)
  (with-unique-names (att plist type)
    `(loop for ,att in ,attributes
      do (let* ((,att (ensure-list ,att))
		(,plist (rest ,att))
		(,type (getf ,plist :type))
		(,var (if ,type
			  (make-attribute :name (first ,att) :type ,type :plist ,plist)
			  (find-attribute ,occurence (first ,att)))))
	   (flet ((display-attribute* (component object)
		    (display-using-description
		     ,var
		     component
		     object
		     (rest ,att))))
	     (with-plist ((plist-union (rest ,att) (find-plist ,var)) ,var)	   
	       ,@body))))))


(defmethod find-plist (object)
  (list))

(defmethod find-plist ((attribute standard-attribute))
  (attribute.plist attribute))

(defmacro with-plist ((plist-form &optional prefix)  &body body)
  (with-unique-names (p)
    (let ((get (intern (string-upcase (if prefix (strcat prefix '-getp) "GETP"))))
	  (set (intern (string-upcase (if prefix (strcat prefix '-setp) "SETP")))))
      `(let ((,p ,plist-form))
	(flet ((,get  (p)
		 (getf ,p p))
	       (,set (p v)
		 (setf (getf ,p p) v)))
	  (declare (ignorable #',get #',set))
	  ,@body)))))


(defmacro defdisplay ((&key
		       (in-layer nil layer-supplied-p)
		       (combination nil combination-supplied-p)
		       (description '(occurence standard-occurence) description-supplied-p)
		       (component 'component)
		       ((:class object)  nil))
		      &body body)

  `(define-layered-method display-using-description
    ,@(when layer-supplied-p `(:in-layer ,in-layer))
    ,@(when combination-supplied-p `(,combination))
    (,description ,component
     ,(if object (if (listp object) object (list object object)) 'object)  properties)
    (declare (ignorable display-attribute))

    (with-plist ((plist-union properties (find-plist ,(car description))))
      
      ,(if (not description-supplied-p)
	   `(flet ((display-attribute (attribute)
		    (let ((a (ensure-list attribute)))
		      (display-using-description (find-attribute ,(car description) (car a)) ,component ,(car (ensure-list object))  (cdr a)))))
	     
	     ,@body)
	   `(progn ,@body)))))


(define-layered-function display (component object &rest args)
  (:documentation
   "Displays OBJECT in COMPONENT. 

 default action is to FUNCALL-WITH-LAYERS the DISPLAY-USING-DESCRIPTION method."))



(define-layered-method display
    ((component t) (object t) &rest args &key layers (type 'viewer)  &allow-other-keys)  
  (let* ((occurence (find-occurence object))
	 (plist (attribute.plist
		 (find-attribute occurence (intern (format nil "~A" type) :KEYWORD))))
	 (layers (append (when type (loop for ty in (ensure-list type)
					  nconc `(+ ,ty)))
			 layers
			 (getf plist :layers))))
    (funcall-with-layers 
     layers		 
     #'display-using-description  occurence component object (plist-union args plist))))

(define-layered-method display
    ((component t) (object symbol) &rest args &key (layers  '(+ viewer)) &allow-other-keys)
  (funcall-with-layers 
     layers		 
     #'display-using-description  t component object args))


(define-layered-method display ((component t) (list list) &rest args)
  "The Default Display* for LISTS"
  (<:ul
   (dolist* (item list)
     (<:li  (apply #'display component item args)))))


(define-layered-function display-using-description (description component object properties)
  (:documentation
   "Render the object in component, using DESCRIPTION, which is an occurence, and attribute, or something else"))

(define-layered-method display-using-description (description component object properties)
  "The standard display simply prints the object"
  (declare (ignore component properties description))
  (<:as-html object))

(define-layered-method display-using-description
    ((occurence standard-occurence) component object properties)

  (with-plist (properties o)
    (loop for att in (or (o-getp :attributes) (list-slots object))
	  do (let* ((att (ensure-list att))
		    (attribute (find-attribute occurence (first att))))
	       (warn "trying to render ~A in ~A" attribute object)
	       (with-plist ((plist-union (rest att) (find-plist attribute)))
		 (<:p :class "attribute"
		      (<:span :class "label" (<:as-html (getp :label) " "))	   
		      (display-using-description
		       attribute
		       component
		       object
		       (rest att))))))))

(define-layered-method display-using-description
  :in-layer one-line ((occurence standard-occurence) component object properties)
  (with-plist (properties occurence)
      (do-attributes (attribute occurence (or (occurence-getp :attributes)
					      (list-slots object)))
	(display-attribute* component object) (<:as-html " "))))


(define-layered-method display-using-description ((attribute standard-attribute) component object properties)
  (let ((p (lol:make-view object :type :viewer))
	(name (attribute.name attribute)))
    (when name (present-slot-view p name))))

(defdisplay (:in-layer
	     editor
 	     :description (attribute standard-attribute))
  "Legacy editor using UCW presentations"
  (let ((p (lol:make-view object :type :editor)))
    (present-slot-view p (getf (find-plist attribute) :slot-name))))



(defdisplay (:class
	     (button (eql 'standard-form-buttons))
	     :description (description t))
  (<ucw:submit :action (ok component)
	       :value "Ok.")


(defdisplay (:in-layer wrap-form
		       :combination :around)
  (<ucw:form
   :action (refresh-component component)
   (call-next-method)
   (display component 'standard-form-buttons))))

(defclass/meta test-class ()
  ((test-string :initform "test string" :type string))
  (:documentation "foo"))

(define-attributes (test-class)
  (test-string t :label "String :" :editablep t))
  
(defcomponent test-component ()
  ((display-types :accessor display-types :initform (list 'viewer 'editor 'creator 'one-line 'as-string))
   (current-type :accessor current-type :initform 'viewer)
   (instance :accessor instance :initform (make-instance 'test-class))))

(defmethod render ((self test-component))
  (let ((test (instance self))) 
    (<:h1 (<:as-html "Lisp on Lines Test Component"))
    (with-component (self)
      (<ucw:form
       :action (refresh-component self)
       (<ucw:select :accessor (current-type self)
		    (dolist* (type (display-types self))
		      (<ucw:option :value type (<:as-html type))))
       (<:input :type "Submit" :value "update")
       (<:fieldset
	(<:legend (<:as-html (current-type self)))
	(display test :type (current-type self)))))

    (<:div
     (<:h2
      (<:as-html "UCW Presentation based displays (the old school"))
     (dolist (type '(:viewer :editor :creator :one-line :as-string))
       (<:h3 (<:as-html type))
       (present-view (test type self))
       (<ucw:a :action (call-view (test type self))
	       (<:as-html "Call to " type))))))


(defcomponent standard-display-component ()
  ((display-function :accessor display-function :initarg :display)))

(defmethod render ((self standard-display-component))
  (funcall (display-function self) self))


       
				     


