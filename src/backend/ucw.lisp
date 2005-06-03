(in-package :it.bese.ucw)

(defslot-presentation clsql-wall-time-slot-presentation ()
       ()
       (:type-name clsql-sys:wall-time))

(defmethod present-slot ((slot clsql-wall-time-slot-presentation) instance)
  (<:as-html (presentation-slot-value slot instance)))

(defslot-presentation  mewa-relation-slot-presentation ()
  ((slot-name :accessor slot-name :initarg :slot-name)
   (foreign-instance :accessor foreign-instance)
   (editablep :initarg :editablep :accessor editablep :initform :inherit))
  (:type-name relation))

(defmethod present-relation (( slot mewa-relation-slot-presentation) instance)
  (when (foreign-instance slot)
  (when (eql (editablep slot) :inherit)
    (setf (editablep slot) (editablep (parent slot))))
  (flet ((render-slot () 
		      (<ucw:render-component 
		       :component (mewa::make-presentation (foreign-instance slot) :type :one-line :initargs '(:global-properties (:editablep nil))))))
    (if (editablep slot)
	(render-slot)
      (<ucw:a :action (view-instance slot instance) 
	      (render-slot))))))

(defmethod present-slot ((slot mewa-relation-slot-presentation) instance)
  (setf (foreign-instance slot) (meta-model:explode-foreign-key instance (slot-name slot)))
  (present-relation slot instance))

(defslot-presentation foreign-key-slot-presentation (mewa-relation-slot-presentation)
  ()
  (:type-name foreign-key)
  (:default-initargs :editablep :inherit))

(defaction view-instance ((self component) instance &rest initargs)
  (call-component (parent self) (apply #'mewa:make-presentation (foreign-instance self) initargs)))

(defmethod  present-slot :before ((slot foreign-key-slot-presentation) instance)
  (setf (foreign-instance slot) (meta-model:explode-foreign-key instance (slot-name slot))))


(defslot-presentation has-many-slot-presentation (mewa-relation-slot-presentation)
  ()
  (:type-name has-many))

(defmethod present-slot ((slot has-many-slot-presentation) instance)
  (<:ul 
   (dolist (s (slot-value instance (slot-name slot)))
     (setf (foreign-instance slot) s)
     (<:li (present-relation slot instance)))))



(defslot-presentation has-a-slot-presentation (one-of-presentation)
  ((key :initarg :key :accessor key))
  (:type-name has-a))

(defmethod get-foreign-slot-value ((slot has-a-slot-presentation) (object t) (slot-name t))
  (slot-value object slot-name))

(defmethod present-slot ((slot has-a-slot-presentation) instance)
      (<:as-html (presentation-slot-value slot instance))
  (if (editablep slot)
      (<ucw:select :accessor (presentation-slot-value slot instance) :test #'equalp
        (when (allow-nil-p slot)
	  (<ucw:option :value nil (<:as-html (none-label slot))))
	(dolist (option (get-foreign-instances (presentation slot) instance))
	  (setf (instance (presentation slot)) option)
	  (<ucw:option :value (get-foreign-slot-value slot option (key slot)) (present (presentation slot)))))
      (if (presentation-slot-value slot instance)
	  (progn
	    (setf (instance (presentation slot)) (presentation-slot-value slot instance))
	    (present (presentation slot)))
	  (<:as-html "--"))))