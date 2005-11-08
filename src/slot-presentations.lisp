(in-package :lisp-on-lines)

(defun multiple-value-funcall->list (function &rest args)
  "The function to be called by m-v-bf"
		   (multiple-value-call #'list (apply function args)))

(defmacro multiple-value-bindf (vars form &body body)
  "Like M-V-B, only it works in actions. form must be a function call"
		   `(destructuring-bind ,vars 
		     (multiple-value-funcall->list #',(car form) ,@(cdr form))
		     ,@body))


;;;; ** Textarea Slot Presentation

(defslot-presentation text-slot-presentation ()
  ((rows :initarg :rows :accessor rows :initform 5)
   (columns :initarg :columns :accessor columns :initform 40)
   (escape-html-p :initarg :escape-html-p :accessor escape-html-p :initform nil)
   (convert-newlines-p :initarg :convert-newlines-p :accessor convert-newlines-p :initform nil))
  (:type-name text))

(defmethod present-slot ((slot text-slot-presentation) instance)
  (flet ((maybe-convert-newline-and-escape-html-then-print ()
	   (let ((string (if (convert-newlines-p slot)
			     (with-output-to-string (new-string)
			       (with-input-from-string
				   (s (presentation-slot-value slot instance))
				 (loop for line = (read-line s nil)
				       while line
				       do (format new-string "~A~A" line "<br/>"))))
			     (presentation-slot-value slot instance))))
	     (if (escape-html-p slot)
		 (<:as-html string)
		 (<:as-is string)))))
    
    (if (editablep slot)
	(<ucw:textarea
	 :accessor (presentation-slot-value slot instance)
	 :reader (or (presentation-slot-value slot instance)
		 "")
	 :rows (rows slot)
	 :cols (columns slot))
	(maybe-convert-newline-and-escape-html-then-print))))


(defcomponent mewa-slot-presentation ()
  ((slot-name :accessor slot-name 
	      :initarg :slot-name 
	      :documentation 
	      "The name of the slot being accessed")
   (fill-gaps-only-p :accessor fill-gaps-only-p 
		     :initarg :fill-gaps-only-p
		     :initform nil
		     :documentation 
		     "When nil, the instance is syncronised with the database. 
When T, only the default value for primary keys and the joins are updated.")
   (show-label-p :accessor show-label-p :initarg :show-label-p :initform t)
   (creatablep :accessor creatablep :initarg :creatablep :initform t))
  (:documentation "The superclass of all Mewa slot presentations"))

;;;; this has to be in the eval when i would think
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-slot-presentation-definition-for-type (type)
    (let* ((u-name (intern (format nil "~A-SLOT-PRESENTATION" type)))
	   (sp-name (intern (format nil "MEWA-~A" u-name)))
	   (t-name (intern (format nil "MEWA-~A" type))))
      `(defslot-presentation ,sp-name (,u-name mewa-slot-presentation)
	()
	(:type-name ,t-name)))))

(defmacro define-base-mewa-presentations (&body types)
  "Define the mewa-slot-presentations by subclassing the base UCW ones"
    `(progn ,@(mapcar #'generate-slot-presentation-definition-for-type
		      types)))

;;;then actually define the base presentations :
(define-base-mewa-presentations 
  boolean
  string
  number
  integer
  currency)

(defslot-presentation clsql-wall-time-slot-presentation (mewa-relation-slot-presentation)
       ((input-id :accessor input-id :initform (arnesi:random-string 10 arnesi:+ascii-alphabet+))
	(trigger-id :accessor trigger-id :initform (arnesi:random-string 10 arnesi:+ascii-alphabet+)))
       (:type-name clsql-sys:wall-time))

(defmethod presentation-slot-value ((slot clsql-wall-time-slot-presentation) instance)
  (let ((date (call-next-method)))
    (when date (multiple-value-bind (y m d) (clsql:time-ymd date)
      (format nil "~a/~a/~a" m d y)))))

(defmethod (setf presentation-slot-value) ((value string) (slot clsql-wall-time-slot-presentation) instance)
  (let ((new-time (clsql:parse-date-time (remove #\Space value)))
	(old-time (when (slot-boundp instance (slot-name slot))
		    (slot-value instance (slot-name slot)))))
    (unless (or (eql old-time new-time)
		(when (and new-time old-time)
		  (equal :equal (clsql:time-compare new-time old-time))))
    (setf (presentation-slot-value slot instance) new-time ))))

(defmethod label :around ((slot clsql-wall-time-slot-presentation))
  (concatenate 'string (call-next-method) "  (m/d/y)"))

(defmethod present-slot ((slot clsql-wall-time-slot-presentation) instance)
  (let ((date (presentation-slot-value slot instance)))
    (if (and date (not (editablep slot)))
	(<:as-html date))
    (when (editablep slot)
      (<ucw:input :accessor (presentation-slot-value slot instance) :id (input-id slot) :style "display:inline")
      (<:button :id (trigger-id slot) (<:as-html "[...]"))
      (<:script :type "text/javascript" 
		(<:as-is (format nil " 
      
Calendar.setup({
 inputField     :    \"~a\",
 button         :    \"~a\",
 ifFormat       :    \"%m/%d/%Y\" });" (input-id slot) (trigger-id slot)))))))

(defslot-presentation  mewa-relation-slot-presentation (mewa-slot-presentation slot-presentation)
  ((foreign-instance :accessor foreign-instance)
   (linkedp :accessor linkedp :initarg :linkedp :initform t)
   (creator :accessor creator :initarg :creator :initform :editor)
   (new-instance :accessor new-instance :initform nil))
  (:type-name relation))

(defaction search-records ((slot mewa-relation-slot-presentation) instance)
  (multiple-value-bindf (finstance foreign-slot-name)
      (meta-model:explode-foreign-key instance (slot-name slot))
    (let ((new-instance (new-instance self)))
      (unless new-instance
	(setf (new-instance self)
	      (call-component 
	       (ucw::parent slot)
	       (make-instance (or (cadr (mewa:find-attribute finstance :presentation-search))
				  'mewa::mewa-presentation-search)
			      :search-presentation
			      (mewa:make-presentation finstance 
						      :type :search-presentation)
			      :list-presentation 
			      (mewa:make-presentation finstance 
						      :type :listing)))))
      (sync-foreign-instance slot new-instance))))

(defmethod sync-foreign-instance ((slot mewa-relation-slot-presentation) foreign-instance)
  (let ((instance (instance (ucw::parent slot))))
    (multiple-value-bind (foo f-slot-name)
	(meta-model:explode-foreign-key instance (slot-name slot))
      (setf (slot-value instance (slot-name slot)) (slot-value foreign-instance f-slot-name))
      (meta-model:sync-instance instance :fill-gaps-only-p (fill-gaps-only-p slot)))))

    
(defaction create-record-on-foreign-key ((slot mewa-relation-slot-presentation) instance)
  (multiple-value-bindf (finstance foreign-slot-name)
      (meta-model:explode-foreign-key instance (slot-name slot))
    (let ((new-instance
           (call-component
            (ucw::parent slot)
            (mewa:make-presentation finstance :type (creator self)))))
      
      ;;;; TODO: this next bit is due to a bad design decision. 
      ;;;; Components should always have (ok) return self, but somewhere 
      ;;;; i've made in return (instance self) sometimes, and this
      ;;;; bahaviour is totatlly fucked.
      
     (when (typep new-instance 'mewa::mewa)
       (setf new-instance (instance new-instance)))

      ;;;; sorry about that, and now back t our regular program.
      
      (meta-model:sync-instance new-instance)
      (setf (slot-value instance (slot-name slot)) (slot-value new-instance foreign-slot-name))
      (meta-model:sync-instance instance :fill-gaps-only-p (fill-gaps-only-p self)))))
      

(defmethod present-relation ((slot mewa-relation-slot-presentation) instance)
 ;;;;(<:as-html (slot-name slot) "=> " (foreign-instance slot) " from " instance )
  (let* ((i (foreign-instance slot))
	 (pres (mewa::make-presentation 
		i
		:type :one-line 
		:initargs (list 
			   :global-properties 
			   (list :editablep nil :linkedp nil)))))
      (when (and (ucw::parent slot) (slot-boundp slot 'ucw::place))
	(setf (component.place pres) (component.place (ucw::parent slot))))
      (when i (<ucw:render-component :component pres))))

(defmethod present-slot ((slot mewa-relation-slot-presentation) instance)
  (present-relation slot instance))

(defslot-presentation foreign-key-slot-presentation (mewa-relation-slot-presentation)
  ()
  (:type-name foreign-key)
  (:default-initargs))

(defaction view-instance ((self component) instance &rest initargs)
  (call-component (ucw::parent self) (apply #'mewa:make-presentation instance initargs))
  ;; the viewed instance could have been changed/deleted, so we sync this instance
  (meta-model:sync-instance (instance (ucw::parent self))))


(defmethod  present-slot :around ((slot foreign-key-slot-presentation) instance)  
  (setf (foreign-instance slot) 
	(when (presentation-slot-value slot instance) 
	  (meta-model:explode-foreign-key instance (slot-name slot))))
  (flet ((render () (when (foreign-instance slot)(call-next-method))))
    (if (slot-boundp slot 'ucw::place)
        (cond 
          ((editablep slot)
	   (render)
           (<ucw:submit :action  (search-records slot instance) :value "Search" :style "display:inline")
           (<ucw:submit :action  (create-record-on-foreign-key slot instance) :value "Add New" :style "display:inline"))
          ((linkedp slot)
           (<ucw:a :action (view-instance slot (foreign-instance slot)) 
                   (render)))
          (t       
           (render)))
	;; presentation is used only for rendering
        (render))))


(defmethod find-foreign-instances ((slot foreign-key-slot-presentation))
  (clsql:select (class-name (class-of (meta-model:explode-foreign-key (instance slot) (slot-name slot))))))



;;;; HAS MANY 
(defslot-presentation has-many-slot-presentation (mewa-relation-slot-presentation)
  ((add-new-label :accessor add-new-label :initarg :add-new-label :initform "Add New"))
  (:type-name has-many))

(defaction add-to-has-many ((slot has-many-slot-presentation) instance)
  ;; if the instance is not stored we must make sure to mark it stored now!
  (unless (meta-model::persistentp instance)
    (setf (mewa::modifiedp (ucw::parent self)) t))
  ;; sync up the instance
  ;;(mewa:ensure-instance-sync (parent slot))
  (meta-model:sync-instance (instance (ucw::parent slot)))
  
  (multiple-value-bindf (class home foreign) 
      (meta-model:explode-has-many instance (slot-name slot))
    (let ((new (make-instance class)))
      (setf (slot-value new foreign) (slot-value instance home))
      (meta-model:sync-instance new :fill-gaps-only-p (fill-gaps-only-p self))
      (call-component (ucw::parent slot)  (mewa:make-presentation new :type (creator slot)))
      (meta-model:sync-instance instance))))

(defmethod present-slot ((slot has-many-slot-presentation) instance)
  (when (slot-boundp slot 'ucw::place)
    (<ucw:submit :action (add-to-has-many slot instance) :value (add-new-label slot)))
  (let* ((i (get-foreign-instances slot instance))
	 (presentation (and i (make-presentation (car  i) :type :one-line))))
    (when i
      (flet ((linker (i string)
	       (<ucw:a
		:action (view-instance slot i
				       :initargs
				       `(:global-properties ,
					 (list
					  :linkedp t
					  :editablep nil)))
		(<:as-html string))))
	(<:table :cellpadding 10
	 (<:tr
	  (<:th)			;empty col for (view) link
	  (dolist (s (slots presentation))
	    (<:th (<:as-html  (label s)))))
	 (dolist (s i)
	   (let ((s s))
	     (setf (foreign-instance slot) s)
	     (when (slot-boundp slot 'ucw::place)
	       (<:tr
		(<:td (linker s " (view) "))
		(dolist (p (slots (make-presentation s :type :one-line
						     :initargs
						     '(:global-properties
						       (:editablep nil)))))
		  (<:td 	     
				
		   (present-slot p s))))))))))))
		       

(defmethod get-foreign-instances ((slot has-many-slot-presentation) instance)
  (slot-value instance (slot-name slot)))

(defmethod presentation-slot-value ((slot has-many-slot-presentation) instance)
  (get-foreign-instances slot instance))

(defslot-presentation has-very-many-slot-presentation (has-many-slot-presentation)
  ((number-to-display :accessor number-to-display :initarg :number-to-display :initform 10)
   (current :accessor current :initform 0)
   (len :accessor len )
   (instances :accessor instances))
  (:type-name has-very-many))

(defmethod list-next ((slot has-very-many-slot-presentation))
  (setf (current slot) (incf (current slot) (number-to-display slot)))
  (when (< (len slot) (current slot))
    (setf (current slot) (- (number-to-display slot) (len slot)))))

(defmethod list-prev ((slot has-very-many-slot-presentation))
  (setf (current slot) (decf (current slot) (number-to-display slot)))
  (when  (> 0 (current slot))
    ;;what to do here is open to debate
    (setf (current slot) (- (len slot)(number-to-display slot)  ))))


(defmethod present-slot ((slot has-very-many-slot-presentation) instance)
  ;;(<:as-html "isance: " instance)
  (if (slot-boundp slot 'ucw::place)
      (progn
        (<ucw:a :action (list-prev slot) (<:as-html "<<"))
        (let ((self (ucw::parent slot)))
          (<ucw:a :action (call-component self (mewa:make-presentation (car (slot-value instance (slot-name slot))) :type :listing :initargs (list :instances (instances slot))))
	        (<:as-html  (label slot) (format nil " ~a-~a " (current slot) (+ (current slot) (number-to-display slot))))))
        (<ucw:a :action (list-next slot) (<:as-html ">>"))
        (call-next-method)
        (<:as-html "total :" (len slot)))
      (call-next-method)))

(defmethod get-foreign-instances :around ((slot has-very-many-slot-presentation) instance)
  (let ((f (call-next-method)))
    (setf (len slot) (length f))
    (setf (instances slot) f)
  (loop for cons on (nthcdr (current slot) f)
		   for i from 0 upto (number-to-display slot)
		   collect (car cons))))


;;;; * Has-a
(defslot-presentation has-a-slot-presentation (mewa-relation-slot-presentation)
  ((allow-nil-p :accessor allow-nil-p :initarg :allow-nil-p :initform t)
   (attributes :accessor attributes :initarg :attributes :initform nil))
  (:type-name has-a))

(defmethod find-foreign-slot-value ((slot has-a-slot-presentation) (object t))
  (multiple-value-bind (c s)      
      (meta-model:explode-foreign-key (instance (ucw::parent slot)) (slot-name slot))
    (slot-value object s)))

(defmethod get-foreign-instances ((slot mewa-relation-slot-presentation) instance)
  (clsql:select (class-name (class-of
			     (meta-model:explode-foreign-key instance (slot-name slot))))
		:flatp t))

(defmethod present-slot ((slot has-a-slot-presentation) instance)
;      (<:as-html (presentation-slot-value slot instance))
  (if (editablep slot)
      (progn (<ucw:select :accessor (presentation-slot-value slot instance) :test #'equalp
        (when (allow-nil-p slot)
	  (<ucw:option :value nil (<:as-html "none")))
	(dolist (option (get-foreign-instances slot instance))
	  (<ucw:option :value (find-foreign-slot-value slot option)
		       (lol:present
			(lol:make-presentation option
			   :type :as-string
			   :initargs
			   `(:attributes ,(attributes slot)))
			))))
	     (when (creatablep slot)
	       (<ucw:submit :action  (create-record-on-foreign-key slot instance) :value "Add New" :style "display:inline"))) 
      (if (presentation-slot-value slot instance)
	  (progn
	   (lol:present
	    (lol:make-presentation (meta-model:explode-foreign-key instance (slot-name slot))
			   :type :one-line
			   :initargs
			   `(:attributes ,(attributes slot)))
			   ))
	  (<:as-html "--"))))

(defslot-presentation many-to-many-slot-presentation (mewa-relation-slot-presentation)
  ((list-view :accessor list-view :initarg :list-view :initform :one-line)
   (action-view :accessor action-view :initarg :action-view :initform :viewer)
   (create-view :initform :creator)
   (select-view :initform :as-string :accessor select-view))
  (:type-name many-to-many)
  (:default-initargs :label "many to many"))

(defun %delete-item (item)
  (clsql:with-default-database (clsql:*default-database*)
    (ignore-errors
    (clsql:delete-instance-records item))))

(defaction delete-item ((self component) instance)
  (multiple-value-bind (res err) (%delete-item instance)
  (if (not err) 
      (call 'info-message :message "Removed Instance")
      (call 'info-message :message (format nil "Could not remove item. Try removing associated items first. ~A" instance)))))

(defaction delete-relationship ((slot many-to-many-slot-presentation) rel instance)
  (delete-item (ucw::parent self) rel)
  (sync-instance instance)
  (answer-component (ucw::parent self)   t))


(defun find-many-to-many-join-class (slot instance)
  (let* ((imd (getf (meta-model::find-slot-metadata instance (slot-name slot))
		    :db-info))
	 (jc (make-instance (getf imd :join-class)))
	 (jcmd (getf (meta-model::find-slot-metadata jc (getf imd :target-slot))
		     :db-info)))
    (getf jcmd :join-class)))

(defmethod find-all-instances ((slot many-to-many-slot-presentation) instance)
  (clsql:select (find-many-to-many-join-class slot instance) :flatp t))

(defmethod present-slot ((slot many-to-many-slot-presentation) instance)  
  (let ((instances (slot-value instance (slot-name slot)))
	new-instance)
    (<:ul
     (<:li (<ucw:button :action (add-to-many-to-many slot instance)
			 (<:as-html "Add New")))
     (<:li  (<ucw:button :action (add-to-many-to-many slot instance new-instance)
			 (<:as-html "Add:"))
	    (<ucw:select :accessor new-instance
			 (arnesi:dolist* (i (find-all-instances slot instance))
			   (<ucw:option
			    :value i
			    (lol:present-view (i (select-view slot) slot))))))
     (dolist* (i instances)
       (<:li
	(<ucw:a :action (call-view ((car i) (action-view slot) (ucw::parent slot)))
		(<:as-html "(view) "))
	(<ucw:a :action (delete-relationship slot (second i) instance)
		(<:as-html "(remove) "))
	(present-view ((car i) (list-view slot) (ucw::parent slot)))) ))))


(defaction add-to-many-to-many ((slot many-to-many-slot-presentation) instance &optional foreign-instance)
  ;;;; First things first, we sync.
  (sync-instance instance)
  (let* (new
	 (imd (getf (meta-model::find-slot-metadata instance (slot-name slot))
		    :db-info))
	 (jc (make-instance (getf imd :join-class)))
	 (jcmd (getf (meta-model::find-slot-metadata jc (getf imd :target-slot))
		     :db-info))
	 (fc (make-instance (getf jcmd :join-class)))
	 (c (if
	     foreign-instance
	     foreign-instance
	     (call-view (fc :creator (ucw::parent slot))))))
    (when c
      (sync-instance c)
;      (error "~A ~A ~A" (getf imd :foreign-key) (getf jcmd :foreign-key) (getf imd :home-key))
      (setf (slot-value jc (getf imd :foreign-key))
	    (slot-value instance (getf imd :home-key)))
      (setf (slot-value jc (getf jcmd :home-key))
	    (slot-value c (getf jcmd :foreign-key)))
      (sync-instance jc)
      
      (sync-instance instance)
      c)))
