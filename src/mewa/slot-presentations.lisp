(in-package :it.bese.ucw)

(defun multiple-value-funcall->list (function &rest args)
  "The function to be called by m-v-bf"
		   (multiple-value-call #'list (apply function args)))

(defmacro multiple-value-bindf (vars form &body body)
  "Like M-V-B, only it works in actions. form must be a function call"
		   `(destructuring-bind ,vars 
		     (multiple-value-funcall->list #',(car form) ,@(cdr form))
		     ,@body))

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
When T, only the default value for primary keys and the joins are updated."))
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
       ()
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
  (concatenate 'string (call-next-method) "  (mm/dd/yyyy)"))

(defmethod present-slot ((slot clsql-wall-time-slot-presentation) instance)
  (let ((date (presentation-slot-value slot instance))
	(input-id (string (gensym))))
    (if (and date (not (editablep slot)))
	(<:span (<:as-html date)))
    (when (editablep slot)
      (<ucw:input :accessor (presentation-slot-value slot instance) :id input-id)
      (<:script :type "text/javascript" 
		(<:as-is (format nil " 
      Calendar.setup({
        inputField     :    \"~a\",
        ifFormat       :    \"%m/%d/%Y\",
      });" input-id))))))

(defslot-presentation  mewa-relation-slot-presentation (mewa-slot-presentation slot-presentation)
  ((foreign-instance :accessor foreign-instance)
   (linkedp :accessor linkedp :initarg :linkedp :initform t)
   (creator :accessor creator :initarg :creator :initform :editor))
  (:type-name relation))

(defaction search-records ((slot mewa-relation-slot-presentation) instance)
  (multiple-value-bindf (finstance foreign-slot-name)
      (meta-model:explode-foreign-key instance (slot-name slot))
    (let ((new-instance
            (call-component 
             (parent slot) 
             (make-instance (or (cadr (mewa:find-attribute finstance :presentation-search))
                                'mewa::mewa-presentation-search)
                            :search-presentation
                            (mewa:make-presentation finstance 
                                                    :type :search-presentation)
                            :list-presentation 
                            (mewa:make-presentation finstance 
                                                    :type :listing)))))
      (setf (slot-value instance (slot-name slot)) (slot-value new-instance foreign-slot-name))
      (meta-model:sync-instance instance :fill-gaps-only-p (fill-gaps-only-p self)))))

(defaction create-record-on-foreign-key ((slot mewa-relation-slot-presentation) instance)
  (multiple-value-bindf (finstance foreign-slot-name)
      (meta-model:explode-foreign-key instance (slot-name slot))
    (let ((new-instance
           (call-component
            (parent slot) 
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
  (call-component (parent self) (apply #'mewa:make-presentation instance initargs))
  ;; the viewed instance could have been changed/deleted, so we sync this instance
  (meta-model:sync-instance (instance (parent self))))



(defmethod present-slot :before ((slot foreign-key-slot-presentation) instance)
  ())


(defmethod  present-slot :around ((slot foreign-key-slot-presentation) instance)  
  (setf (foreign-instance slot) 
	(when (presentation-slot-value slot instance) 
	  (meta-model:explode-foreign-key instance (slot-name slot))))
  (flet ((render () (when (foreign-instance slot)(call-next-method))))
    (if (slot-boundp slot 'place)
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

;;;; HAS MANY 
(defslot-presentation has-many-slot-presentation (mewa-relation-slot-presentation)
  ((add-new-label :accessor add-new-label :initarg :add-new-label :initform "Add New"))
  (:type-name has-many))


(defaction add-to-has-many ((slot has-many-slot-presentation) instance)
  ;; if the instance is not stored we must make sure to mark it stored now!
  (unless (mewa::instance-is-stored-p instance)
    (setf (mewa::modifiedp (parent self)) t))
  ;; sync up the instance
  (mewa:ensure-instance-sync (parent slot))

  (multiple-value-bindf (class home foreign) 
      (meta-model:explode-has-many instance (slot-name slot))
    (let ((new (make-instance class)))
      (setf (slot-value new foreign) (slot-value instance home))
      (meta-model:sync-instance new :fill-gaps-only-p (fill-gaps-only-p self))
      (call-component (parent slot) (mewa:make-presentation new :type (creator slot)))
      (meta-model:sync-instance instance))))

(defmethod present-slot ((slot has-many-slot-presentation) instance)
  (when (slot-boundp slot 'place)
    (<ucw:submit :action (add-to-has-many slot instance) :value (add-new-label slot)))
  (let ((i (get-foreign-instances slot instance)))
	
    (<:ul 
     (dolist (s i)
       (let ((s s))
	 (setf (foreign-instance slot) s)
         (when (slot-boundp slot 'place)
           (<ucw:a :action (view-instance slot s :initargs `(:global-properties ,(list :linkedp t :editablep nil)))
		 (<:li   (setf (linkedp slot) nil)
			 (present-relation slot instance)))))))))


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
  (if (slot-boundp slot 'place)
      (progn
        (<ucw:a :action (list-prev slot) (<:as-html "<<"))
        (let ((self (parent slot)))
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
