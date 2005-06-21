(in-package :it.bese.ucw)

(defun multiple-value-funcall->list (function &rest args)
		   (multiple-value-call #'list (apply function args)))

(defmacro multiple-value-bindf (vars form &body body)
		   `(destructuring-bind ,vars 
		     (multiple-value-funcall->list #',(car form) ,@(cdr form))
		     ,@body))

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
		(and (null old-time) new-time)
		 (equal :equal (clsql:time-compare new-time old-time)))
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

(defslot-presentation  mewa-relation-slot-presentation ()
  ((slot-name :accessor slot-name :initarg :slot-name)
   (foreign-instance :accessor foreign-instance)
   (linkedp :accessor linkedp :initarg :linkedp :initform t))
  (:type-name relation))

(defun get-fkey-data (instance slot-name)
  "ugly workaround b/c UCW does not like M-V-B"
  (multiple-value-bind (finstance foreign-slot-name)
      (meta-model:explode-foreign-key instance slot-name)
    (cons finstance foreign-slot-name)))

(defaction search-records ((slot mewa-relation-slot-presentation) instance)
  (let* ((d (get-fkey-data instance (slot-name slot)))
	 (finstance (car d))
	 (foreign-slot-name (cdr d))
	 (new-instance
    (call-component 
     (parent slot) 
     (make-instance 'mewa::mewa-presentation-search
		    :search-presentation
		    (mewa:make-presentation finstance 
					    :type :search-presentation)
		    :list-presentation 
		    (mewa:make-presentation finstance 
					    :type :listing)))))
    (setf (slot-value instance (slot-name slot)) (slot-value new-instance foreign-slot-name))
    (meta-model:sync-instance instance)
    (clsql:update-objects-joins (list instance))))
    
(defmethod present-relation ((slot mewa-relation-slot-presentation) instance)
 ;;;;(<:as-html (slot-name slot) "=> " (foreign-instance slot) " from " instance )
  (let* ((i (foreign-instance slot))
	 (pres (mewa::make-presentation 
		i
		:type :one-line 
		:initargs (list 
			   :global-properties 
			   (list :editablep nil :linkedp nil)))))
      (when (ucw::parent slot) 
	(setf (component.place pres) (component.place (ucw::parent slot))))
      (when i (<ucw:render-component :component pres))))



(defmethod present-slot ((slot mewa-relation-slot-presentation) instance)
  (present-relation slot instance))

(defslot-presentation foreign-key-slot-presentation (mewa-relation-slot-presentation)
  ()
  (:type-name foreign-key)
  (:default-initargs))

(defaction view-instance ((self component) instance &rest initargs)
  (call-component (parent self) (apply #'mewa:make-presentation instance initargs)))


(defmethod  present-slot :around ((slot foreign-key-slot-presentation) instance)
  (setf (foreign-instance slot) (when (presentation-slot-value slot instance) (meta-model:explode-foreign-key instance (slot-name slot))))
  (flet ((render () (call-next-method)))
    (cond 
      ((editablep slot)
       (render)
       (<ucw:a :action (search-records slot instance) (<:as-html " (search)"))
       (<ucw:a :action (create-record slot instance) (<:as-html " (new)")))
      ((linkedp slot)
       (<ucw:a :action (view-instance slot (foreign-instance slot)) 
	       (render)))
      (t       
       (render)))))

;;;; HAS MANY 
(defslot-presentation has-many-slot-presentation (mewa-relation-slot-presentation)
  ()
  (:type-name has-many))


(defun get-join-class-info (slot instance)
  "hack around m-v-b"
  (multiple-value-bind (s h f) (meta-model:explode-has-many instance (slot-name slot))
    (list s h f)))

(defaction add-to-has-many ((slot has-many-slot-presentation) instance)
  (destructuring-bind (class home foreign) 
      (multiple-value-funcall #'meta-model:explode-has-many instance (slot-name slot))
    (let ((new (make-instance class)))
      (setf (slot-value new foreign) (slot-value instance home))
      (meta-model:sync-instance new)
      (call-component (parent slot) (mewa:make-presentation new :type :editor)))))

(defmethod present-slot ((slot has-many-slot-presentation) instance)
  (<ucw:a :action (add-to-has-many slot instance) 
	  (<:as-html "(add new)"))
  (let ((i (get-foreign-instances slot instance))
	(linkedp (linkedp slot)))
    (<:ul 
     (dolist (s i)
       (let ((s s))
	 (setf (foreign-instance slot) s)
	 (<ucw:a :action (view-instance slot s :initargs `(:global-properties ,(list :linkedp t :editablep nil)))
		 (<:li   (setf (linkedp slot) nil)
			 (present-relation slot instance))))))))


(defmethod get-foreign-instances ((slot has-many-slot-presentation) instance)
  (slot-value instance (slot-name slot)))

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
  (<ucw:a :action (list-prev slot) (<:as-html "<<"))
  (let ((self (parent slot)))
    (<ucw:a :action (call-component self (mewa:make-presentation (car (slot-value instance (slot-name slot))) :type :listing :initargs (list :instances (instances slot))))
	    (<:as-html  (label slot) (format nil " ~a-~a " (current slot) (+ (current slot) (number-to-display slot))))))
  (<ucw:a :action (list-next slot) (<:as-html ">>"))
  (call-next-method)
  (<:as-html "total :" (len slot))) 

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