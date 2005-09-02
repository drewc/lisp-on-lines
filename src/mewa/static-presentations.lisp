;;;; -*- lisp -*-

(in-package :mewa)

(defcomponent presentation ()
  ((css-class :accessor css-class :initarg :css-class :initform nil))
  (:documentation "The super class of all UCW presentations.

A presentation object is a UCW component which knows how to
read/write different kinds of data types.

There are three major kinds of presentations:

1) object-presentation - Managing a single object.

2) slot-presentation - Managing the single parts (slots) which
   make up an object.

3) collection-presentation - Managing multiple objects.

Presentations are independant of the underlying application
specific lisp objects they manage. A presentation can be created
once and reused or modified before and aftre it has been used.

Presentations fulfill two distinct roles: on the one hand they
create, given a lisp object, a grahpical (html) rendering of that
object, they also deal with whatever operations the user might
wish to perform on that object.

* Creating Presentation Objects

Presentation objects are created by making an instance of either
an object-presentation or a collection-presentation and then
filling the slots property of this object."))

(defgeneric present (presentation)
  (:documentation "Render PRESENTATION (generally called from render-on)."))

(defmacro present-object (object &key using presentation)
  (assert (xor using presentation)
	  (using presentation)
	  "Must specify exactly one of :USING and :PRESENTATION.")
  (if using
      (destructuring-bind (type &rest args)
	  (ensure-list using)
	`(call ',type ,@args 'instance ,object))
      (rebinding (presentation)
	`(progn
	   (setf (slot-value ,presentation 'instance) ,object)
	   (call-component self ,presentation)))))

(defmacro present-collection (presentation-type &rest initargs)
  `(call ',presentation-type ,@initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; object-presentation

(defcomponent object-presentation (presentation)
  ((slots :accessor slots :initarg :slots :initform nil)
   (instance :initform nil :initarg instance :accessor instance))
  (:documentation "Presentations for single objects."))

(defmethod render-on ((res response) (o object-presentation))
  (unless (slot-value o 'instance)
    (error "Attempting to render the presentation ~S, but it has no instance object to present."
	   o))
  (present o))

(defmethod present ((pres object-presentation))
  (<:table :class (css-class pres)
    (dolist (slot (slots pres))
      (<:tr :class "presentation-slot-row"
        (<:td :class "presentation-slot-label" (<:as-html (label slot)))
	(<:td :class "presentation-slot-value" (present-slot slot (instance pres)))))
    (render-options pres (instance pres))))

(defmethod render-options ((pres object-presentation) instance)
  (declare (ignore instance pres))
  #| (<:tr
    (<:td :colspan 2 :align "center"
      (<ucw:input :type "submit" :action (ok pres) :value "Ok."))) |# )

(defaction ok ((o object-presentation) &optional (value (slot-value o 'instance)))
  (answer value))

(defmethod find-slot ((o object-presentation) slot-label)
  (find slot-label (slots o) :test #'string= :key #'label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; inline objects are extremly similar to object-presentations
;;;; except that we assume they're being edited within the context of
;;;; some other and so don't get their own edit/delete/confirm
;;;; whatever buttons.

(defcomponent inline-object-presentation (object-presentation)
  ())

(defmethod render-options ((pres inline-object-presentation) instance)
  (declare (ignore instance))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 'one line' objects

(defcomponent one-line-presentation (object-presentation)
  ((before :accessor before :initform "" :initarg :before
	   :documentation "Text to render before rendirng the slots.")
   (between :accessor between :initform " " :initarg :between
	    :documentation "Text to render between each slot.")
   (after :accessor after :initform "" :initarg after
	  :documentation "Text to render after all the slots have been rendered.")))

(defmethod present ((pres one-line-presentation))
  (<:as-is (before pres))
  (when (slots pres)
    (present-slot (first (slots pres)) (instance pres)))
  (dolist (slot (cdr (slots pres)))
    (<:as-is (between pres))
    (present-slot slot (instance pres)))
  (<:as-is (after pres)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; List

(defcomponent list-presentation (presentation)
  ((slots :accessor slots :initarg :slots)
   (editablep :accessor editablep :initform t :initarg :editablep)
   (edit-label :accessor edit-label :initform "Edit")
   (deleteablep :accessor deleteablep :initform t :initarg :deleteablep)
   (delete-label :accessor delete-label :initform "Delete")
   (instances :accessor instances)))

(defmethod initialize-instance :after ((l list-presentation) &rest initargs)
  (declare (ignore initargs))
  (setf (instances l) (get-all-instances l)))

(defmethod render-on ((res response) (l list-presentation))
  (present l))

(defgeneric get-all-instances (listing)
  (:documentation "Returns all the instances which should be viewable with LISTING.

This method is also used by relation-slot-presentations for the same reason."))

(defmethod present ((listing list-presentation))
  (<:table :class (css-class listing)
    (render-list-heading listing)
    (iterate
      (for element in (instances listing))
      (for index upfrom 0)
      (render-list-row listing element index))))

(defmethod render-list-heading ((listing list-presentation))
  (<:tr :class "presentation-list-heading-row"
    (<:th "")
    (dolist (slot (slots listing))
      (<:th :class "presentation-list-heading-cell"
        (<:as-html (label slot))))
    (<:th "")))
  
(defmethod render-list-row ((listing list-presentation) object index)
  (<:tr :class "item-row"
    (<:td :class "index-number-cell"
      (<:i (<:as-html index)))
    (dolist (slot (slots listing))
      (<:td :class "data-cell" (present-slot slot object)))
    (<:td :align "center" :valign "top"
      (when (editablep listing)
	(let ((object object))
	  (<ucw:input :type "submit"
		      :action (edit-from-listing listing object index)
		      :value (edit-label listing))))
      (<:as-is " ")
      (when (deleteablep listing)
	(let ((index index))
	  (<ucw:input :type "submit"
		      :action (delete-from-listing listing object index)
		      :value (delete-label listing)))))))

(defgeneric/cc create-from-listing (listing))

(defmethod/cc create-from-listing :after ((l list-presentation))
  (setf (instances l) (get-all-instances l)))

(defgeneric/cc delete-from-listing (listing item index))

(defmethod/cc delete-from-listing :after ((l list-presentation) item index)
  (declare (ignore item index))
  (setf (instances l) (get-all-instances l)))

(defgeneric/cc edit-from-listing (listing item index))

(defmethod/cc edit-from-listing :after ((l list-presentation) item index)
  (declare (ignore item index))
  (setf (instances l) (get-all-instances l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Searching/Filtering

(defcomponent presentation-search (presentation)
  ((criteria :accessor criteria :initform '())
   (search-presentation :accessor search-presentation :initarg :search-presentation
			:documentation "The presentation object
			used in determining what the possible
			search options are.")
   (list-presentation :accessor list-presentation :initarg :list-presentation
		      :documentation "The presentation object used when showing the results.")))

(defgeneric applicable-criteria (presentation)
  (:method-combination nconc))

(defmethod applicable-criteria nconc ((search presentation-search))
  (let ((criteria '()))
    (dolist (slot (slots (search-presentation search)))
      (setf criteria (append criteria (applicable-criteria slot))))
    (cons (make-instance 'negated-criteria :presentation search)
	  criteria)))

(defcomponent criteria ()
  ((presentation :accessor presentation :initarg :presentation)))

(defaction add-criteria ((search presentation-search) (criteria criteria))
  (push criteria (criteria search)))

(defaction drop-criteria ((search presentation-search) (criteria criteria))
  (setf (criteria search) (delete criteria (criteria search))))

(defgeneric apply-criteria (criteria instance)
  (:method-combination and))

(defmethod valid-instances ((search presentation-search))
  (let ((valid '()))
    (dolist (i (get-all-instances search))
      (block apply-criteria
	(dolist (criteria (criteria search))
	  (unless (apply-criteria criteria i)
	    (return-from apply-criteria nil)))
	(push i valid)))
    valid))

(defcomponent search-results-list (list-presentation)
  ((search-presentation :accessor search-presentation)))

(defmethod render-on ((res response) (s presentation-search))
  (<:p "Results:")
  (let ((listing (list-presentation s)))
    (<:table
      (<:tr :class "presentation-list-heading-row"
        (<:th "")
	(dolist (slot (slots (list-presentation s)))
	  (<:th :class "presentation-list-heading-cell"
	    (<:as-html (label slot))))
	(<:th ""))
      (loop
         for object in (valid-instances s)
         for index upfrom 0
         do (<:tr :class "item-row"
              (<:td :class "index-number-cell" (<:i (<:as-html index)))
              (dolist (slot (slots (list-presentation s)))
                (<:td :class "data-cell" (present-slot slot object)))
              (<:td :align "center" :valign "top"
                (when (editablep listing)
                  (let ((object object))
                    (<ucw:input :type "submit"
                                :action (edit-from-search s object index)
                                :value (edit-label listing))))
                (<:as-is " ")
                (when (deleteablep listing)
                  (let ((index index))
                    (<ucw:input :type "submit"
                                :action (delete-from-search s object index)
                                :value (delete-label listing)))))))))
  (<:p "Search Criteria:")
  (<:ul
   (render-criteria res s)
   (<:li (<ucw:input :type "submit" :action (refresh-component s)
		     :value "update"))))

(defmethod render-criteria ((res response) (s presentation-search))
  (<:ul
   (dolist (c (criteria s))
     (<:li (render-on res c)
	   (let ((c c))
	     (<ucw:input :action (drop-criteria s c) :type "submit" :value "eliminate"))))
   (let ((new-criteria nil))
     (<:li "Add Criteria: "
       (<ucw:select :accessor new-criteria
         (dolist (criteria (applicable-criteria s))
	   (<ucw:option :value criteria (<:as-html (label criteria)))))
       (<ucw:input :type "submit" :action (add-criteria s new-criteria)
		   :value "add")))))

(defgeneric/cc edit-from-search (search object index))

(defgeneric/cc delete-from-search (search object index))

;;;; meta criteria

(defcomponent negated-criteria (criteria)
  ((criteria :accessor criteria :initform nil)))

(defmethod label ((n negated-criteria)) "Not:")

(defmethod render-on ((res response) (n negated-criteria))
  (<:p "Not: "
       (when (criteria n)
	 (render-on res (criteria n))))
  (let ((new-criteria nil))
    (<:p "Set Criteria: "
      (<ucw:select :accessor new-criteria
        (dolist (criteria (applicable-criteria (presentation n)))
	  (<ucw:option :value criteria (<:as-html (label criteria)))))
      (<ucw:input :type "submit" :action (setf (criteria n) new-criteria)
		  :value "add"))))

(defmethod apply-criteria and ((n negated-criteria) instance)
  (if (criteria n)
      (not (apply-criteria (criteria n) instance))
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Slot presentations

(defcomponent slot-presentation (presentation)
  ((label :accessor label :initarg :label)
   (label-plural :accessor label-plural :initarg :label-plural)
   (getter :accessor getter :initarg :getter
	   :documentation "A function used for getting the
	   current value of the object. It will be passed the
	   objcet and must return the current value.")
   (setter :accessor setter :initarg :setter
	   :documentation "A function used for updatig the value of
	   the underlying object. It will be passed the new
	   value and the object (in that order).")
   (editablep :accessor editablep :initarg :editablep :initform t)
   (print-object-label)))

(defmethod print-object ((s slot-presentation) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (s stream :type t :identity t)
        (princ (label s) stream)
        (princ " (" stream)
        (princ (slot-value s 'print-object-label) stream)
        (princ ")" stream))))

(defgeneric present-slot (slot instance))

(defmethod initialize-instance :after ((presentation slot-presentation)
				       &key slot-name getter setter &allow-other-keys)
  (if slot-name
      (setf (slot-value presentation 'print-object-label) slot-name)
      (setf (slot-value presentation 'print-object-label) getter))
  (when slot-name
    (assert (not (or getter setter))
	    (slot-name getter setter)
	    "Can't specify :GETTER and/or :SETTER alnog with :SLOT-NAME.")
    (setf (getter presentation) (lambda (object)
				  (when (slot-boundp object slot-name)
				    (slot-value object slot-name)))
	  (setter presentation) (lambda (value object)
				  (setf (slot-value object slot-name) value)))))

(defvar *presentation-slot-type-mapping* (make-hash-table :test 'eql))

(defun register-slot-type-mapping (name class-name)
  (setf (gethash name *presentation-slot-type-mapping*) class-name))

(defmacro defslot-presentation (name supers slots &rest options)
  `(progn
     (defcomponent ,name ,(or supers `(slot-presentation))
       ,slots
       ,@(remove :type-name options :key #'car))
     ,(let ((type-name (assoc :type-name options)))
	(when type-name
	`(register-slot-type-mapping ',(second type-name) ',name)))
     ',name))

(defgeneric presentation-slot-value (slot instance)
  (:method ((slot slot-presentation) instance)
    (funcall (getter slot) instance)))

(defgeneric (setf presentation-slot-value) (value slot instance)
  (:method (value (slot slot-presentation) instance)
    (funcall (setter slot) value instance)))

(defmethod applicable-criteria nconc ((s slot-presentation))
  nil)

(defmacro criteria-for-slot-presentation (slot &body criteria-clauses)
  (rebinding (slot)
    `(list
      ,@(mapcar (lambda (criteria-clause)
		  (let ((criteria-clause (ensure-list criteria-clause)))
		    `(make-instance ',(first criteria-clause)
				    ,@(cdr criteria-clause)
				    :presentation ,slot)))
		criteria-clauses))))

(defmacro defslot-critera (class-name supers slots &key label apply-criteria)
  (with-unique-names (obj instance)
    (list
     'progn
     `(defcomponent ,class-name ,supers ,slots)
     (when label
       `(defmethod label ((,obj ,class-name))
          (format nil ,label (label (presentation ,obj)))))

     (when apply-criteria
       `(defmethod apply-criteria and ((,obj ,class-name) ,instance)
          (funcall ,apply-criteria
                   ,obj
                   ,instance
                   (presentation-slot-value (presentation ,obj) ,instance))))
     `(quote ,class-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Boolean

(defslot-presentation boolean-slot-presentation ()
  ()
  (:type-name boolean))

(defmethod present-slot ((slot boolean-slot-presentation) instance)
  (<ucw:input :type "checkbox" :accessor (presentation-slot-value slot instance))
  (setf (presentation-slot-value slot instance) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; strings

(defslot-presentation string-slot-presentation ()
  ((max-length :accessor max-length :initarg :max-length :initform nil)
   (size :accessor size :initarg :size :initform nil))
  (:type-name string))

(defmethod present-slot ((slot string-slot-presentation) instance)
  (if (editablep slot)
      (<ucw:input :type "text"
		  :accessor (presentation-slot-value slot instance)
		  :size (or (size slot)
			    (if (string= "" (presentation-slot-value slot instance))
				(size slot)
				(+ 3 (length (presentation-slot-value slot instance)))))
		  :maxlength (max-length slot))
      (<:as-html (presentation-slot-value slot instance))))

;;;; Critera

(defmethod applicable-criteria nconc ((s string-slot-presentation))
  (criteria-for-slot-presentation s
    string-starts-with
    string-contains
    string-ends-with))

(defcomponent string-criteria (criteria)
  ((search-text :accessor search-text :initform nil)))

(defmethod render-on ((res response) (criteria string-criteria))
  (<:as-html (label criteria) " ")
  (<ucw:input :type "text" :accessor (search-text criteria) :size 10))

(defslot-critera string-contains (string-criteria)
  ()
  :label "~A contains:"
  :apply-criteria (lambda (criteria instance slot-value)
		    (declare (ignore instance))
		    (and (<= (length (search-text criteria)) (length slot-value))
			 (search (search-text criteria) slot-value :test #'char-equal))))

(defslot-critera string-starts-with (string-contains)
  ()
  :label "~A starts with:"
  :apply-criteria (lambda (criteria instance slot-value)
                    (declare (ignore instance))
		    (and (<= (length (search-text criteria)) (length slot-value))
			 (= 0 (or (search (search-text criteria) slot-value
					  :test #'char-equal)
				  -1)))))

(defslot-critera string-ends-with (string-contains)
  ()
  :label "~A ends with:"
  :apply-criteria (lambda (criteria instance slot-value)
		    (declare (ignore instance))
		    (and  (<= (length (search-text criteria)) (length slot-value))
			  (= (- (length slot-value) (length (search-text criteria)))
			     (or (search (search-text criteria) slot-value
					 :from-end t
					 :test #'char-equal)
				 -1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; numbers

(defslot-presentation number-slot-presentation ()
  ((min-value :accessor min-value :initarg :min-value :initform nil)
   (max-value :accessor max-value :initarg :max-value :initform nil)))

(defcomponent number-criteria (criteria)
  ((number-input :accessor number-input :initform nil)))

(defmethod applicable-criteria nconc ((s number-slot-presentation))
  (criteria-for-slot-presentation s
    number-less-than
    number-greater-than
    number-equal-to))

(defmacro defnumber-criteria (name &key label render-on-prefix apply-criteria)
  `(progn
     (defslot-critera ,name (number-criteria)
       ()
       :label ,label
       :apply-criteria (lambda (criteria instance slot-value)
			 (declare (ignore instance))
			 (if (numberp slot-value)
			     (if (number-input criteria)
				 (funcall ,apply-criteria slot-value (number-input criteria))
				 t)
			     nil)))

     (defmethod render-on ((res response) (obj ,name))
       (<:as-html (format nil ,render-on-prefix (label (presentation obj))))
       (<ucw:input :type "text"
		   :reader (or (number-input obj) "")
		   :writer (lambda (v)
			     (unless (string= "" v)
			       (let ((n (parse-float v)))
				 (when n
				   (setf (number-input obj) n)))))))))

(defnumber-criteria number-equal-to
  :apply-criteria (lambda (slot-value number-input)
		    (= slot-value number-input))
  :label "~A is equal to:"
  :render-on-prefix "~A = ")

(defnumber-criteria number-less-than
  :apply-criteria (lambda (slot-value number-input)
		    (< slot-value number-input))
  :label "~A is less than:"
  :render-on-prefix "~A < ")

(defnumber-criteria number-greater-than
  :apply-criteria (lambda (slot-value number-input)
		    (> slot-value number-input))
  :label "~A is greater than:"
  :render-on-prefix "~A > ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Integers

(defslot-presentation integer-slot-presentation (number-slot-presentation)
  ()
  (:type-name integer))

(defmethod presentation-slot-value ((slot integer-slot-presentation) instance)
  (declare (ignore instance))
  (or (call-next-method) ""))

(defmethod (setf presentation-slot-value) ((value string) (slot integer-slot-presentation) instance)
  (unless (string= "" value)
    (let ((i (parse-integer value :junk-allowed t)))
      (when i
	(setf (presentation-slot-value slot instance) (parse-integer value))))))

(defmethod present-slot ((slot integer-slot-presentation) instance)
  (if (editablep slot)
      (<ucw:input :type "text"
		  :accessor (presentation-slot-value slot instance))
      (<:as-html (presentation-slot-value slot instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Reals

(defcomponent real-slot-presentation (number-slot-presentation)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Currency (double precision reals)

(defslot-presentation currency-slot-presentation (real-slot-presentation)
  ()
  (:type-name currency))

(defmethod (setf presentation-slot-value) ((value string) (c currency-slot-presentation) instance)
  (let ((*read-eval* nil))
    (unless (string= "" value)
      (let ((value (read-from-string value)))
	(when (numberp value)
	  (setf (presentation-slot-value c instance) value))))))

(defmethod present-slot ((currency currency-slot-presentation) instance)
  (if (editablep currency)
      (<ucw:input :type "text" :size 10
		  :accessor (presentation-slot-value currency instance))
      (<:as-html (presentation-slot-value currency instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; dates and times

(defslot-presentation timestamp-slot-presentation (slot-presentation)
  ()
  (:type-name timestamp))

(defmacro deftimestamp-slot-accessor (accessor time-accessor nth-value make-time-arg)
  (let ((accessor-name (intern (strcat '#:timestamp-slot- accessor))))
    `(progn
       (defgeneric ,accessor-name (slot instance))
       (defgeneric (setf ,accessor-name) (value slot instance))
       (defmethod ,accessor-name ((slot timestamp-slot-presentation) instance)
	 (when (presentation-slot-value slot instance)
	   (nth-value ,nth-value (,time-accessor (presentation-slot-value slot instance)))))
       (defmethod (setf ,accessor-name) ((value integer) (slot timestamp-slot-presentation) instance)
	 (if (presentation-slot-value slot instance)
	     (setf (presentation-slot-value slot instance)
		   (make-time ,make-time-arg value :defaults (presentation-slot-value slot instance)))
	     (setf (presentation-slot-value slot instance) (make-time ,make-time-arg value))))
       (defmethod (setf ,accessor-name) ((value string) (slot timestamp-slot-presentation) instance)
         (setf (,accessor-name slot instance)
               (if (string= "" value)
                   nil
                   (parse-integer value))))
       (defmethod (setf ,accessor-name) ((value null) (slot timestamp-slot-presentation) instance)
         (setf (presentation-slot-value slot instance) nil)))))

(deftimestamp-slot-accessor second time-hms 2 :second)
(deftimestamp-slot-accessor minute time-hms 1 :minute)
(deftimestamp-slot-accessor hour time-hms 0 :hour)
(deftimestamp-slot-accessor year time-ymd 0 :year)
(deftimestamp-slot-accessor month time-ymd 1 :month)
(deftimestamp-slot-accessor day time-ymd 2 :day)

(defslot-presentation ymd-slot-presentation (timestamp-slot-presentation)
  ()
  (:type-name date))

(defmethod present-slot ((slot ymd-slot-presentation) instance)
  (if (editablep slot)
      (<:progn
        (<ucw:input :class (css-class slot) :type "text" :size 2
                    :accessor (timestamp-slot-day slot instance))
        "/"
        (<ucw:input :class (css-class slot) :type "text" :size 2
                    :accessor (timestamp-slot-month slot instance))
        "/"
        (<ucw:input :class (css-class slot) :type "text" :size 4
                    :accessor (timestamp-slot-year slot instance)))
      (if (presentation-slot-value slot instance)
	  (<:progn
	    (<:as-html (timestamp-slot-day slot instance))
	    "/"
	    (<:as-html (timestamp-slot-month slot instance))
	    "/"
	    (<:as-html (timestamp-slot-year slot instance)))
	  (<:as-html "---"))))

(defmethod applicable-criteria nconc ((slot ymd-slot-presentation))
  (criteria-for-slot-presentation slot
    date-before-criteria))

(defslot-critera date-before-criteria (criteria)
  ((target :accessor target))
  :label "Date Before:")

(defmethod render-on ((res response) (dbc date-before-criteria))
  (<:as-html "Date Before: "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Relations

(defcomponent relation-slot-presentation (slot-presentation)
  ((presentation :accessor presentation
		 :initarg :presentation
		 :documentation "The class of presentation
		 objects used to fill the options of a select
		 tag.")
   (search-presentation :accessor search-presentation
			:initarg :search-presentation
			:initform nil)
   (allow-nil-p :accessor allow-nil-p
		:initarg :allow-nil-p
		:initform t
		:documentation "Can this relation not exist.")))

(defmethod presentation ((slot relation-slot-presentation))
  (with-slots (presentation)
      slot
    (if (or (symbolp presentation)
	    (consp presentation))
	(setf presentation (apply #'make-instance (ensure-list presentation)))
	presentation)))

(defgeneric get-foreign-instances (pres instance))

(defcomponent relation-criteria (criteria presentation-search)
  ((criteria :accessor criteria :initform '())))

(defmethod search-presentation ((criteria relation-criteria))
  (or (search-presentation (presentation criteria))
      (presentation (presentation criteria))))

;;;; One-Of

(defslot-presentation one-of-presentation (relation-slot-presentation)
  ((none-label :initarg :none-label :accessor none-label
	       :initform "none"))
  (:type-name one-of))

(defmethod present-slot ((slot one-of-presentation) instance)
  (if (editablep slot)
      (<ucw:select :accessor (presentation-slot-value slot instance)
        (when (allow-nil-p slot)
	  (<ucw:option :value nil (<:as-html (none-label slot))))
	(dolist (option (get-foreign-instances (presentation slot) instance))
	  (setf (instance (presentation slot)) option)
	  (<ucw:option :value option (present (presentation slot)))))
      (if (presentation-slot-value slot instance)
	  (progn
	    (setf (instance (presentation slot)) (presentation-slot-value slot instance))
	    (present (presentation slot)))
	  (<:as-html "--"))))

(defmethod applicable-criteria nconc ((slot one-of-presentation))
  (criteria-for-slot-presentation slot
    one-of-criteria
    one-of-not-null))

(defslot-critera one-of-criteria (relation-criteria)
  ())

(defmethod label ((ooc one-of-criteria))
  (strcat (label (presentation ooc)) " with:"))

(defmethod render-on ((res response) (ooc one-of-criteria))
  (<:as-html (label (presentation ooc)) " with:")
  (render-criteria res ooc))

(defmethod apply-criteria and ((ooc one-of-criteria) instance)
  (let ((nested-instance (presentation-slot-value (presentation ooc) instance))
	(criteria (criteria ooc)))
    (if criteria
	(if nested-instance
	    (dolist (c (criteria ooc) t)
	      (unless (apply-criteria c nested-instance)
		(return-from apply-criteria nil)))
	    nil)
	t)))

(defslot-critera one-of-not-null (criteria)
  ())

(defmethod label ((oonn one-of-not-null))
  (strcat (label (presentation oonn)) " exists."))

(defmethod apply-criteria and ((oonn one-of-not-null) instance)
  (not (null (presentation-slot-value (presentation oonn) instance))))

(defmethod render-on ((res response) (oonn one-of-not-null))
  (<:as-html (label (presentation oonn)) " exists."))

;;;; Some-Of

(defslot-presentation some-of-presentation (relation-slot-presentation)
  ()
  (:type-name some-of))

(defmethod present-slot ((slot some-of-presentation) instance)
  (<:ul
   (if (presentation-slot-value slot instance)
       (loop
          for option in (presentation-slot-value slot instance)
          for index upfrom 0
          do (let ((option option) ;; loop changes the values, it does
                                   ;; not create fresh bindings
                   (index index))
               (<:li
                 (<:table
                   (<:tr
                     (<:td (setf (instance (presentation slot)) option)
                           (present (presentation slot)))
                     (when (editablep slot)
                       (<:td :align "left" :valign "top"
                         (<ucw:input :type "submit"
                                     :action (delete-element slot instance option index)
                                     :value (concatenate 'string "Delete " (label slot))))))))))
       (<:li "None."))
   (render-add-new-item slot instance)))

(defmethod render-add-new-item ((slot some-of-presentation) instance)
  (let ((new-object nil)
	(foreign-instances (get-foreign-instances (presentation slot) instance)))
    (when (and foreign-instances (editablep slot))
      (<:li "Add: "
        (<ucw:select :accessor new-object
	  (dolist (option foreign-instances)
	    (setf (instance (presentation slot)) option)
	    (<ucw:option :value option (present (presentation slot)))))
	(<ucw:input :type "submit"
		    :action (add-element slot instance new-object)
		    :value "Add")))))

(defaction add-element ((some-of some-of-presentation) instance item)
  (push item (presentation-slot-value some-of instance)))

(defaction delete-element ((some-of some-of-presentation) instance item index)
  (let ((nth (nth index (presentation-slot-value some-of instance))))
    (unless (eq nth item)
      (error "Attempting to delete the ~Dth item, which should be ~S, but the ~Dth item is actually ~S."
	     index item index nth))
    (setf (presentation-slot-value some-of instance)
	  (iterate
	    (for element in (presentation-slot-value some-of instance))
	    (for i upfrom 0)
	    (unless (= index i)
	      (collect element))))))

(defmethod applicable-criteria nconc ((slot some-of-presentation))
  (criteria-for-slot-presentation slot
    some-of-any
    some-of-all))

(defslot-critera some-of-criteria (relation-criteria)
  ())

(defmethod render-on ((res response) (soa some-of-criteria))
  (<:as-html (label soa))
  (render-criteria res soa))

(defmacro defsome-of-criteria (name supers slots &key label apply-criteria)
  (with-unique-names (obj)
    `(progn
       (defslot-critera ,name ,supers ,slots)
       (defmethod label ((,obj ,name))
	 (format nil ,label (label (presentation ,obj))))
       (defmethod apply-criteria and ((,obj ,name) instance)
	 (let ((nested-instances (presentation-slot-value (presentation ,obj) instance))
	       (criteria (criteria ,obj)))
	   (if criteria
	       (if nested-instances
		   (funcall ,apply-criteria (criteria ,obj) nested-instances)
		   nil)
	       t))))))

(defsome-of-criteria some-of-any (some-of-criteria)
  ()
  :label "Any ~A with:"
  :apply-criteria (lambda (criteria nested-instances)
		    ;; return T if any nested-instance meets all of criteria
		    (some (lambda (instance)
			    (every (lambda (criteria)
				     (apply-criteria criteria instance))
				   criteria))
			  nested-instances)))

(defsome-of-criteria some-of-all (some-of-criteria)
  ()
  :label "All ~A with:"
  :apply-criteria (lambda (criteria nested-instances)
		    ;; return T only if every nested-instances meets
		    ;; all of our criteria
		    (every (lambda (instance)
			     (every (lambda (criteria)
				      (apply-criteria criteria instance))
				    criteria))
			   nested-instances)))

;;;; An-Object

(defslot-presentation an-object-presentation (one-of-presentation)
  ()
  (:type-name an-object))

(defmethod present-slot ((slot an-object-presentation) instance)
  (if (presentation-slot-value slot instance)
      (progn
	(setf (instance (presentation slot)) (presentation-slot-value slot instance))
	(present (presentation slot))
	(<ucw:input :type "submit" :action (delete-an-object slot instance)
                    :value (concatenate 'string "Delete " (label slot))))
      (<ucw:input :type "submit" :action (create-an-object slot instance) :value "Create")))

(defaction delete-an-object ((slot an-object-presentation) instance)
  (setf (presentation-slot-value slot instance) nil))

(defaction create-an-object ((slot an-object-presentation) instance)
  (let ((obj (make-new-instance (presentation slot) instance)))
    (format t "Setting (presentation-slot-value ~S ~S) to ~S.~%" slot instance obj)
    (setf (presentation-slot-value slot instance) obj)))

;;;; Some-Objects

(defslot-presentation some-objects-presentation (some-of-presentation)
  ()
  (:type-name some-objects))

(defmethod render-add-new-item ((slot some-objects-presentation) instance)
  (when (editablep slot)
    (<:li (<ucw:input :type "submit"
		      :action (add-an-object slot instance)
		      :value "Add new object."))))

(defgeneric make-new-instance (presentation instance)
  (:documentation "Create an new instance suitable for
  PRESENTATION which will be added to INSTANCE (according to
  PRESENTATION)."))

(defaction add-an-object ((slot some-objects-presentation) instance)
  (push (make-new-instance (presentation slot) instance) (presentation-slot-value slot instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Convience macros/functions

(defmacro slot-presentations (&rest slot-specs)
  `(list ,@(mapcar (lambda (slot)
		     (let ((class-name (gethash (car slot) *presentation-slot-type-mapping*)))
		       (if class-name
			   `(make-instance ',class-name ,@(cdr slot))
			   (error "Unknown slot type ~S." (car slot)))))
		   slot-specs)))

(defmacro defpresentation (name supers slots &rest default-initargs)
  `(defcomponent ,name ,supers
     ()
     (:default-initargs
       ,@(when slots `(:slots (slot-presentations ,@slots)))
       ,@default-initargs)))

