(in-package :mewa)

(defaction edit-instance ((self mewa))
  (call-presentation (instance self) :type :editor))

;;;one-line objects
(defcomponent mewa-one-line-presentation (mewa one-line-presentation)
  ()
  (:default-initargs :attributes-getter #'one-line-attributes-getter))

(defmethod one-line-attributes-getter ((self mewa))
  (or (meta-model:list-keys (instance self))))

;;;objects
(defcomponent mewa-object-presentation (mewa object-presentation) 
  ((instance :accessor instance :initarg :instance :initform nil)))

(defmethod present ((pres mewa-object-presentation))
  (<:table :class (css-class pres)
    (dolist (slot (slots pres))
      (<:tr :class "presentation-slot-row"
	    (present-slot-as-row pres slot))))
    (render-options pres (instance pres)))
        
(defmethod present-slot-as-row ((pres mewa-object-presentation) (slot slot-presentation))
  (<:td :class "presentation-slot-label" (<:as-html (label slot)))
  (<:td :class "presentation-slot-value" (present-slot slot (instance pres))))


(defcomponent two-column-presentation (mewa-object-presentation) ())

(defmethod present ((pres two-column-presentation))
  
  (<:table :class (css-class pres)
	   (loop for slot on (slots pres) by #'cddr
		 do 
		 (<:tr :class "presentation-slot-row"
		       (<:td :class "presentation-slot-label" 
			     (<:as-html (label (first slot))))
		       (<:td :class "presentation-slot-value" 
			     (present-slot (first slot) (instance pres)))
		       (when (second slot)
			 (<:td :class "presentation-slot-label" 
			       (<:as-html (label (second slot))))
			 (<:td :class "presentation-slot-value" 
			       (present-slot (second slot) (instance pres))))))
	   (render-options pres (instance pres))))


;;;lists
(defcomponent mewa-list-presentation (mewa list-presentation) 
  ((instances :accessor instances :initarg :instances :initform nil)
   (instance :accessor instance)
   (select-label :accessor select-label :initform "select" :initarg :select-label)
   (selectablep :accessor selectablep :initform t :initarg :selectablep)
   (deleteablep :accessor deletablep :initarg :deletablep :initform nil)
   (viewablep :accessor viewablep :initarg :viewablep :initform nil)))

(defaction select-from-listing ((listing mewa-list-presentation) object index)
  (answer object))

(defmethod render-list-row ((listing mewa-list-presentation) object index)
  (<:tr :class "item-row"
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
		      :value (delete-label listing))))
      (when (selectablep listing)
	(let ((index index))
	  (<ucw:input :type "submit"
		      :action (select-from-listing listing object index)
		      :value (select-label listing))))
      (when (viewablep listing)
	(let ((index index))
	  (<ucw:input :type "submit"
		      :action (call-component listing  (make-presentation object))
		      :value "view"))))
    (dolist (slot (slots listing))
      (<:td :class "data-cell" (present-slot slot object)))
    (<:td :class "index-number-cell")
    ))

(defmethod get-all-instances ((self mewa-list-presentation))
  (instances self))


;;;; * Presentation Searches


;;;; ** "search all fields" criteria

(defgeneric search-expr (criteria instance)
  (:documentation "Return ready to apply criteria.
                   What to do with it is backend dependent."))

(defmacro def-search-expr (((self criteria-type)) (model-expr &body body))
  `(defmethod search-expr ((,self ,criteria-type) instance)
     (,model-expr
      instance
      (slot-name (presentation ,self))
      ,@body)))

(defmethod search-expr ((self negated-criteria) instance)
  (when (criteria self)
    (meta-model:expr-not
     instance
     (search-expr (criteria self) instance))))

(def-search-expr ((self string-starts-with))
    (meta-model:expr-starts-with (search-text self)))

(def-search-expr ((self string-ends-with))
    (meta-model:expr-ends-with (search-text self)))

(def-search-expr ((self string-contains))
    (meta-model:expr-contains (search-text self)))

(def-search-expr ((self number-less-than))
    (meta-model:expr-< (number-input self)))

(def-search-expr ((self number-greater-than))
    (meta-model:expr-> (number-input self)))

(def-search-expr ((self number-equal-to))
    (meta-model:expr-= (number-input self)))



(defcomponent mewa-presentation-search (presentation-search)
  ((display-results-p :accessor display-results-p :initarg :display-results-p :initform nil)
   (criteria-input :accessor criteria-input :initform "")
   (new-criteria :accessor new-criteria :initform nil)))

(defmethod instance ((self mewa:mewa-presentation-search))
  (instance (search-presentation self)))

(defmethod search-expr ((self mewa:mewa-presentation-search) instance)
  (apply #'meta-model:expr-and instance
         (mapcan (lambda (c) (let ((e  (search-expr c instance)))
                               (if (listp e) e (list e))))
                 (criteria self))))

(defmethod search-query ((self mewa:mewa-presentation-search))
  (search-expr self (instance self)))

(defmethod valid-instances ((self mewa:mewa-presentation-search))
  (meta-model:select-instances (instance self) (search-query self)))

(defmethod get-all-instances ((self mewa-presentation-search))
  (meta-model:select-instances (instance self)))

(defmethod ok ((self mewa-presentation-search) &optional arg)
  (declare (ignore arg))
  (setf (instances (list-presentation self)) (valid-instances self))
  (setf (display-results-p self) t))


(defmethod set-search-input-for-criteria ((criteria criteria) (input t))
  (error "No search-input-for-criteria method for ~A : ~A" criteria input))

(defmethod set-search-input-for-criteria ((c string-criteria) input)
  (setf (search-text c) input))

(defmethod set-search-input-for-criteria ((c negated-criteria) i)
  nil)


(defmethod mewa-add-criteria ((self component) (criteria criteria))
  (set-search-input-for-criteria criteria (criteria-input self))
  (add-criteria self criteria))

(defmethod find-default-criteria (c mewa-string-slot-presentation)
  'string-contains)



(defmethod render-criteria ((res response) (s mewa-presentation-search))
  (setf (criteria-input s) "")
  (<:ul
   (dolist (c (criteria s))
     (<:li (render-on res c)
	   (let ((c c))
	     (<ucw:input :action (drop-criteria s c) :type "submit" :value "eliminate"))))
     (<:li 
      "Search For: "
      (<ucw:input :type "text" :accessor (criteria-input s))
      " Using : "
       (<ucw:select :accessor (new-criteria s) 
         (dolist (criteria (applicable-criteria s))
	   (<ucw:option :value criteria (<:as-html (label criteria)))))
       (<ucw:input :type "submit" :action (mewa-add-criteria s (new-criteria s))
		   :value "add"))))

(defmethod submit-search ((s mewa-presentation-search))
  (with-slots (criteria-input) s
    
    (unless (or (null criteria-input)
		(string-equal "" (remove #\Space criteria-input)))
      
      (mewa-add-criteria s (new-criteria s)))
	       
    (ok s)))

(defmethod render-on ((res response) (self mewa-presentation-search))
  ;(<:as-html (search-query self))
  (render-criteria res self)
  (<ucw:input :type "submit" :value "Search" :action (submit-search self))
  (when (display-results-p self)
    (let ((listing (list-presentation self)))
      (setf 
       (slot-value listing 'ucw::calling-component) (slot-value self 'ucw::calling-component)
       (slot-value listing 'ucw::place) (slot-value self 'ucw::place)
       (slot-value listing 'ucw::continuation) (slot-value self 'ucw::continuation))
      
      (render-on res listing))))


;;;; 
(defcomponent dont-show-unset-slots ()())

(defmethod slots :around ((self dont-show-unset-slots))
  (remove-if-not #'(lambda (s) (let ((s (presentation-slot-value s (instance self))))
				 (and s (not (equal "" s)))))
		 (call-next-method)))