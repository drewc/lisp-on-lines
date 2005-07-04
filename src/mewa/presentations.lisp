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
(defcomponent mewa-object-presentation (mewa ucw:object-presentation) ())

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
(defcomponent mewa-list-presentation (mewa ucw:list-presentation) 
  ((ucw::instances :accessor instances :initarg :instances :initform nil)
   (instance :accessor instance)
   (select-label :accessor select-label :initform "select" :initarg :select-label)
   (selectablep :accessor selectablep :initform t :initarg :selectablep)))

(defaction select-from-listing ((listing mewa-list-presentation) object index)
  (answer object))

(defmethod render-list-row ((listing mewa-list-presentation) object index)
  (<:tr :class "item-row"
    (<:td :align "center" :valign "top"
      (when (ucw::editablep listing)
	(let ((object object))
	  (<ucw:input :type "submit"
		      :action (edit-from-listing listing object index)
		      :value (ucw::edit-label listing))))
      (<:as-is " ")
      (when (ucw::deleteablep listing)
	(let ((index index))
	  (<ucw:input :type "submit"
		      :action (delete-from-listing listing object index)
		      :value (ucw::delete-label listing))))
      (when (selectablep listing)
	(let ((index index))
	  (<ucw:input :type "submit"
		      :action (select-from-listing listing object index)
		      :value (select-label listing)))))
    (dolist (slot (slots listing))
      (<:td :class "data-cell" (present-slot slot object)))
    (<:td :class "index-number-cell")
    ))

(defmethod get-all-instances ((self mewa-list-presentation))
  (instances self))


;;; searching
(defgeneric search-expr (criteria instance)
  (:documentation "Return ready to apply criteria.
                   What to do with it is backend dependent."))

(defmacro def-search-expr (((self criteria-type)) (model-expr &body body))
  `(defmethod search-expr ((,self ,criteria-type) instance)
     (,model-expr
      instance
      (ucw::slot-name (ucw::presentation ,self))
      ,@body)))

(defmethod search-expr ((self ucw::negated-criteria) instance)
  (when (ucw::criteria self)
    (meta-model:expr-not
     instance
     (search-expr (ucw::criteria self) instance))))

(def-search-expr ((self ucw::string-starts-with))
    (meta-model:expr-starts-with (ucw::search-text self)))

(def-search-expr ((self ucw::string-ends-with))
    (meta-model:expr-ends-with (ucw::search-text self)))

(def-search-expr ((self ucw::string-contains))
    (meta-model:expr-contains (ucw::search-text self)))

(def-search-expr ((self ucw::number-less-than))
    (meta-model:expr-< (ucw::number-input self)))

(def-search-expr ((self ucw::number-greater-than))
    (meta-model:expr-> (ucw::number-input self)))

(def-search-expr ((self ucw::number-equal-to))
    (meta-model:expr-= (ucw::number-input self)))

(defcomponent mewa-presentation-search (ucw::presentation-search)
  ((display-results-p :accessor display-results-p :initarg :display-results-p :initform nil)))

(defmethod instance ((self mewa:mewa-presentation-search))
  (instance (ucw::search-presentation self)))

(defmethod search-expr ((self mewa:mewa-presentation-search) instance)
  (apply #'meta-model:expr-and instance
         (mapcan (lambda (c) (let ((e (search-expr c instance)))
                               (if (listp e) e (list e))))
                 (ucw::criteria self))))

(defmethod search-query ((self mewa:mewa-presentation-search))
  (search-expr self (instance self)))

(defmethod valid-instances ((self mewa:mewa-presentation-search))
  (meta-model:select-instances (instance self) (search-query self)))

(defmethod get-all-instances ((self mewa-presentation-search))
  (meta-model:select-instances (instance self)))

(defmethod ok ((self mewa-presentation-search) &optional arg)
  (declare (ignore arg))
  (setf (ucw::instances (ucw::list-presentation self)) (valid-instances self))
  (setf (display-results-p self) t))

(defmethod render-on ((res response) (self mewa-presentation-search))
  (<:as-html (search-query self))
  (ucw::render-criteria res self)
  (<ucw:input :type "submit" :value "Search" :action (ok self))
  (when (display-results-p self)
    (let ((listing (ucw::list-presentation self)))
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