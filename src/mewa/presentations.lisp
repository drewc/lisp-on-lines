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

(defcomponent mewa-presentation-search (ucw::presentation-search) 
 ((display-results-p :accessor display-results-p :initarg :display-results-p :initform nil)))

(defmethod ok ((self mewa-presentation-search) &optional arg)
  (declare (ignore arg))
  (setf (display-results-p self) t))

(defmethod get-all-instances ((self mewa-presentation-search))
  (clsql:select (class-name (class-of (instance (ucw::search-presentation self)))) :flatp t))

(defmethod render-on ((res response) (self mewa-presentation-search))
  (ucw::render-criteria res self)
  (when (display-results-p self)
    (let ((listing (ucw::list-presentation self))) 
      (setf (instances listing ) (ucw::valid-instances self)
	    (slot-value listing 'ucw::calling-component) (slot-value self 'ucw::calling-component)
	    (slot-value listing 'ucw::place) (slot-value self 'ucw::place)
	    (slot-value listing 'ucw::continuation) (slot-value self 'ucw::continuation))
    
      (render-on res listing))))