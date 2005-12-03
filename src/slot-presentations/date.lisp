(in-package :lol)

;;;; Expiry date picker

(defslot-presentation date-slot-presentation (clsql-wall-time-slot-presentation)
  ((date-field :component (my-date-field :year-min 2005 :year-max 2015)
	       :accessor date-field))
  (:type-name date))

(defmethod update-value ((slot date-slot-presentation))
  (multiple-value-bind (year month day)
      (time-ymd (presentation-slot-value slot (instance (ucw::parent slot))))
    (multiple-value-bind (new-year new-month new-day)
	(time-ymd)
      (if (remove nil (map 'list #'(lambda (old new)
				     (unless (equal (car old) (car new))
				       t))
			   (list year month day)
			   (list new-year new-month new-day)))
	  (setf (presentation-slot-value slot (instance (ucw::parent slot)))
		(make-time t))))))

(defmethod present-slot ((slot date-slot-presentation) instance)
  (let ((date (slot-value instance (slot-name slot))))
    ;; Default values
    (when (and (not date) (default-to-now-p slot))
      (setf date (clsql:get-time)
	    (presentation-slot-value slot instance) date))
    ;;simple viewer
    (if (and date (not (editablep slot)))
	(<:as-html date))
    ;; editor
    (when (editablep slot)
      (with-slots ((m month)  (y year))
	  (date-field slot)
	
      (multiple-value-bind (year month) (time-ymd date)
	(setf (lisp-value m) month
	      (lisp-value y) year)
      (<ucw:render-component :component (date-field slot)))))))




(defcomponent %integer-range-field (ucw::integer-range-field)
  ())

(defmethod (setf lisp-value) :after (value (self %integer-range-field))
  ())
  
  

(defclass %date-field (ucw::date-field)
  ((day :component (%integer-range-field :min-value 1 :max-value 31))
   (month :component (%integer-range-field :min-value 1 :max-value 12))
   (year :component (%integer-range-field :min-value 2005 :max-value 2015) ))
  (:metaclass standard-component-class))

(defmethod shared-initialize :after ((field %date-field) slot-names
                                     &key (year-min 1960) (year-max 2010))
  (declare (ignore slot-names year-min year-max))
  (mapcar #'(lambda (x) (setf (slot-value (slot-value field x) 'ucw::parent) field))
	  '(year month day)))

(defclass my-date-field (%date-field)
  ()
  (:metaclass standard-component-class))

(defmethod present ((date my-date-field))
  (with-slots (year month)
      date
    (<ucw:render-component :component month)
      "/"
      (<ucw:render-component :component year)))

