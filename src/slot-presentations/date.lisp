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




(defcomponent %integer-range-field (integer-range-field)
  ())

(defmethod (setf lisp-value) :after (value (self %integer-range-field))
  ())
(defclass date-field (form-element)
  ((day :component (integer-range-field :min-value 1 :max-value 31))
   (month :component (integer-range-field :min-value 1 :max-value 12))
   (year :component integer-range-field))
  (:metaclass standard-component-class))

(defmethod shared-initialize :after ((field date-field) slot-names
                                     &key (year-min 1960) (year-max 2010))
  (declare (ignore slot-names))
  (setf (min-value (slot-value field 'year)) year-min
        (max-value (slot-value field 'year)) year-max
        (max-value (slot-value field 'day)) 31
        (max-value (slot-value field 'month)) 12))

(defmethod read-client-value ((date date-field))
  (with-slots (year month day)
      date
    (read-client-value year)
    (read-client-value month)
    (read-client-value day)
    (setf (lisp-value date) (encode-universal-time 0 0 0
                                                   (lisp-value day)
                                                   (lisp-value month)
                                                   (lisp-value year)))))

 
(defclass %date-field (date-field)
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



(defconstant +uninitialized+ '+uninitialized+
  "The value used in UCW form elements to specify that there is no value.

This obviously implies that you can't have a form element whose
real value is +uninitialized+, since +uninitialized+ is a ucw
internal symbol this shouldn't be a problem.")

(defclass form-element (widget-component)
  ((client-value :accessor client-value :initform ""
		 :initarg :client-value
                 :documentation "Whetever the client's browse sent for this form element."
                 :backtrack t)
   (lisp-value :accessor lisp-value :initform +uninitialized+
               :initarg :lisp-value
               :documentation "The current lisp object in this form element."
               :backtrack t))
  (:metaclass standard-component-class)
  (:documentation "A single value in a form.

A form-element is, simply put, a wrapper for a value in an html
form."))

(defgeneric read-client-value (element)
  (:method ((element form-element))
    (setf (lisp-value element) (client-value element))))

(defclass form-component (widget-component)
  ()
  (:metaclass standard-component-class))

;; remeber that actions are just methods
(defgeneric/cc submit (form))

(defaction submit :before ((f form-component))
  (iterate
    (with form-element-class = (find-class 'form-element))
    (for slot in (mopp:class-slots (class-of f)))
    (for slot-name = (mopp:slot-definition-name slot))
    (when (and (slot-boundp f slot-name)
               (subtypep (class-of (slot-value f slot-name)) form-element-class))
      (read-client-value (slot-value f slot-name)))))

(defaction submit ((f form-component)) t)


(defclass select-field (form-element)
  ((options :accessor options :initform '() :initarg :options)
   (key :accessor key :initform #'identity :initarg :key)
   (test :accessor test :initform #'eql :initarg :test)
   (option-map :accessor option-map :initform (make-array 10 :adjustable t :fill-pointer 0))
   (option-writer :accessor option-writer :initform #'princ-to-string))
  (:metaclass standard-component-class))

(defmethod render-option ((select select-field) (object t))
  (<:as-html (funcall (option-writer select) object)))

(defmethod render ( (select select-field))
  (setf (fill-pointer (option-map select)) 0)
  (<:select :name (make-new-callback (context.current-frame *context*)
                                     (lambda (v) (setf (client-value select) v)))
    (iterate
      (for o in (options select))
      (for index upfrom 0)
      (vector-push-extend o (option-map select))
      (<:option :value index
                :selected (funcall (test select)
                                   (funcall (key select) o)
                                   (funcall (key select) (lisp-value select))) 
        (render-option res select o)))))

(defmethod read-client-value ((select select-field))
  (with-slots (lisp-value option-map client-value)
      select
    (setf lisp-value (aref option-map (parse-integer client-value)))))

;;;; Numbers from text inputs

(defclass number-field (form-element)
  ((min-value :accessor min-value :initform nil :initarg :min-value)
   (max-value :accessor max-value :initform nil :initarg :max-value)
   (size :accessor size :initarg :size :initform 0)
   (maxlength :accessor maxlength :initarg :maxlength :initform 20))
  (:metaclass standard-component-class))

(defmethod validate-form-element ((number number-field))
  (with-slots (min-value max-value lisp-value)
      number
    (if (eql +uninitialized+ lisp-value)
	nil
	(if (numberp lisp-value)
	    (cond
	      ((and min-value max-value)
	       (< min-value lisp-value max-value))
	      (min-value (< min-value lisp-value))
	      (max-value (< lisp-value max-value))
	      (t lisp-value))
	    nil))))

(defmethod read-client-value :around ((number number-field))
  (unless (or (null (client-value number))
              (string= "" (client-value number)))
    (ignore-errors ; returns NIL in case of SIMPLE-PARSE-ERROR
      (call-next-method))))

(defmethod render ( (n number-field))
  (<ucw:input :type "text" :accessor (client-value n)
              :size (size n)
	      :value (if (eql +uninitialized+ (lisp-value n))
			 ""
			 (lisp-value n))
	      :maxlength (maxlength n)))

(defclass decimal-field (number-field)
  ((precision :accessor precision :initarg :precision :initform nil
              :documentation "Number of significant digits."))
  (:metaclass standard-component-class))

(defmethod read-client-value ((decimal number-field))
  (setf (lisp-value decimal) (parse-float (client-value decimal))))

(defclass integer-field (number-field)
  ()
  (:metaclass standard-component-class))

(defmethod read-client-value ((integer integer-field))    
  (setf (lisp-value integer) (parse-integer (client-value integer))))

(defclass integer-range-field (integer-field)
  ()
  (:metaclass standard-component-class)
  (:default-initargs :min-value 1 :max-value 5))

(defmethod shared-initialize :after ((field integer-range-field) slot-names
                                     &rest initargs)
  (declare (ignore slot-names initargs))
  (setf (lisp-value field) (min-value field)))

(defmethod render ( (range integer-range-field))
  (<:select :name (ucw::make-new-callback 
                                     (lambda (v) (setf (client-value range) v)))
    (iterate
      (for value from (min-value range) to (max-value range))
      (<:option :value value :selected (= value (lisp-value range))
                (<:as-html value)))))