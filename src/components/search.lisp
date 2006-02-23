(in-package :lisp-on-lines)


(defmethod simple-word-search (class-name slots search-terms)  
  (select class-name 
	  :where  (simple-word-search-where class-name slots search-terms)
	  :flatp t))

(defmethod simple-word-search-where (class-name slots search-terms)
    (sql-or 
		  (mapcar #'(lambda (term)
			      (apply #'sql-or 
				     (mapcar #'(lambda (slot)  
						 (sql-uplike
						  (sql-slot-value class-name slot)
						  (format nil "%~a%" term)))
					     slots)))
			  search-terms)))

(defmethod find-slots-of-type (model &key (type 'string)
			      (types '((string)) types-supplied-p))
  "returns a list of slots matching TYPE, or matching any of TYPES"
  (let (ty)
    (if types-supplied-p 
	(setf ty types)
	(setf ty (list type)))
    (remove nil (mapcar #'(lambda (st) (when (member (second st) ty)
					 (first st)))
	     (list-slot-types model)))))

;;;; * Simple Search Component

(defcomponent simple-search ()
  ((search-term :initarg :search-term :accessor search-term :initform "")
   (listing :initarg :listing :accessor listing :initform :listing)
   (select-returns-p :initarg :select-returns-p :accessor select-returns-p :initform nil)
   (search-tables :initarg :search-tables :accessor search-tables :initform nil)))

(defmethod render-on ((res response)(self simple-search))
  (<ucw:input :type "text" :accessor (search-term self))
  (<ucw:submit :action (do-search self)))

(defmethod perform-simple-search ((self simple-search) &key (base-classes (meta-model:list-base-classes :clsql)))
  (when (search-tables self)
    (setf base-classes (search-tables self)))
  (remove nil (mapcar #'(lambda (x) 
			  (simple-word-search  x 
					       (find-slots-of-type x) 
					       (split-sequence #\Space (search-term self))))
		      base-classes)))


(defaction do-search ((self simple-search))
  (let* ((target (or (slot-value self 'ucw::parent) self))
	 (result (call-component 
		  target 
		  (make-instance 'simple-search-results 
				 :listing (listing self)
				 :results 
				 (perform-simple-search self :base-classes 
							(remove 'claim-history (meta-model:list-base-classes :clsql)))
				 :search-term (split-sequence #\Space (search-term self))))))
    (when result
      (if (select-returns-p self)
	  (answer result)
	  (call-component target (make-presentation result :type :viewer))))))

(defcomponent simple-search-results ()
  ((results :accessor results :initarg :results :initform nil)
   (listing :initarg :listing :accessor listing :initform :listing)
   (search-term :initarg :search-term :accessor search-term :initform nil)))

(defmethod view-name (view)
  (class-name (class-of view)))

(defmethod render-on ((res response) (self simple-search-results))
  (<:h3 (<:as-html "Search results for " (search-term self)))
  (dolist (r (results self))
    (<:fieldset 
     (<:legend (<:as-html (format nil "Found ~A results in ~A:" (length r) (view-name (car r)))))
    (render-on res 
	       (embed-component 
		self 
		(make-presentation 
		 (car r) 
		 :type :listing 
		 :initargs `(:instances ,r)))))))

(defaction ok ((self simple-search-results) &optional arg)
  (declare (ignore arg))
  (answer nil))



;;;; * Advanced Search Component 

(defcomponent advanced-search () 
  ((simple-search :component simple-search :accessor simple-search)
   (search-table :accessor search-table :initform nil)
   (search-presentation :accessor search-presentation :initform nil)))

(defmethod render-on ((res response) (self advanced-search))
  (<:h2 (<:as-html "Advanced Search"))
  ;; simple search :
  (<:fieldset 
   (<:legend (<:as-html "simple text search")) 
   (render-on res (simple-search self)))
  ;; complex-search
  (<:fieldset 
   (<:legend (<:as-html "Complex Search"))
   (<:as-html "Choose search table:")
   (<ucw:select 
    :accessor (search-table self)
    (dolist (tbl (meta-model:list-base-classes :clsql))
      (<ucw:option :value tbl (<:as-html tbl))))
   (<ucw:submit :action (select-search-table self) :value "select")
   ;;
   (when (search-presentation self)
     (<:fieldset 
      (<:legend (<:as-html (format nil "search ~A" (search-table self))))
      (render-on res (embed-component self (search-presentation self)))))))


(defun make-search-presentation (instance )
  (make-instance 'mewa::mewa-presentation-search
		     :search-presentation (make-presentation instance :type :search-model)
		     :list-presentation (make-presentation instance :type :listing
(defaction select-search-table ((self advanced-search))
  (let* ((i (make-instance (search-table self)))
	 (p (make-search-presentation i)))
    (embed-component self p)
    (setf (search-presentation self) p) ))


(defcomponent table-search 




