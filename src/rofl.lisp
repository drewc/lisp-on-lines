(in-package :lisp-on-lines)


(defclass db-access-slot-definition ()
  ((column-name  :initform nil :initarg :db-name :accessor slot-definition-column-name
               :documentation
              "If non-NIL, contains the name of the column this slot is representing.")
   (primary-key :initform nil 
		:initarg :primary-key 
		:accessor slot-definition-primary-key-p)
   (transient  :initform nil :initarg :transient :accessor slot-definition-transient-p
               :documentation
              "If non-NIL, this slot should be treated as transient and
ignored in all database related operations.")
   (not-null :initform nil :initarg :not-null :accessor slot-definition-not-null-p
             :documentation "If non-NIL, a NON NULL database
constrained will be introduced.")
   (foreign-type 
    :initform nil 
    :initarg :foreign-type
    :initarg :references
    :accessor slot-definition-foreign-type)
   (unique :initform nil :initarg :unique :accessor slot-definition-unique)
   

   (on-delete :initform :cascade :initarg :on-delete :accessor slot-definition-on-delete
              :documentation "Action to be performed for this slot
when the refering row in the database ceases to exist. Possible
values: :CASCADE, :RESTRICT, :SET-NULL, :SET-DEFAULT. If this slot is
not a foreign key, it does nothing.")
   (delayed-constraint :initform nil :accessor slot-definition-delayed-constraint
                        :documentation "Closures adding constraints
that, for some reason, could not be executed. If there's a slot with
this attribute not-NIL in a class definition, then there's something
wrong with its SQL counterpart.")))

(defmethod slot-definition-column-name :around (slotd)
  (or (call-next-method) (slot-definition-name slotd)))


(defclass db-access-class (standard-class)
  ((table-name :initarg :table-name :initform nil :accessor class-table-name)
   (indices :initarg :indices :initform () :reader class-indices)
   (unique :initarg :unique :initform () :reader class-unique)
   #+not!(connection-spec :initarg :connection-spec :initform nil :reader db-class-connection-spec)
   
   (unfinished-classes :initform nil :allocation :class :accessor class-unfinished-classes
                       :documentation "A class allocated slot
containing classes for whom not all the constraints could be
applied.")
   (foreign-keys :initform nil :accessor class-foreign-keys
                 :documentation "List of foreign-key slots.")
   (unique-keys :initform nil :accessor class-unique-keys
                :documentation "List of slots whose value should be unique."))
  (:documentation "Metaclass for PostgreSQL aware classes. It takes
two additional arguments in DEFTABLE: :INDICES (which slots are used
as indices) and :CONNECTION-SPEC, which specifies how the class should
connect to the database (its format is the same as in
POSTMODERN:CONNECT-TOPLEVEL). If :CONNECTION-SPEC is not provided,
SUBMARINE assumes it is a class created just for the sake of
inheritance and does not create any tables for it."))

(defmethod validate-superclass
           ((class db-access-class)
            (superclass standard-class))
  t)


(defclass db-access-direct-slot-definition (standard-direct-slot-definition
					    db-access-slot-definition)
  ())

(defmethod direct-slot-definition-class
           ((class db-access-class) &key &allow-other-keys)
  (find-class 'db-access-direct-slot-definition))

(defclass db-access-effective-slot-definition 
    (standard-effective-slot-definition
     db-access-slot-definition)
  ())

(defmethod effective-slot-definition-class
           ((class db-access-class) &key &allow-other-keys)
  (find-class 'db-access-effective-slot-definition))

(defmethod compute-effective-slot-definition
           ((class db-access-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((slotd (call-next-method)))
    (setf (slot-definition-primary-key-p slotd) 
	  (some #'slot-definition-primary-key-p direct-slot-definitions)
	  (slot-definition-transient-p slotd) 
	  (every #'slot-definition-transient-p direct-slot-definitions)
	  (slot-definition-foreign-type slotd) 
	  (slot-definition-foreign-type (car direct-slot-definitions))
	  (slot-definition-not-null-p slotd) 
	  (slot-definition-not-null-p (car direct-slot-definitions))
	  (slot-definition-unique slotd) (slot-definition-unique (car direct-slot-definitions))
	  (slot-definition-type slotd) (slot-definition-type (car direct-slot-definitions)))
    slotd))

(defun class-id-slot-definition (class)
  (find-if #'slot-definition-primary-key-p 
	   (class-slots class)))

(defmethod class-table-name :around (class)
  (or (call-next-method) 
      (class-name class)))

(defclass standard-db-access-class (db-access-class)
  ())

(defun dao-id-column-name (class)
  (slot-definition-column-name
   (or (class-id-slot-definition class)
       (error "No ID slot (primary key) for ~A" class))))

(defclass described-db-access-class (standard-db-access-class described-class)
  ())

(defmethod initialize-instance :around ((class standard-db-access-class) &rest initargs &key (direct-superclasses '()))
  (declare (dynamic-extent initargs))
  (if (loop for direct-superclass in direct-superclasses
	 thereis (ignore-errors (subtypep direct-superclass 'standard-db-access-object)))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'standard-db-access-object)))
	     initargs)))

(defmethod reinitialize-instance :around ((class standard-db-access-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
	  (loop for direct-superclass in direct-superclasses
	     thereis (ignore-errors (subtypep direct-superclass 'standard-db-access-object))))
      (call-next-method)
      (apply #'call-next-method
	     class
	     :direct-superclasses
	     (append direct-superclasses
		     (list (find-class 'standard-db-access-object)))
	     initargs)))

(defclass standard-db-access-object (standard-object)
  ())



(defun find-dao (type id 
		 &key (table (class-table-name (find-class type)))
		      id-column-name)
			     
  "Get the dao corresponding to the given primary key,
or return nil if it does not exist."
  (let ((row (first (query 
	      (:select '* 
	       :from table 
               :where (:= id (or id-column-name
				 (dao-id-column-name 
				  (find-class type)))))))))
    (make-dao-from-row type row)))

(defmethod shared-initialize :after ((dao standard-db-access-object) 
				     slots &rest initargs)
  (let ((class (class-of dao)))
    (dolist (slotd (class-slots class))
      (with-slots (foreign-type) slotd
	(when foreign-type
	  (if (slot-boundp-using-class class dao slotd)
	      (let ((value (slot-value-using-class class dao slotd)))		
		(unless (typep value foreign-type)
		  (if (connected-p *database*)
		      (setf (slot-value-using-class class dao slotd)
			    (find-dao foreign-type value))
		      (let ((obj (make-instance foreign-type)))
			(setf (slot-value-using-class 
			       (class-of obj)
			       obj
			       (class-id-slot-definition (class-of obj)))
			      value)))))))))))
			       
(defgeneric dao-id (dao)
  (:method ((dao standard-db-access-object))
    (let ((class (class-of dao)))
      
      (slot-value-using-class class dao (class-id-slot-definition class)))))

(postmodern::def-row-reader symbol-plist-row-reader (fields)

  (let ((symbols (map 'list (lambda (desc) 
		   (postmodern::from-sql-name (postmodern::field-name desc))) fields)))
    (loop :while (postmodern::next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :nconc (list symbol (postmodern::next-field field))))))


(setf postmodern::*result-styles* 
      (nconc (list '(:plists symbol-plist-row-reader nil)
		   '(:plist symbol-plist-row-reader t))
	     postmodern::*result-styles*))

(defun select (&rest query)
    (query (sql-compile (cons :select query)) :plists))

(defun select-only (num &rest query)
  (query (sql-compile `(:limit ,(cons :select query) ,num)) 
	 :plists))

(defun make-dao-from-row (type row &key slots)
  (let* ((class (find-class type))
	 (dao (make-instance class))
	 (slotds (class-slots class)))
    (loop 
	 :for val :in row 
	 :for slotd 
       :in (or 
	    (loop 
	       :for slot :in slots 
	       :collect (find slot slotds 
			      :key #'slot-definition-name))
	    slotds)
	 :do (setf (slot-value-using-class class dao slotd) val)
	 :finally (return (reinitialize-instance dao)))))
  
;(defgeneric make-dao (type &rest initargs)
#+nil(defun make-dao (type initargs)
  "Create a DAO of the given `TYPE' and initialize it according
  to the values of the alist `INITARGS'. `Initargs' may contain
  additional values, not used in the initialization proccess."
  (let ((instance (make-instance type)))
    (iter (for slot in (slots-of instance))
	  (setf (slot-value instance (slot-definition-name slot))
		(let ((the-value (cdr (assoc (intern (symbol-name (slot-definition-name slot)) 'keyword) initargs))))
		  (if (foreign-type-p slot)
		      (make-instance (sb-pcl:slot-definition-type slot) :id the-value)
		      the-value))))
    instance))





