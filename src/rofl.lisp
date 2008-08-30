(in-package :lisp-on-lines)

;;;; NB: These could really be in upstream

;;;; * A PLIST reader for postmodern.	  
(postmodern::def-row-reader symbol-plist-row-reader (fields)
  (let ((symbols (map 'list (lambda (desc) 
		   (postmodern::from-sql-name (postmodern::field-name desc))) fields)))
    (loop :while (postmodern::next-row)
          :collect (loop :for field :across fields
                         :for symbol :in symbols
                         :nconc (list symbol (postmodern::next-field field))))))

(s-sql::def-sql-op :between (n start end)
  `(,@(s-sql::sql-expand n) " BETWEEN " ,@(s-sql::sql-expand start) " AND " ,@(s-sql::sql-expand end)))

(s-sql::def-sql-op :case (&rest clauses)
  `("CASE " ,@(loop for (test expr) in clauses collect (format nil "WHEN ~A THEN ~A " (s-sql::sql-expand test) (s-sql::sql-expand expr))) "END"))


;;;; now the rofl code itself

(defvar *row-reader* 'symbol-plist-row-reader)

(defun %query (query)
  (cl-postgres:exec-query *database* (sql-compile query) *row-reader*))

(defun select (&rest query)
  (%query (cons :select query)))

(defun prepare (&rest query)
  (cl-postgres:prepare-query *database* "test2" (sql-compile (cons :select query))))


(defun select-only (num &rest query)
  (let ((results (%query `(:limit ,(cons :select query) ,num))))
    (if (eql 1 num)
	(first results)
	results)))

(defun insert-into (table &rest values-plist)
  (postmodern:execute 
   (postmodern:sql-compile `(:insert-into ,table :set ,@values-plist))))
    

(defclass db-access-slot-definition ()
  ((column-name  :initform nil 
		 :initarg :db-name 
		 :initarg :column
		 :accessor slot-definition-column-name
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
   (foreign-relation
    :initform nil
    :initarg :referenced-from
    :initarg :referenced-by
    :accessor slot-definition-foreign-relation)
   (foreign-join-spec
    :initform nil
    :initarg :on
    :initarg :using
    :accessor slot-definition-foreign-join-spec)
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
	  (slot-definition-column-name slotd)
	  (or (let ((slot (find-if #'slot-definition-column-name direct-slot-definitions)))
		(when slot
		  (slot-definition-column-name slot)))
	      name)
	  (slot-definition-transient-p slotd) 
	  (every #'slot-definition-transient-p direct-slot-definitions)
	  (slot-definition-foreign-type slotd) 
	  (slot-definition-foreign-type (car direct-slot-definitions))
	  (slot-definition-foreign-relation slotd) 
	  (slot-definition-foreign-relation (car direct-slot-definitions))
	  (slot-definition-foreign-join-spec slotd) 
	  (slot-definition-foreign-join-spec (car direct-slot-definitions))
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

(defun find-foreign-relations (class object slotd)
  (when (slot-boundp object (dao-id-column-name class))
  (select-objects (slot-definition-foreign-relation slotd)
		  :where `(:= ,(or (slot-definition-foreign-join-spec slotd) 
				   (dao-id-column-name class))
			   ,(slot-value object (dao-id-column-name class))))))

(defmethod slot-boundp-using-class :around 
    ((class standard-db-access-class) object slotd)
  (let ((bound? (call-next-method)))
    (when (and (not bound?) (slot-definition-foreign-relation slotd))
	  (setf (slot-value-using-class class object slotd) 
		(find-foreign-relations class object slotd)))

    (call-next-method)))

(defmethod slot-value-using-class :around 
    ((class standard-db-access-class) object slotd)
  (if (slot-definition-foreign-relation slotd)
      (if (slot-boundp-using-class class object slotd)
	  (call-next-method)
	  (setf (slot-value-using-class class object slotd) 
		(find-foreign-relations class object slotd)))
      (call-next-method)))


 (defun dao-id-column-name (class)
  (slot-definition-column-name
   (or (class-id-slot-definition class)
       (error "No ID slot (primary key) for ~A" class))))

(defun primary-key-boundp (object)
  (slot-boundp object (dao-id-column-name (class-of object))))

(defclass described-db-access-class (described-class standard-db-access-class)
  ())

(defmethod initialize-instance :around ((class standard-db-access-class) &rest initargs &key name (direct-superclasses '()) direct-slots)
  (declare (dynamic-extent initargs))
  (let ((direct-slots (loop for slot in direct-slots 
			 collect (let* ((sname (getf slot :name))
					(readers (getf slot :readers))
					(writers (getf slot :writers)))
				   (setf (getf slot :readers)
					 (cons (intern (format nil "~A.~A"
							       name sname)) readers))
				   (setf (getf slot :writers)
					 (cons `(setf ,(intern (format nil "~A.~A"
								       name sname))) writers))
				   slot))))
	 
					      

    (if (loop for direct-superclass in direct-superclasses
	   thereis (ignore-errors (subtypep direct-superclass 'standard-db-access-object)))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (find-class 'standard-db-access-object)))
	       :direct-slots direct-slots
	       initargs))))

(defmethod reinitialize-instance :around ((class standard-db-access-class) 
					  &rest initargs 
					  &key (name (class-name class)) 
					  (direct-superclasses '() direct-superclasses-p) direct-slots)
  (declare (dynamic-extent initargs))
  (let ((direct-slots (loop for slot in direct-slots 
			 collect (let* ((sname (getf slot :name))
					(readers (getf slot :readers))
					(writers (getf slot :writers)))
				   (setf (getf slot :readers)
					 (cons (intern (format nil "~A.~A"
							       name sname)) readers))
				   (setf (getf slot :writers)
					 (cons `(setf ,(intern (format nil "~A.~A"
								       name sname))) writers))
				   slot))))
	 
					      

    (if (loop for direct-superclass in direct-superclasses
	   thereis (ignore-errors (subtypep direct-superclass 'standard-db-access-object)))
	(call-next-method)
	(apply #'call-next-method
	       class
	       :direct-superclasses
	       (append direct-superclasses
		       (list (find-class 'standard-db-access-object)))
	       :direct-slots direct-slots
	       initargs))))

(defclass standard-db-access-object (standard-object)
  ())

(defun %select-objects (type select-fn query)
  (mapcar (curry 'make-object-from-plist type)
	  (apply select-fn (intern (format nil "*")) 
		 (if (string-equal (first query) :from)
		     query
		     (append `(:from ,type) query)))))

(defun select-objects (type &rest query)
  (%select-objects type #'select query))

(defun select-only-n-objects (n type &rest query)
  (let ((fields (if (eq :fields (car query))
		    (loop 
		       :for cons :on (cdr query)
		       :if (not (keywordp (car cons)))
		       :collect (car cons) into fields
		       :else :do  
		         (setf query cons)
		         (return (nreverse (print fields)))
		       :finally 		       
		         (setf query cons)
		         (return (nreverse (print fields))))
		       
		    (list (intern "*")))))
    (let ((results 
	   (%query 
	    (print `(:limit (:select 
		      ,@fields 
		      ,@(if (string-equal (first query) :from)
			    (print query)
			    (append `(:from ,type) query)))
		     ,n)))))
    (if (eql 1 n)
	(make-object-from-plist type (first results))
	(mapcar (curry 'make-object-from-plist type) results)))))

(defun make-object-from-plist (type plist)
  (let* ((class (find-class type))
	 (object (make-instance class))
	 (slotds (class-slots class)))
	 
    (loop 
       :for (key val) :on plist :by #'cddr 
       :do 
       (dolist (slotd (remove key slotds 
			      :key #'slot-definition-column-name
			      :test-not #'string-equal))

	     (setf (slot-value-using-class class object slotd) val))
       :finally (return (reinitialize-instance object)))))

(defun make-object (type &rest plist)
  (make-object-from-plist type plist))

(defun insert-object (object)
  (let ((class (class-of object))
	insert-query)
    (flet ((ins (slotd &optional (val (slot-value-using-class class object slotd)))
	     (push (slot-definition-column-name slotd) insert-query)
	     (push  val insert-query)))
    (loop :for slotd in (class-slots class) 
	  :do (cond ((slot-boundp-using-class class object slotd)
		     (unless (or (slot-definition-foreign-relation slotd)
				 (slot-definition-foreign-type  slotd))
		       (ins slotd)))
		    ((slot-definition-primary-key-p slotd)
		     (setf (slot-value-using-class class object slotd) (get-default-value (class-table-name class)
						  (slot-definition-column-name slotd)))
		     (ins slotd ))))
    (apply #'insert-into (class-table-name class) (nreverse insert-query))))
  object)

(defun select-using-object (object &key (combinator :and))
  (let ((class (class-of object))
	select-query)
    (flet ((sel (slotd &optional (val (slot-value-using-class class object slotd)))
	     (push `(:ilike ,(slot-definition-column-name slotd) ,(if (stringp val)
								      (format nil "~A%" val) val)) select-query)))
    (loop :for slotd in (class-slots class) 
	  :do (cond ((slot-boundp-using-class class object slotd)
		     (unless (or (slot-definition-foreign-relation slotd)
				 (slot-definition-foreign-type  slotd))
		       (sel slotd)))))
    (if select-query
	   (select-objects (class-table-name class) 
	     :where (print `(,combinator ,@(nreverse select-query))))
	   nil))))
  

(defun get-default-value-query (table column)
  (format nil "select ~A " 
	  (second (select-only 1 ':adsrc 
			       :from 'pg_attribute 'pg_attrdef 
			       :where `(:and (:= adnum attnum) 
					(:= attname ,(s-sql::to-sql-name column)) 
					(:= adrelid attrelid) 
					(:= attrelid 
					 (:select oid 
					  :from pg_class 
					  :where (:= relname ,(s-sql::to-sql-name table)))))))))

(defun get-default-value (table column)
  (caar (query (get-default-value-query table column))))

(defun find-dao (type id 
		 &key (table (class-table-name (find-class type)))
		      id-column-name)
			     
  "Get the dao corresponding to the given primary key,
or return nil if it does not exist."
  (let ((plist 
	      (select-only 1 '* 
	       :from table 
               :where (list ':= id (or id-column-name
				 (dao-id-column-name 
				  (find-class type)))))))
    (make-object-from-plist type plist)))

(defmethod shared-initialize :after ((dao standard-db-access-object) 
				     slots &rest initargs)
  (let ((class (class-of dao))
	(foreign-key))
    (dolist (slotd (class-slots class))
      (with-slots (foreign-type) slotd
	(when foreign-type
	  (when (consp foreign-type)
	    (setf foreign-key (cdr foreign-type)
		  foreign-type (car foreign-type)))
	  (if (slot-boundp-using-class class dao slotd)
	      (let ((value (slot-value-using-class class dao slotd)))				(unless (typep value foreign-type)
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





