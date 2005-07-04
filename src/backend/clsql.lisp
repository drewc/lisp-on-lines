(in-package :meta-model)

(eval-when (:compile-toplevel :load-toplevel :execute)
 (use-package :clsql))

(export 'def-meta-model-from-table)
(export 'def-meta-models)
(export 'def-view-class/meta)
(export 'list-base-classes)



(defmethod sync-instance ((view clsql:standard-db-object) &key (fill-gaps-only nil) (database *default-database*))
  (labels ((sym->sql (sym) (string-downcase (substitute #\_ #\- (string sym))))
           (get-def (slot) (caar (query
                                  (format nil                                                             "SELECT DISTINCT adsrc from pg_attrdef join pg_attribute on attnum = adnum where adrelid = (select oid from pg_class where relname = '~A') and attname = '~A'" (sym->sql (class-name (class-of view))) (sym->sql slot)))))
           (get-default-value (slot) 
	     (let ((def (get-def slot)))
	       (if def
		   (caar (query (format nil "SELECT ~A" def)))))))

    (dolist (slot (list-slots view))
      (when (and (primary-key-p view slot)
                 (or (not (slot-boundp view slot))
                     (equal (slot-value view slot) nil)))
        (setf (slot-value view slot) (get-default-value slot))
        (when (and (primary-key-p view slot)
                   (not (slot-value view slot))
                   (not  fill-gaps-only))
          (error "No default value for primary key : ~A" slot))))
    (when fill-gaps-only
      (update-objects-joins (list view))
      (return-from sync-instance))
    (update-records-from-instance view :database database)
    (update-instance-from-records view :database database)
    (update-objects-joins (list view))))



(defparameter *clsql-base-classes* (list) )

(defmethod list-base-classes ((type (eql :clsql)))
  *clsql-base-classes*)

(defmethod def-base-type-class-expander ((base-type (eql :clsql)) (model meta-model-class) (name t) (args t))
  `(def-view-class ,name () 
		   ,(meta-model.metadata model)))

(defmethod def-base-type-class-expander :after ((base-type (eql :clsql)) (model meta-model-class) (name t) (args t))
  (unless (member name *clsql-base-classes*)
    (setf *clsql-base-classes* (cons name *clsql-base-classes*))))

(defparameter *sql-type-map* '((:INT4 integer) (:TEXT string) (:VARCHAR string) (:TIMESTAMP clsql-sys::wall-time) (:NUMERIC number)(:BYTEA string)))

(defun gen-type (table column)
  (cadr (assoc
	 (cadr (assoc
		column
		(list-attribute-types table)
		:test #'equalp ))
	 *sql-type-map*)))

(defun sql->sym (name &optional (package nil)) 
  (flet ((xform (x)
	   (string-upcase (substitute #\- #\_ x))))
    (if package
	(intern (xform (string name)) package)
	(intern (xform (string name))))))

(defun table->slots (table pkey &optional (accesor-prefix table) (prefix-all-p nil))
  (mapcar
   #'(lambda (col)
       (flet ((accessor-name (col)
                (let ((name (sql->sym col)))
                  (if (or prefix-all-p
                          (and (fboundp name)
                               (eq (type-of (symbol-function name)) 'function)))
                      (sql->sym (concatenate 'string
                                             (string accesor-prefix) "-" col))
                      name))))

         `(,(sql->sym col)
            :accessor ,(accessor-name col)
            :initarg ,(sql->sym col "KEYWORD")
            :type ,(gen-type table col)
            :db-kind
            ,(if (equalp col pkey)
                 `:key
                 `:base))))
   (list-attributes table)))

(defun view-class-definition-list ()
  (mapcar #'(lambda (x) `(def-meta-model-from-table ,x))
		(list-tables)))

(defmacro def-meta-models ()
  (let ((defs (view-class-definition-list)))
    `(progn ,@defs)))


(defun get-pkeys ()
  (let ((keys '()))
    (dolist (row (get-pkeys-query))
      (setf keys (acons (car row) (list (cadr row)) keys)))
    keys))
    
(defun get-pkeys-query()
  (query
		    "SELECT  pg_class.relname, pg_attribute.attname, pg_catalog.quote_ident(conname) AS constraint_n
                 , pg_catalog.pg_get_indexdef(d.objid) AS constraint_definition
                 , CASE
                   WHEN contype = 'p' THEN
                         'PRIMARY KEY'
                   ELSE
                         'UNIQUE'
                   END as constraint_type
          FROM 
                pg_class, pg_attribute,
          pg_catalog.pg_constraint AS c
          JOIN pg_catalog.pg_depend AS d ON (d.refobjid = c.oid)
         WHERE contype IN ('p', 'u')
           AND deptype = 'i'
           and conrelid = pg_class.oid 
            and pg_attribute.attnum = ANY (c.conkey)
             and pg_attribute.attrelid = pg_class.oid"))

;;here is how this works
;;from the postgres system tables we get
;;list of all the has-a relationships.
;;the inverse of a has-a is an implicit has-many
;;and any relation having more than one foreign key
;;is a join table hosting a many-to-many relationship

(defun get-fkey-explosions ()
  (let ((key-table (get-fkey-explosions-query))
	(keys '()))
    (dolist (row key-table)
      (setf row (mapcar #'(lambda (x)
			    (sql->sym x))
			row))
      ;;this one does the has-a 
      (setf keys (acons (car row) (gen-has-a row)
			keys))
      ;;the inverse of the previous represents a has-many.
      (setf keys
	    (acons (fourth row) (gen-has-many row)
		   keys))
      
      ;;many-to-many
      (dolist (mrow
		(remove-if #'(lambda (r) (or (not (equal (car row) (car r)))
					     (equal (last row) (last r))))
			   (mapcar #'(lambda (x)
				       (mapcar #'sql->sym x))
				   key-table)))
	(setf keys (acons (fourth row)
			  (gen-many-to-many mrow (third row) (second row))
			  keys))))
    keys ))
			  
      
(defun get-fkey-explosions-query ()
;;these query's are a mess, i don't even know how they work :)
  (query "
SELECT pg_class.relname,
       pg_attribute.attname,
       fa.attname  ,
       f.relname
FROM   pg_class,
       pg_constraint,
       pg_attribute,
       pg_class as f  ,
       pg_attribute as fa
WHERE pg_class.relname in (select tablename from pg_tables where schemaname = 'public')
AND pg_class.oid = pg_constraint.conrelid
AND pg_attribute.attnum = ANY (pg_constraint.conkey)
AND pg_attribute.attrelid = pg_class.oid
AND f.oid = confrelid
AND fa.attrelid = f.oid
AND fa.attnum = ANY (pg_constraint.confkey)"))


;; i chose keyword args here so as to make the code more understandable.
;; it didn't really work.
(defun gen-join-slot (&key name home-key foreign-key join-class (set nil))
  `(,(intern name)
    :accessor ,(intern name)
    :db-kind :join
    :db-info (:join-class ,join-class
	      :home-key ,home-key
	      :foreign-key ,foreign-key
	      :set ,set)))

(defun gen-has-a (row)
  (gen-join-slot
   :name
   (format nil "~A->~A" (string (car row))(string (second row)))
   :home-key (second row)
   :foreign-key (third row)
   :join-class (fourth row)))

(defun gen-has-many (row)
  (gen-join-slot
   :name
   (format nil "~A->~A" (string (car row))(string (second row)))
   :home-key (third row)
   :foreign-key (second row)
   :join-class (car row)
   :set t))

(defun gen-many-to-many (row home-key foreign-key)
 (let ((name (sql->sym (string-upcase (format nil "~A->~A" (string (car row)) (string (second row)))))))
   (setf row (mapcar #'sql->sym row))
   `(,name
     :accessor ,name
     :db-kind :join
     :db-info (:join-class ,(car row)
	       :home-key ,home-key
	       :foreign-key ,foreign-key
	       :target-slot ,name
	       :set t))))
 
(defmethod update-records-from-instance :before ((view clsql::standard-db-object) &key database)
  (declare (ignorable database))
  (labels ((sym->sql (sym) (string-downcase (substitute #\_ #\- (string sym))))
	   (get-def (slot) (caar (query
				  (format nil								  "SELECT DISTINCT adsrc from pg_attrdef join pg_attribute on attnum = adnum where adrelid = (select oid from pg_class where relname = '~A') and attname = '~A'" (sym->sql (class-name (class-of view))) (sym->sql slot)))))
	   (get-default-value (slot) (caar (query (format nil "SELECT ~A" (get-def slot))))))

    (dolist (slot (list-slots view))
      (when (and (primary-key-p view slot)
		 (or (not (slot-boundp view slot))
		     (equal (slot-value view slot) nil)))
	(setf (slot-value view slot) (get-default-value slot))))))

;;;;

(defmacro def-view-class/meta (name supers slots &rest args)
  "Create and instrument CLSQL view-class NAME and
appropriate meta-model class its default name is %NAME-meta-model."

  (let ((model-name (cond ((eq :model-name (car args))
                           (pop args)	; remove keyword
                           (pop args))	; get value
                          (t (intern (format nil "%~S-META-MODEL" name))))))

    `(progn
      (let* ((m (def-meta-model ,model-name ,supers ,slots ,args))
	     (i (make-instance m)))
	(setf (meta-model.base-type i) :clsql)
	(prog1 (eval (def-base-class-expander i ',name ',args))
	  (defmethod meta-model.metadata ((self ,name))
	    (meta-model.metadata i)))))))

(defmacro def-view-class/table (table &optional (name (sql->sym table)) model-name)
  "takes the name of a table as a string and
creates a clsql view-class"
  (let* ((pkey (cadr (assoc table (get-pkeys) :test #'equalp)))
	 (table-slots (table->slots table pkey name))
	 (join-slots
	  (let ((slots nil))
	    (dolist (exp (get-fkey-explosions))
	      (when (equalp (car exp) (sql->sym table))
		(setf slots (cons (cdr exp) slots))))
	    slots)))
    `(def-view-class/meta ,name
         ()
       ,(append table-slots join-slots)
       ,@(when model-name (list :model-name model-name)))))

(def-compare-expr standard-db-object expr-= sql-=)
(def-compare-expr standard-db-object expr-< sql-<)        
(def-compare-expr standard-db-object expr-> sql->)
(def-compare-expr standard-db-object expr-ends-with sql-like :value-format "%~A")
(def-compare-expr standard-db-object expr-starts-with sql-like :value-format "~A%")
(def-compare-expr standard-db-object expr-contains sql-like :value-format "%~A%")

(def-logical-expr standard-db-object expr-and #'sql-and)

(def-logical-expr standard-db-object expr-or #'sql-or)

(def-logical-expr standard-db-object expr-not #'sql-not)

(defmethod select-instances ((instance standard-db-object) &rest query)
  (unless (keywordp (car query))
    (setf query (cons :where query)))
  (apply #'select (class-name (class-of instance)) :flatp t query))
