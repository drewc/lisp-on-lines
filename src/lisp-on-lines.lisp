(in-package :lisp-on-lines)

;;;; *LoL Entry points
;;;; 

;;;; This file contains the high level functions and macros 
;;;; that are part of LoL proper, that is to say, not Mewa 
;;;; or Meta-Model.

;;;; ** Initialisation
;;;; The following macros are used to initialise a set of database tables as LoL objects.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-initialize-lol-for-table (table)
    "
Generates a form that, when evaluated, initialises the given table as an lol object.
This involves creating a meta-model, a clsql view-class, and the setting up the default attributes for a mewa presentation"

    `(progn 
      (def-view-class-from-table ,table)
      (set-default-attributes (quote ,(meta-model::sql->sym table))))))
    
(defmacro initialize-lol-for-table (&rest tables)
  " expand to a form which initialises TABLES for use with LOL"
  `(progn
    ,@(loop for tbl in tables collect (generate-initialize-lol-for-table tbl))
    (values)))

(defmacro initialize-lol-for-database ()
  "expands to init-i-f-t using the listing of tables provided by meta-model"
  `(initialize-lol-for-table ,@(meta-model::list-tables)))

;;;; * AJAX stuff 

;;;; TODO: This search stuff should probably me refactored elsewhere

(defmethod find-slots-of-type (model &key (type 'string)
			      (types '((string)) types-supplied-p))
  "returns a list of slots matching TYPE, or matching any of TYPES"
  (let (ty)
    (if types-supplied-p 
	(setf ty types)
	(setf ty (list type)))
    (remove nil (mapcar #'(lambda (st) (when (member (second st) ty)
					 (first st)))
	     (lisp-on-lines::list-slot-types model)))))


(defmethod word-search (class-name slots search-terms 
			&key (limit 10) (where (sql-and t)))
  (select class-name 
	  :where (sql-and
		  where
		  (word-search-where class-name slots search-terms :format-string "~a%"))
			   :flatp t
			   :limit limit))


(defmethod word-search (class-name slots (s string) &rest args)
  (apply #'word-search class-name slots 
	 (split-sequence:split-sequence #\Space s) args))

(defmethod word-search-where (class-name slots search-terms &key (format-string "%~a%"))
  (sql-or 
   (mapcar #'(lambda (term)
	       (apply #'sql-or 
		      (mapcar #'(lambda (slot)  
				  (sql-uplike
				   (sql-slot-value class-name slot)
				   (format nil format-string  term)))
			      slots)))
	   search-terms)))

