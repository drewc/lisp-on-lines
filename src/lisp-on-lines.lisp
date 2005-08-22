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
      (def-view-class/table ,table)
      (set-default-attributes (quote ,(meta-model::sql->sym table))))))
    
(defmacro initialize-lol-for-table (&rest tables)
  " expand to a form which initialises TABLES for use with LOL"
  `(progn
    ,@(loop for tbl in tables collect (generate-initialize-lol-for-table tbl))
    (values)))

(defmacro initialize-lol-for-database ()
  "expands to init-i-f-t using the listing of tables provided by meta-model"
  `(initialize-lol-for-table ,@(meta-model::list-tables)))