(defpackage :lisp-on-lines
  (:use :mewa :meta-model :common-lisp :it.bese.ucw :js :clsql)
  (:nicknames :lol)
  (:export 
   ;;;; LoL 
   :initialize-lol-for-table
   :initialize-lol-for-database

   ;;;; Ajax
   :auto-complete
   :call-auto-complete

   ;;;; Mewa Exports
   :mewa ;the superclass of all mewa-presentations
   :make-presentation
   :call-presentation
   ;;attributes
   :attributes
   :set-default-attributes
   :set-attribute
   :find-attribute
   :perform-set-attributes
   ;;
   :perform-set-attribute-properties

   ;; presentation objects
   :mewa-object-presentation
   :mewa-one-line-presentation
   :mewa-list-presentation

   ;; CRUD
   :instance-is-stored-p

   ;;;; Meta Model Exports))
   :define-meta-model
   :def-view-class-from-table
   :def-view-class/meta
   :list-slot-types
   ))