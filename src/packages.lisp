(defpackage :lisp-on-lines
  (:use :arnesi
	:iterate
	:meta-model
	:common-lisp
	:it.bese.ucw
	:clsql
	:contextl)
  (:shadowing-import-from
   :iterate
   :with)
  (:nicknames :lol :mewa)
  (:export 
   ;;;; LoL 
   :define-view-for-table
   :define-views-for-database

   ;;;;a wrapper for calling make-presentation
   :call-view
   :present-view
   :slot-view
   :present-slot-view
   :make-view
   ;;;; Ajax
   :auto-complete
   :call-auto-complete

   ;;;; Mewa Exports
   :mewa ;the superclass of all mewa-presentations
   :make-presentation
   :call-presentation

   ;;attributes
   :attributes
   :define-attributes
   :with-default-attributes
   :set-default-attributes
   :set-attribute
   :find-attribute
   :perform-set-attributes
   ;;
   :perform-set-attribute-properties
   :define-attributes

   ;; presentation objects
   :present
   :instance
   :mewa-object-presentation
   :mewa-one-line-presentation
   :mewa-list-presentation
   :mewa-search-presentation
   :mewa-presentation-search

   :editablep
   :global-properties
   ;; SLOT presentations
   :defslot-presentation
   :slot-name
   :mewa-relation-slot-presentation
   :mewa-string-slot-presentation
   :has-many-slot-presentation
   :present-slot

   :has-a
   :has-many
   :has-very-many
   :many-to-many
   
   ;; CRUD
   :instance-is-stored-p

   ;;;; Meta Model Exports))
   :define-meta-model
   :def-view-class-from-table
   :def-view-class/meta
   :list-slot-types
   ))