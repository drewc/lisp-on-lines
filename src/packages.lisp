(defpackage :lisp-on-lines
  (:use :arnesi
	:iterate
	:meta-model
	:common-lisp
	:it.bese.ucw
	:clsql
	:contextl)
  (:nicknames :lol :mewa)
  
  (:shadowing-import-from
   :iterate
   :with)

  (:shadowing-import-from
   :clsql
   :time-difference
   :make-time
   :time-ymd
   :date
   :get-time
   :time-element
   :time+
   :date-element)

  (:shadow
   :present
   :present-slot
   :presentation
   :instance
   :slot-presentation
   :integer-slot-presentation
   :string-slot-presentation
   :object-presentation
   :one-line-presentation
   :presentation-slot-value
   :get-foreign-instances)
  
  (:export 
   ;;;; CLSQL meta-model/default attributes definers
   ;;;; TODO: should be moved to meta-model,
   ;;;; with lol specific function implemented like the
   ;;;; CLOS meta-model
   :define-view-for-table
   :define-views-for-database


   ;;;; The LoL 3 displays
   :defdisplay
   :defattribute
   :display
   :display-using-description
   :call-display


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

   :find-occurence

   ;;attributes
   :attributes
   :attribute-value
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
   :mewa-object-presentation
   :mewa-one-line-presentation
   :mewa-list-presentation
   :mewa-search-presentation
   :mewa-presentation-search

   :editablep
   :global-properties
   ;; SLOT presentations
  
   :mewa-relation-slot-presentation
   :mewa-string-slot-presentation
   :has-many-slot-presentation 
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