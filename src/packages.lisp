(defpackage :meta-model
  (:use :common-lisp :clsql :clsql-pg-introspect)
  (:shadow :primary-key-p)
  (:export
   :meta-model-class
   :meta-model.base-type
   :meta-model.instance
   :meta-model.metadata
   :define-meta-model
   :def-view-class-from-table
   :def-view-class/meta
   :view-class-metadata
   :create-table-from-model
   :list-slots
   :list-slot-types
   :slot-type
   :display-slot
   :list-joins
   :list-join-attributes
   :list-keys
   :list-view-classes
   :display-slot
   :primary-key-p
   :list-foreign-keys
   :foreign-key-p
   :explode-foreign-key
   :find-join-class
   :find-join-key
   :find-default-value
   :explode-foreign-key
   :list-has-many
   :list-many-to-many
   :sync-instance
   :explode-has-many
   :expr-ends-with
   :expr-starts-with
   :expr-contains
   :expr-=
   :expr-<
   :expr->
   :expr-and
   :expr-or
   :expr-not
   :select-instances))


(defpackage :mewa 
  (:use :ucw :common-lisp)
  (:export 
   :mewa 
   :mewa-object-presentation 
   :mewa-one-line-presentation
   :mewa-list-presentation
   :mewa-presentation-search
   :find-attribute 
   :set-default-attributes 
   :make-presentation 
   :call-presentation 
   :label
   :attributes
   :set-attribute
   :perform-set-attributes
   :perform-set-attribute-properties
   :find-class-attributes 
   :default-attributes 
   :ok
   :edit-instance
   :save-instance
   :cancel-save-instance
   :ensure-instance-sync
   :instance-is-stored-p
   :global-properties
   :search-expr
   :search-query))

(defpackage :lisp-on-lines
  (:use :mewa :meta-model :common-lisp :it.bese.ucw :js)
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