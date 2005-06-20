(defpackage :meta-model
  (:use :common-lisp :clsql)
  (:export
   :meta-model-class
   :meta-model.base-type
   :meta-model.instance
   :meta-model.metadata
   :def-meta-model
   :def-base-class
   :%def-base-class
   :def-view-class/table
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
   :explode-has-many))


(defpackage :mewa 
  (:use :ucw :common-lisp)
  (:export 
   :mewa 
   :mewa-object-presentation 
   :mewa-one-line-presentation 
   :find-attribute 
   :set-default-attributes 
   :make-presentation 
   :call-presentation 
   :label 
   :set-attribute 
   :find-class-attributes 
   :default-attributes 
   :ok
   :edit-instance
   :save-instance
   :cancel-save-instance
   :global-properties))


(defpackage :lisp-on-lines
  (:use :mewa :meta-model :common-lisp :it.bese.ucw))