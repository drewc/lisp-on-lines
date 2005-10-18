(defpackage :mewa 
  (:use :ucw :common-lisp :arnesi :iterate)
  (:export 
   :mewa 
   :editablep
   
   ;object presentations
   :present
   :foreign-key-slot-presentation
   :mewa-object-presentation 
   :mewa-one-line-presentation
   :mewa-list-presentation
   :mewa-presentation-search
   
   ;;Slot Presentations
   :defslot-presentation
   :slot-presentation
   :mewa-slot-presentation

   :present-slot
   :presentation-slot-value
   

   :mewa-relation-slot-presentation
   :has-a-slot-presentation
   :has-a
   :has-many-slot-presentation
   :has-many
   :has-very-many-slot-presentation
   :has-very-many
   :slot-name

   ;attributes
   :define-attributes
   :with-default-attributes
   :find-attribute
   :find-attribute-slot
   :set-default-attributes 
   :make-presentation 
   :call-presentation 
   :label
   :attributes
   :set-attribute
   :set-attribute-properties
   :perform-set-attributes
   :perform-set-attribute-properties
   :find-class-attributes 
   :default-attributes 
   :ok
   :instance
   :edit-instance
   :save-instance
   :cancel-save-instance
   :ensure-instance-sync
   :instance-is-stored-p
   :global-properties
   :search-expr
   :search-query))

