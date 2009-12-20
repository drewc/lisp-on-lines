(defpackage #:lisp-on-lines
  (:use 
   :common-lisp
   #:contextl
   #:closer-mop

   #:alexandria)
  (:nicknames #:lol)
  (:export

   
;; Descriptions
   #:*description*
   #:description
   #:defdescription
   #:find-description   
   #:current-description
   #:description-of
   #:define-description
   #:defining-description
   #:described-object
   #:with-described-object
   #:funcall-with-described-object
   #:described-class
   #:described-standard-class
   #:with-active-descriptions
   #:with-inactive-descriptions


   ;; Displays
   #:define-display
   #:display
   #:display-using-description
   #:display-attribute-label
   #:*display*
   #:*object*
   
   ;; Attributes
   #:find-attribute
   #:attribute
   #:attributes
   #:attribute-object
   #:attribute-label
   #:attribute-delimiter
   #:attribute-slot-name
   #:label
   #:attribute-active-p
   #:attribute-function
   #:attribute-value
   #:display-attribute-value
   #:active-attributes
   #:attribute-delimiter
   #:standard-attribute
   #:funcall-with-attribute-context
   #:with-attribute-context

   ;; Standard Library
   
   ;; editing
   #:editable
   #:attribute-editor
   #:string-attribute-editor
   #:number-attribute-editor
   #:password-attribute-editor
   #:password

   ;; :validation
   #:validation
   #:validate
   #:validp

   ;; CLOS
   #:slot-definition-attribute

   ;; html
   #:display-html-attribute-editor
   #:make-attribute-value-writer))


