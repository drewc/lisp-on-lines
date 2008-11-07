(defpackage #:lisp-on-lines
  (:use 
   :common-lisp
   #:contextl
   #:closer-mop

   #:alexandria)
  (:nicknames #:lol)
  (:export

   
;; Descriptions
   #:find-description
   #:description-of
   #:define-description
   #:described-object
   #:described-class
   #:described-standard-class
   #:with-active-descriptions
   #:with-inactive-descriptions


   ;; Displays
   #:define-display
   #:display
   #:display-using-description
   #:*display*
   #:*object*
   
   ;; Attributes
   #:find-attribute
   #:attribute
   #:attributes
   #:attribute-object
   #:attribute-label
   #:label
   #:attribute-function
   #:attribute-value
   #:display-attribute-value
   #:active-attributes
   #:attribute-delimiter
   #:standard-attribute

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


