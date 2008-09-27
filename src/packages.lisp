(defpackage #:lisp-on-lines
  (:use 
   :common-lisp
   #:contextl
   #:closer-mop
   #:postmodern
   #:alexandria)
  (:nicknames #:lol)
  (:export

;; ROFL stuff here temporarily
   #:standard-db-access-class
   #:standard-db-access-object
   #:make-object-from-plist
   #:described-db-access-class
   #:select-only
   #:select
   #:insert-into   
   #:select-objects
   #:select-only-n-objects
   #:insert-object
   #:primary-key-boundp
   
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

   ;; html
   #:display-html-attribute-editor
   #:make-attribute-value-writer))


