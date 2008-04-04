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
   #:make-object-from-plist
   #:described-db-access-class
   #:select-only
   #:select
   #:insert-into   
   #:select-objects
   #:select-only-n-objects
   
;; Descriptions
   #:find-description
   #:description-of
   #:define-description
   #:described-object
   #:described-class
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
   #:attribute-label
   #:attribute-function
   #:attribute-value
   #:active-attributes))


