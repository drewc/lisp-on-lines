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
   #:make-dao-from-row
   #:described-db-access-class

;; Descriptions
   #:find-description
   #:define-description
   #:described-class
   #:with-active-descriptions

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
   #:attribute-value))


