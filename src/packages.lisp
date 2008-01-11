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
   #:define-description
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


