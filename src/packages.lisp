(defpackage #:lisp-on-lines
  (:use 
   :common-lisp
   #:contextl)
  (:nicknames #:lol)
  (:export
   
;; Descriptions
   #:find-description
   #:define-description
   
   ;; Displays
   #:define-display
   #:display
   #:display-using-description
   #:*display*
   #:*object*
   
   ;; Attributes
   #:find-attribute
   #:attribute-label
   #:attribute-function
   #:attribute-value))


