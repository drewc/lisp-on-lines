(defpackage #:lisp-on-lines
  (:use 
	#:common-lisp
	#:contextl)
  (:nicknames #:lol)

  (:export

   #:find-description
   #:ensure-description
   #:define-description

   #:define-display
   #:display
   #:*display*
   #:*object*
   
   #:find-attribute
   #:attribute-label

(cl:defpackage #:lol-test
  (:use #:cl #:lisp-on-lines #:stefil #:contextl))
