(in-package :lisp-on-lines)

(define-description inline ())

(define-description t ()
  ((identity :label nil)
   (active-attributes :value (identity)))
  (:in-description inline))

(define-display :in-description inline ((description t))
  (format *display* "~{~A ~}" 
	  (mapcar 
	   (lambda (attribute)
	     (with-output-to-string (*display*)
	       (display-attribute *object* attribute)))
	   (attributes description))))
