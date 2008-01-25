(in-package :lisp-on-lines)

(define-description T ()
  ((identity :label nil :function #'identity)
   (type :label "Type" :function #'type-of)
   (class :label "Class" :function #'class-of)
   (active-attributes :label "Attributes"
		      :value nil
		      :activep nil
		      :keyword :attributes)))

(define-layered-method description-of (any-lisp-object)
  (find-description 't))

(define-display ((description t))
  (format *display* "~{~A~%~}" 
	  (mapcar 
	   (lambda (attribute)
	     (with-output-to-string (*display*)
	       (display-attribute *object* attribute)))
	   (attributes description))))



