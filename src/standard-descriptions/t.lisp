(in-package :lisp-on-lines)

(define-description T ()
  ((identity :label nil :function #'identity)
   (type :label "Type of" :function #'type-of)
   (class :label "Class" :function #'class-of)
   (active-attributes :label "Attributes"
		      :value nil
		      :activep nil
		      :keyword :attributes)))

(define-layered-method description-of (any-lisp-object)
  (find-description 't))

(define-layered-function display-attribute (object attribute)
  (:method (object attribute)
    (display-using-description attribute *display* object)))

(define-layered-function display-attribute-label (object attribute)
  (:method (object attribute)
        (format *display* "~A " (attribute-label attribute))))

(define-layered-function display-attribute-value (object attribute)
  (:method (object attribute)
    (let ((val (attribute-value attribute)))
      (if (eql val object)
	  (format *display* "~A " val)
	  (with-active-descriptions (inline)
	    (display *display* val))))))

(define-layered-method display-using-description 
  ((attribute standard-attribute) display object &rest args)
  (declare (ignore args))
  (when (attribute-label attribute)
    (display-attribute-label object attribute))
  (display-attribute-value object attribute))

(define-display ((description t))
  (format *display* "~{~A~%~}" 
	  (mapcar 
	   (lambda (attribute)
	     (with-output-to-string (*display*)
	       (display-attribute *object* attribute)))
	   (attributes description))))



