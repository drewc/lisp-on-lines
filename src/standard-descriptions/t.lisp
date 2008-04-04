(in-package :lisp-on-lines)

(define-description T ()
  ((identity :label nil :function #'identity)
   (type :label "Type of" :function #'type-of)
   (class :label "Class" :function #'class-of)
   (active-attributes :label "Attributes"
		      :value nil
		      :activep nil
		      :keyword :attributes)
   (attribute-delimiter :label "Attribute Delimiter"
			:value "~%"
			:activep nil
			:keyword :delimter)
   (active-descriptions :label "Active Descriptions"
			:value nil
			:activep nil
			:keyword :activate)
   (inactive-descriptions :label "Inactive Descriptions"
			:value nil
			:activep nil
			:keyword :deactivate)
   (label-formatter :value (curry #'format nil "~A "))
   (value-formatter :value (curry #'format nil "~A"))))

(define-layered-method description-of (any-lisp-object)
  (find-description 't))

(define-layered-function display-attribute (attribute)
  (:method (attribute)
    (display-using-description attribute *display* (attribute-object attribute))))

(define-layered-function display-attribute-label (attribute)
  (:method (attribute)
    (princ (funcall (attribute-label-formatter attribute) (attribute-label attribute))
	   *display*)))


(define-layered-function display-attribute-value (attribute)
  (:method (attribute)
    (flet ((disp (val &rest args)
	     (apply #'display *display* val 
		    :activate (attribute-active-descriptions attribute)
		    :deactivate (attribute-inactive-descriptions attribute)
		    args)))
	     
    (let ((val (attribute-value attribute)))
      (if (eql val (attribute-object attribute))
	  (generic-format *display* (funcall (attribute-value-formatter attribute) val))
	  (with-active-descriptions (inline)
	    (if (slot-boundp attribute 'active-attributes)
		(disp val :attributes (slot-value attribute 'active-attributes))
		(disp val))))))))

(define-layered-method display-using-description 
  ((attribute standard-attribute) display object &rest args)
  (declare (ignore args))
  (when (attribute-label attribute)
    (display-attribute-label attribute))
  (display-attribute-value attribute))

(define-display ((description t))
 (let ((attributes (attributes description)))
   (display-attribute (first attributes))
   (dolist (attribute (rest attributes))
     (generic-format *display* 
      (attribute-value 
       (find-attribute description 'attribute-delimiter)))
     (display-attribute attribute))))
  

(define-display :around ((description t) (display null))
 (with-output-to-string (*display*) 
   (print (call-next-method) *display*)))		




