(in-package :lisp-on-lines)

(define-description T ()
  ((label :label nil 
	  :function (lambda (object)
		      (format nil "~@(~A~)" 
			      (substitute #\Space #\- 
					  (symbol-name 
					   (class-name (class-of 
							object)))))))
   (identity :label nil :function #'identity)
   (type :label "Type" :function #'type-of)
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
   (label-formatter :value (lambda (label)
			     (generic-format *display* "~A:" label))
		    :activep nil)
   (value-formatter :value (curry #'format nil "~A")
		    :activep nil)))

(define-layered-method description-of (any-lisp-object)
  (find-description 't))

(define-layered-function display-attribute (attribute)
  (:method (attribute)
    (display-using-description attribute *display* (attribute-object attribute))))


(define-layered-function display-attribute-label (attribute)
  (:method (attribute)
    (funcall (attribute-label-formatter attribute) (attribute-label attribute))))
	   
(define-layered-function display-attribute-value (attribute)
  (:method (attribute)
    (flet ((disp (val &rest args)
	     (apply #'display *display* val 
		    :activate (attribute-active-descriptions attribute)
		    :deactivate (attribute-inactive-descriptions attribute)
		    args)))


    (let ((val (attribute-value attribute)))
#+nil            (break "display Attribute value: ~A with object ~A ~% Description ~A att-d ~A ~% VALUE ~A display on ~A" 
		   	 attribute 
			 (attribute-object attribute)
			 *description*
			 (attribute-description attribute)
			 val
			 *display*
			 )
      (if (and (not (slot-boundp attribute 'active-attributes))
	       (equal val (attribute-object attribute)))
	  (progn (generic-format *display* "~A"(funcall (attribute-value-formatter attribute) val))		 
		 #+nil(break "using generic format because val is object and there is no active attributes."))
	  
	  (with-active-descriptions (inline)
	    (cond ((slot-value attribute 'value-formatter)
		   (generic-format *display* "~A"(funcall (attribute-value-formatter attribute) val)))
		   ((slot-boundp attribute 'active-attributes)
		    (disp val :attributes (slot-value attribute 'active-attributes)))
		   (t
		    (disp val)))))))))

(define-layered-method display-using-description 
  ((attribute standard-attribute) display object &rest args)
  (declare (ignore args))
  (when (attribute-label attribute)
    (display-attribute-label attribute))
  (display-attribute-value attribute))

(define-layered-method display-attribute :around
  ((attribute standard-attribute))
    (funcall-with-layer-context 
   (modify-layer-context (current-layer-context) 
			 :activate (attribute-active-descriptions attribute)
			 :deactivate (attribute-inactive-descriptions attribute))
   (lambda () 
     (call-next-method))))

(define-layered-method display-attribute :before
  ((attribute standard-attribute))
#+nil  (break "Attribute : ~A with object ~A ~% Description ~A att-d ~A" 
	 attribute 
	 (attribute-object attribute)
	 *description*
	 (attribute-description attribute)
))

(define-display ((description t))
 (let ((attributes (attributes description)))
   (display-attribute (first attributes))
   (dolist (attribute (rest attributes) (values))
     (generic-format *display* 
      (attribute-value 
       (find-attribute description 'attribute-delimiter)))
     (display-attribute attribute))))
  

(define-display :around ((description t) (display null) object)
 (with-output-to-string (*standard-output*)
   (call-next-layered-method description t object))
)		




