(in-package :lisp-on-lines)

(defstruct invalid-attribute-value
  (initial-value nil :read-only t)
  (invalid-value nil :read-only t)
  (conditions nil :read-only t))

(deflayer validate (editor))

(defattribute base-attribute ()
  ()
  (:in-layer validate)
  (:default-properties
      :validate-using nil
    :requiredp nil
    :required-test 'validate-true))

(defparameter *invalid-attribute-renderer*
  #'(lambda (invalid-attribute-value next-method)
      (<:div
	    :class "lol-invalid-attribute"
	    (<:ul
	     :class "lol-invalid-attribute-message"
	     (dolist (c (invalid-attribute-value-conditions invalid-attribute-value))
	       (<:li (<:as-html (message c)))))
	    (funcall next-method))))

(defdisplay :in-layer validate
	    :around ((attribute base-attribute) object)
  "Set the callback to perform validation 
and create invalid-attribute-values when things don't validate. duh "
  (let ((callback (or
		   (callback attribute)
		   (ucw::make-new-callback
		    #'(lambda (val)
			(setf (attribute-value object attribute) val)))))
	;;;; We need to lexically bind some slots here
	;;;; As by the time the validator runs we'll be in
	;;;; a totally different dynamic scope.
	(validators (validate-using attribute))
	(label (label attribute)))
    (when (requiredp attribute)
      (push (required-test attribute) validators))
   (dletf (((callback attribute)
	    (ucw::make-new-callback
	     #'(lambda (val)
		 (flet ((setter (value)
			      (ucw::call-callback
			       (ucw::context.current-frame *context*)
			       callback
			       value)))
		   
		 ;; We have to do DLETF ,as we will no longer be
		 ;; in the validation layer at callback-application time. 
		   (dletf (((validate-using attribute) validators)
			   ((slot-value attribute 'label) label)
			   )
		     (multiple-value-bind (validp conditions)
			 (validate-attribute object attribute val)
		       (if validp
			   (setter val)
			   (setter
			    (make-invalid-attribute-value
			     :initial-value (attribute-value object attribute)
			     :invalid-value val
			     :conditions conditions))))))))))


     ;;;; Ok, so if the attribute-value holds an
     ;;;; invalid-attribute-value struct, we take the appropriate action
     (let ((value (attribute-value object attribute)))
       (if (invalid-attribute-value-p value)
	   (progn
	     ;;;; set the value back the the previous
	     ;;;; TODO: does not handle unbound slots
	     (ucw::call-callback
	      (ucw::context.current-frame *context*)
	      callback
	      (invalid-attribute-value-initial-value value))
	     (funcall *invalid-attribute-renderer*
		      value
		      #'(lambda ()
			  (call-next-method))))
	   (call-next-method))))))