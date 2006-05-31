(in-package :lisp-on-lines)

(defstruct invalid-attribute-value
  (initial-value nil :read-only t)
  (invalid-value nil :read-only t)
  (conditions nil :read-only t))

(deflayer validate (editor))

(defcomponent validation-mixin ()
  ((validation-conditions
    :accessor validation-conditions
    :initform nil
    :backtrack t)
   (inhibit-call-if-invalid-p
    :accessor inhibit-call-if-invalid-p
    :initform t
    :initarg :inhibit-call-if-invalid-p)
   (inhibit-answer-if-invalid-p
    :accessor inhibit-answer-if-invalid-p
    :initform t
    :initarg :inhibit-answer-if-invalid-p)))

(defmethod render :wrap-around ((self validation-mixin))
  (call-next-method)
  (setf (validation-conditions self) nil))

(defun component-valid-p (component)
  (not (validation-conditions component)))

(defmethod/cc call-component :around ((from validation-mixin) to)
   (if (inhibit-call-if-invalid-p from)
      (when (component-valid-p from)
	(call-next-method from to))
      (call-next-method from to)))

(defmethod answer-component :around ((target validation-mixin) value)
  (if (inhibit-answer-if-invalid-p target)
      (when (component-valid-p target)
	(call-next-method))
      (call-next-method)))

(defparameter *invalid-attribute-renderer*
  #'(lambda (invalid-attribute-value next-method)
      (<:div
	    :class "lol-invalid-attribute"
	    (<:ul
	     :class "lol-invalid-attribute-message"
	     (dolist (c (invalid-attribute-value-conditions invalid-attribute-value))
	       (<:li (<:as-html (message c)))))
	    (funcall next-method))))

(defattribute string-attribute ()
  ()
  (:in-layer validate)
  (:default-properties
      :validate-using nil
    :requiredp nil
    :required-test 'validate-string-exists))

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
    
    ;;;; We have a convenience property for :requiredp
    (when (requiredp attribute)
      (push (required-test attribute) validators))

    ;;;; Now we create the validation callback
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
			   ((slot-value attribute 'label) label))
		     (multiple-value-bind (validp conditions)
			 (validate-attribute object attribute val)
		       (if validp
			   (setter val)
			   (progn
			     (setter
			      (make-invalid-attribute-value
			       :initial-value (attribute-value object attribute)
			       :invalid-value val
			       :conditions conditions))
			     (when (subtypep (type-of self) 'validation-mixin)
			       (setf (validation-conditions self)
				     (append conditions (validation-conditions self)))))))))))))


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