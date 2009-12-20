(in-package :lisp-on-lines)

(define-layered-class simple-plist-attribute ()
  (%property-access-function 
   (description-class :initarg description-class 
		      :accessor attribute-description-class))
  (:documentation "A very simple implementation of ATTRIBUTEs based on
  simple plists.

To implement layered slot values, we use an anonymous layered function
with a combination of APPEND. Methods on different layers return a
plist (which is APPENDed), from which we simply GETF for the slot
value.

This is ineffecient, of course, but is easy to understand. Caching and
performance hacks are implemented in subclasses that extend the simple
protocol we define here."))

(defstruct static-attribute-slot value)

(defmethod ensure-property-access-function ((attribute simple-plist-attribute))
  "return the PROPERTY-ACCESS-FUNCTION of this attribute.  FUNCALLing
the returned symbol will return the plist of slot values."
  (if (slot-boundp attribute '%property-access-function)
      (slot-value attribute '%property-access-function)
      (let ((fn-name (gensym))) 
	(ensure-layered-function fn-name :lambda-list '(description) :method-combination '(append))
	(setf (slot-value attribute '%property-access-function) fn-name))))

(defun property-access-value (attribute)
  (ignore-errors (funcall (ensure-property-access-function attribute) (attribute-description attribute))))

(defconstant +property-not-found+ '=lisp-on-lines-property-not-found-indicator= 
  "A default value for GETF to return.")

(defvar *special-symbol-access* nil)

(defun special-symbol-access-p ()
  *special-symbol-access*)

(defmacro with-special-symbol-access (&body body)
  `(let ((*special-symbol-access* t))
     ,@body))

(defmacro without-special-symbol-access (&body body)
  `(let ((*special-symbol-access* nil))
     ,@body))

(define-layered-method 
    contextl:slot-value-using-layer (class (attribute simple-plist-attribute) slotd reader) ()
    "Only layered slots that are not currently dynamically rebound are looked up via the plist.
Initial slot values are stored in the PLIST of the symbol ENSURE-PROPERTY-ACCESS-FUNCTION returns." 
 
     (if (or contextl:*symbol-access*  
	  (special-symbol-access-p)
	  (not (slot-definition-layeredp slotd)))
	 (call-next-method)
	 (multiple-value-bind (value boundp)
	     (handler-case (values (call-next-method) t) 
	       (unbound-slot () (values nil nil)))

	   (when (and boundp (not (static-attribute-slot-p value)))
	     (return-from slot-value-using-layer value))

	   (let ((dynamic-value 
		  (getf (ignore-errors  (funcall (ensure-property-access-function attribute) 
						     (find-layer (slot-value attribute 'description-class))))
			
			(slot-definition-name slotd)
			+property-not-found+)))
		 
	     (if (eq dynamic-value +property-not-found+)
		 (if boundp 
		     (static-attribute-slot-value value)
		     (call-next-method))
		 dynamic-value)))))

(defun set-property-value-for-layer (attribute property value layer)
   (let ((vals (property-access-value attribute)))
     (ensure-layered-method  
      (ensure-property-access-function attribute)
      `(lambda (description-class)
	 ',(append (list property value) (alexandria:remove-from-plist vals property)))
      :specializers (list (class-of (attribute-description attribute)))
      :qualifiers '(append)
      :in-layer layer)))

(define-layered-method 
    (setf contextl:slot-value-using-layer) :around (value class (attribute simple-plist-attribute) slotd writer)
"This might not be here"
  (if (and (not contextl:*symbol-access*)
	    (not (special-symbol-access-p))
	    (slot-definition-layeredp slotd)) 
      (with-special-symbol-access  (setf (slot-value-using-layer class attribute slotd writer) (make-static-attribute-slot :value value)))
      (call-next-method))
)

(defmethod initialize-attribute-for-description (description-class (attribute simple-plist-attribute) layer-name &rest args)
  "Define a method on the PROPERTY-ACCESS-FUNCTION to associate
slots (named by their :initarg) with values in layer LAYER-NAME."
  (let* ((class (class-of attribute))
	 (slotds (class-slots class)))    
    (setf (slot-value attribute 'description-class) description-class)
    (ensure-layered-method  
     (ensure-property-access-function attribute)
     `(lambda (description-class)
	',(alexandria:remove-from-plist  
	   (loop 
	      :for (key val) :on args :by #'cddr 
	      :nconc (list 
		      (loop 
			 :for slotd :in slotds 
			 :do (when (find key (slot-definition-initargs slotd))
			       (return  (slot-definition-name slotd))))
		      val))
	   nil)) 
     :specializers (list description-class)
     :qualifiers '(append)
     :in-layer layer-name)))

