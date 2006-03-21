(in-package :lisp-on-lines)

;;;; LoL CLOS Tests
;;;; 
(defclass lol-test-class ()
  ((test-slot-value :initform "slot-value")
   (test-string :initform "Test String"))
  (:documentation "foo"))

(defvar *foo* nil)

(defvar *standard-layers* '(viewer editor creator one-line as-string))

(define-attributes (lol-test-class)
  (test-getter t
	       :label "Getter"
	       :getter (constantly "Hello World"))
  (test-getter/setter t
	       :label "Getter/Setter:"
	       :getter (lambda ()
			 *foo*)
	       :setter #'(lambda (value)
			   (setf *foo* value)))
  (test-slot-value t)
  (test-string string :label "String" :documentation))
  
(defcomponent test-component ()
   (current-layer :accessor current-type :initform 'viewer)
   (layer-spec :accessor layer-spec :initform nil)
   (instance :accessor instance :initform (make-instance 'test-class))))

(defmethod render ((self test-component))
  (let ((test (instance self))) 
    (<:h1 (<:as-html "Lisp on Lines Test Component"))
    (with-component (self)
      (<ucw:form
       :action (refresh-component self)
       (<ucw:select :accessor (current-layer self)
		    (dolist* (type (display-types self))
		      (<ucw:option :value type (<:as-html type))))
       (<:input :type "Submit" :value "update")
       (<:fieldset
	(<:legend (<:as-html (current-type self)))
	(display test :type (current-type self)))))))


(defcomponent standard-display-component ()
  ((display-function :accessor display-function :initarg :display)))

(defmethod render ((self standard-display-component))
  (funcall (display-function self) self))