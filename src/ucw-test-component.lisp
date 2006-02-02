(in-package :lisp-on-lines)

;;;; LoL CLOS Test Class
(defclass/meta test-class ()
  ((test-string :initform "test string" :type string))
  (:documentation "foo"))

(define-attributes (test-class)
  (test-string t :label "String :" :editablep t))
  
(defcomponent test-component ()
  ((display-types :accessor display-types :initform (list 'viewer 'editor 'creator 'one-line 'as-string))
   (current-type :accessor current-type :initform 'viewer)
   (instance :accessor instance :initform (make-instance 'test-class))))

(defmethod render ((self test-component))
  (let ((test (instance self))) 
    (<:h1 (<:as-html "Lisp on Lines Test Component"))
    (with-component (self)
      (<ucw:form
       :action (refresh-component self)
       (<ucw:select :accessor (current-type self)
		    (dolist* (type (display-types self))
		      (<ucw:option :value type (<:as-html type))))
       (<:input :type "Submit" :value "update")
       (<:fieldset
	(<:legend (<:as-html (current-type self)))
	(display test :type (current-type self)))))

    (<:div
     (<:h2
      (<:as-html "UCW Presentation based displays (the old school"))
     (dolist (type '(:viewer :editor :creator :one-line :as-string))
       (<:h3 (<:as-html type))
       (present-view (test type self))
       (<ucw:a :action (call-view (test type self))
	       (<:as-html "Call to " type))))))


(defcomponent standard-display-component ()
  ((display-function :accessor display-function :initarg :display)))

(defmethod render ((self standard-display-component))
  (funcall (display-function self) self))