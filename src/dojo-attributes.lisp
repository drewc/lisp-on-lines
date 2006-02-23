(in-package :lisp-on-lines)

(deflayer dojo)

(define-layered-class
    attribute :in-layer dojo ()
  ((dojo-type :accessor dojo-type :initarg :dojo-type :initform nil :special t)))


(defgeneric display-as-dojo-type (type attribute object component))

(defdisplay
  :in-layer dojo :after ((attribute standard-attribute) object)
 (when (dojo-type attribute)
   (display-as-dojo-type (dojo-type attribute) attribute object self)))

(defcomponent dojo-test (window-component)
  (
   (results :accessor results :initarg :results)))

(defmethod render ((self dojo-test))
  (<:as-is (js:js* `(array
		     ,@(loop for r in (results self)
			     for n upfrom 0
			     collect `(array , 
				       (with-output-to-string (s)
					 (yaclml:with-yaclml-stream s
					   (display self r :type 'as-string))) ,n))))))


(defmethod display-as-dojo-type ((type (eql 'combo-box)) attribute object component)
  
  (let* ((search-function (search-function attribute))
	(select-function (select-function attribute))
	(select-callback (ucw::make-new-callback (lambda (x)
						   (warn "setting index to ~A" 							    (parse-integer x))
						   (funcall select-function 
							    (parse-integer x))))))
    "The combo box widget"
  (<ucw:script
   `(dojo.require "dojo.*")
   `(dojo.require "dojo.widget.*")
   `(dojo.require "dojo.widget.html.ComboBox")
   (js:with-unique-js-names (element combo-box)

     `(dojo.add-on-load
       (lambda ()
	 (setf ,element (dojo.by-id ,(id attribute)))
	 (setf ,combo-box
	       (dojo.widget.from-script
		"ComboBox"
		(create
		 :data-url (+ , (lol::make-action-url
				 component
				 (call-component
				  (context.window-component *context*)
				  (make-instance 'dojo-test
						 :results
						 (funcall search-function
							  (attribute-value object attribute)))))
				"&"
				,(escape-as-uri (callback attribute))
				"=%{searchString}")
		 :mode "remote")
		,element))
	 ((slot-value ,combo-box 'set-value) (slot-value ,element 'value))
	 (dojo.event.connect
	  ,combo-box "selectOption"
	  (lambda ()
	    (setf (slot-value ,element 'value)
		  (slot-value ,combo-box 'selected-result))
	    (dojo.io.bind
	     (create
	      :url (+ ,(lol::make-action-url
			component
			nil)
		      "&"
		      ,(escape-as-uri (callback attribute))
		      "="
		      (slot-value ,combo-box 'selected-result)
		      "&"
		      ,select-callback
		      "="
		      (slot-value ,combo-box 'combo-box-selection-value.value))))))))))))
  