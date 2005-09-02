(in-package :lisp-on-lines)

;;;; for when there is nothing left to display.
(defcomponent empty-page (window-component)
  ())

(defmethod render-on ((res response) (self empty-page))
  "didnt find a thing")

(defcomponent auto-complete ()
  ((input-id :accessor input-id :initform (arnesi:random-string 10 arnesi:+ascii-alphabet+))
   (output-id :accessor output-id :initform (arnesi:random-string 10 arnesi:+ascii-alphabet+))
   (index-id :accessor index-id :initform (arnesi:random-string 10 arnesi:+ascii-alphabet+))
   (client-value :accessor client-value :initform "" :documentation "The string the user has, so far, insterted.")
   (selected-value-index :accessor selected-value-index :initform nil :documentation "The index in value-list of the item selected via Ajax")
   (value-list :accessor value-list :initform '())
   (values-generator :accessor values-generator :initarg :values-generator
                     :documentation "Function which, when passed the auto-complete component, returns a list of objects.")
   (as-value :accessor as-value :initarg :as-value
             :documentation "Function which, when passed a value, returns the string to put in the text box.")
   (render :accessor render :initarg :render
           :documentation "Function which, when passed the component and one of the values render it (the value).")
   (input-size :accessor input-size :initarg :input-size :initform 20)
   (submit-on-click-p :accessor submit-on-click-p :initarg :submit-on-click-p :initform t)
   (output-component :accessor output-component :initarg :output-component :initform 'auto-complete-output)))

(defmethod js-on-complete ((l auto-complete))
  `(lambda (transport) 
    (setf (slot-value (document.get-element-by-id ,(output-id l)) 
	   'inner-h-t-m-l)
     transport.response-text)))

(defmacro make-action-url (component action)
  "
There has got to be something like this buried in UCW somewhere, 
but here's what i use."
  `(ucw::print-uri-to-string
    (compute-url ,component 
     :action-id (ucw::make-new-action (ucw::context.current-frame *context*)
		 (lambda ()
		   (arnesi:with-call/cc
		     ,action))))))

(defmethod generate-ajax-request-for-action ((l auto-complete) &key (action-url "index.ucw"))
  `(new 
    (*Ajax.*Request 
     ,action-url 
     (create))))
	       
(defmacro with-ajax-action ((component) &body action)
  `(generate-ajax-request-for-action ,component 
    :action-url (make-action-url ,component (progn ,@action)))) 
								      

(defaction call-auto-complete ((self t) auto-complete-id value)
    (let ((auto-complete (get-session-value (intern auto-complete-id))))
    (if auto-complete
        (call-auto-complete-from-output auto-complete auto-complete-id value self)
        (call 'empty-page :message (error "ASD")))))

(defaction call-auto-complete-from-output ((auto-complete auto-complete) auto-complete-id value output)
  (setf (client-value auto-complete) value)
  (let ((self output))
    (call (output-component auto-complete) :auto-complete auto-complete)
    (call 'empty-page :message (error "ASD"))))



(defmethod js-on-select ((l auto-complete)))
  
(defmethod render-on ((res response) (l auto-complete))
  ;; session-values are stored in an eql hash table.
  (let ((input-key (intern (input-id l))))
    ;; We are storing the input components in the session,
    ;; keyed on the string that we also use as the id for 
    ;; the input field. 
    
    (unless (get-session-value input-key)
      (setf (get-session-value input-key) l))
    
    ;; A hidden field to hold the index number selected via javascript
    (<ucw:input :type "hidden" 
		:accessor (selected-value-index l)
		:id (index-id l))
    (<ucw:text :accessor (client-value l)
	       :id (input-id l) :size (input-size l))
    (<:div :id (output-id l) :class "auto-complete" (<:as-html " ")))
  (let* ((a (make-symbol (format nil "~A-autocompleter" (input-id l))))
	(f (make-symbol (format nil "~A.select-entry-function"a))))
    (<ucw:script 
     `(setf ,a
       (new 
	(*Ajax.*Autocompleter 
	 ,(input-id l) ,(output-id l) 
	 ,(format nil "auto-complete.ucw?&auto-complete-id=~A&~A=~A" 
		  (input-id l) ucw::+session-parameter-name+ 
		  (ucw::session.id (ucw::context.session ucw::*context*)))
	 (create
	  :param-name "value"))))
     `(setf ,f (slot-value ,a 'select-entry))
     `(setf (slot-value ,a 'select-entry)
       (lambda () 
	 (,f)
	 (setf (slot-value (document.get-element-by-id ,(index-id l)) 'value)
	       (slot-value ,a 'index))
	 ,(js-on-select l)
	 )))))
     

(defmethod find-selected-object ((self auto-complete))
  (if (< 0 (length (selected-value-index self)))
      (nth (parse-integer (selected-value-index self))
	   (value-list self))))


;;;; * auto-complete-ouput 


(defcomponent auto-complete-output (window-component)
  ((auto-complete :initarg :auto-complete :accessor auto-complete)))

(defmethod render-on ((res response) (output auto-complete-output))
  (let ((auto-complete (auto-complete output)))
    (setf (value-list auto-complete)
	  (funcall (values-generator auto-complete) (client-value auto-complete)))
    (<:ul 
     :class "auto-complete-list" 
     (arnesi:dolist* (value (value-list auto-complete))
       (<:li 
	:class "auto-complete-list-item"
	(funcall (render auto-complete) value))))))


(defcomponent fkey-auto-complete (auto-complete)
  ())

(defmethod js-on-select ((self fkey-auto-complete))
  (with-ajax-action (self)
    (mewa::sync-foreign-instance (ucw::parent self) (find-selected-object self))))

(defslot-presentation ajax-foreign-key-slot-presentation (mewa::foreign-key-slot-presentation)
  ((search-slots :accessor search-slots :initarg :search-slots :initform nil)
   (live-search 
     :accessor live-search
     :component fkey-auto-complete))
  (:type-name ajax-foreign-key))


(defmethod shared-initialize :after ((slot ajax-foreign-key-slot-presentation) slots &rest args)
  (let* ((l (live-search slot))
	 (slot-name (slot-name slot))
	 (instance (instance (ucw::parent slot)))
	 (foreign-instance (explode-foreign-key instance slot-name))
	 (class-name (class-name
		      (class-of foreign-instance))))
    ;; If no search-slots than use the any slots of type string
    (unless (search-slots slot)
      (setf (search-slots slot) (find-slots-of-type foreign-instance)))

    (setf (lisp-on-lines::values-generator l) 
	  (lambda (input)
	    (word-search class-name  
			 (search-slots slot)  input)))
		    
    (setf (lisp-on-lines::render l)
	  (lambda (val) 
	    (<ucw:render-component 
	     :component (make-presentation val :type :one-line))))))
	  


(defmethod  present-slot :around ((slot ajax-foreign-key-slot-presentation) instance)  
  (setf (mewa::foreign-instance slot) 
	(when (presentation-slot-value slot instance) 
	  (meta-model:explode-foreign-key instance (slot-name slot))))
  (flet ((render () (when (mewa::foreign-instance slot)(call-next-method))))
    (if (slot-boundp slot 'ucw::place)
        (cond 
          ((editablep slot)
	   (<ucw:render-component :component (live-search slot))
           (<ucw:submit :action  (mewa::search-records slot instance) :value "find" :style "display:inline"))
          ((mewa::linkedp slot)
           (<ucw:a :action (mewa::view-instance slot (foreign-instance slot)) 
                   (render)))
          (t       
           (render)))
	;; presentation is used only for rendering
        (render))))