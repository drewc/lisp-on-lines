(in-package :lol-test)

(defclass lol-test-server (ucw-core:standard-server)
  ())

(defclass lol-test-application (ucw:standard-application)
  ()
  (:default-initargs
    :url-prefix "/lisp-on-lines.test/"
;    :www-roots (list (cons "static/" (project-relative-pathname #P"wwwroot/")))
))

(defparameter *lol-test-ucw-application* (make-instance 'lol-test-application))

(defun make-backend ()
  (ucw::make-backend
   :httpd
   :host "localhost"
   :port 9090))

(defun make-server ()
  (make-instance
   'lol-test-server
   :backend (make-backend)))

(defparameter *lol-test-ucw-server* (make-server))

(ucw-core:register-application *lol-test-ucw-server* *lol-test-ucw-application*)

(ucw-core:defentry-point "index.ucw" (:application *lol-test-ucw-application*) ()
  (call 'lol-test-window))

(defun startup-lol-ucw-test ()
  (ucw-core:startup-server *lol-test-ucw-server*))

(defun shutdown-lol-ucw-test ()
 (ucw-core:shutdown-server *lol-test-ucw-server*))

(ucw-core:defcomponent lol-test-window (standard-window-component)
  ()
  (:default-initargs 
      :body (make-instance 'lol-test-suite-component)))

(define-symbol-macro $window (ucw-core:context.window-component *context*))

(define-symbol-macro $body (window-body $window))

(ucw-core:defcomponent lol-test-suite-component ()
  ((test :component lol-test-simple-action :accessor test)
   (component :component lol-test-render :accessor component)))

(define-symbol-macro $test (test $body))

(define-symbol-macro $component (component $body))

(defmethod ucw-core:render ((self lol-test-suite-component))
  (<:H1 "Lisp On Lines Web test suite")
     (render (slot-value self 'test))
  (<:div 
   :style "border:1px solid black;"
   (render (slot-value self 'component))))

(ucw-core:defcomponent lol-test-render ()
  ((message :initform "test" :accessor message :initarg :message)))

(defmethod ucw-core:render ((self lol-test-render))
  (<:h3 :id "test-render" 
	(<:as-html (format nil "Hello ~A." (message self)))))

(ucw-core:defcomponent lol-test-simple-action ()
  ())

(defmethod ucw-core:render ((self lol-test-simple-action))
  (<:ul
   (<:li (<ucw:a 
	  :function 
	  (lambda ()
	    (setf (message $component) 
		  (format nil "~A : ~A" (message $component) "FUNCTION")))
	  "Test <:A :FUNCTION type actions"))
   (<:li 
    (<ucw:a 
     :action (setf (message $component) 
		   (format nil "~A : ~A" (message $component) "ACTION"))
     "Test <:A :ACTION type actions"))
   (<:li 
    (<ucw:a 
     :action* (make-action 
	       (lambda ()
		 (setf (message $component) 
		       (format nil "~A : ~A" (message $component) "ACTION*"))))
     "Test <:A :ACTION* type actions"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-answer))
     "Test CALL-COMPONENT/ANSWER-COMPONENT"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-call-magic))
     "Test CALL/ANSWER MAGIC"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-call-answer-action-magic))
     "Test CALL/ANSWER ACTION MAGIC"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-simple-form))
     "Test Simple Form"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-multi-submit-form))
     "Test Multi Form"))
   (<:li 
    (<ucw:a 
     :action (call-component $component (make-instance 'lol-test-input))
     "Test Form input"))
))

(ucw-core:defcomponent lol-test-answer (lol-test-render) ()
  (:default-initargs :message "CALL was ok. Go Back will answer"))

(defmethod ucw-core:render :wrapping ((self lol-test-answer))
  (call-next-method)
  (<ucw:a :action (answer-component self nil) "Go Back."))

(ucw-core:defcomponent lol-test-simple-form (lol-test-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod ucw-core:render :wrapping ((self lol-test-simple-form))
  (call-next-method)
  (<ucw:form 
   :action (setf (message self) "Form Submitted")
   (<:submit))
  (<ucw:a :action (answer-component self nil) "Go Back."))

(ucw-core:defcomponent lol-test-multi-submit-form (lol-test-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod ucw-core:render :wrapping ((self lol-test-multi-submit-form))
  (call-next-method)
  (<ucw:form 
   :action (setf (message self) "Form Submitted")
   (<:submit)
   (<ucw:submit :action (setf (message self) "Submit 2" )
		:value "2")
   (<ucw:submit :action (setf (message self) "Submit 3")
		3))
  (<ucw:a :action (answer-component self nil) "Go Back."))

(ucw-core:defcomponent lol-test-input (lol-test-render) 
 ()	      
  (:default-initargs :message "Testing INPUTS"))

(defmethod ucw-core:render :wrapping ((self lol-test-input))
  (call-next-method)
  (<ucw:form 
   :function (constantly t)
   (<ucw:input :type "text" :accessor (message self))
   
   (<:submit)
  )
  (<ucw:a :action (answer-component self nil) "Go Back."))



(ucw-core:defcomponent lol-test-call-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Testing CALL magic."))

(defmethod ucw-core:render :wrapping ((self lol-test-call-magic))
  (call-next-method)
  (<ucw:a :action (setf (message self) (call 'lol-test-answer-magic)) "Test CALL")
  (<:br)
  (<ucw:a :action (answer-component self nil) "Go Back."))



(ucw-core:defcomponent lol-test-answer-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(defmethod ucw-core:render :wrapping ((self lol-test-answer-magic))
  (call-next-method)
  
  (<ucw:a :action (answer "Ja, dat is vut ve answer" ) "IT! (hit here)"))

(ucw-core:defcomponent lol-test-call-answer-action-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(ucw:defaction test-call-component ()
  (call 'lol-test-call-answer-action-magic :message "We made it"))

(ucw:defaction test-answer-component ()
  (answer "We Made IT BACK!!!"))

(defmethod ucw-core:render :wrapping ((self lol-test-call-answer-action-magic))
  (call-next-method)
  (<ucw:a :action (test-call-component) "Test CALL from ACTION")
  (<:br)  
  (<ucw:a :action (test-answer-component) "Test ANSWER from ACTION"))


	    	      



