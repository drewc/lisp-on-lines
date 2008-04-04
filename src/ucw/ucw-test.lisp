(in-package :lol-test)

(defclass lol-test-server (standard-server)
  ())

(defclass lol-test-application (standard-application)
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

(register-application *lol-test-ucw-server* *lol-test-ucw-application*)

(defentry-point "index.ucw" (:application *lol-test-ucw-application*) ()
  (call 'lol-test-window))

(defun startup-lol-ucw-test ()
  (startup-server *lol-test-ucw-server*))

(defun shutdown-lol-ucw-test ()
 (shutdown-server *lol-test-ucw-server*))

(defcomponent lol-test-window (standard-window-component)
  ()
  (:default-initargs 
      :body (make-instance 'lol-test-suite-component)))

(define-symbol-macro $window (lol-ucw:context.window-component *context*))

(define-symbol-macro $body (window-body $window))

(defcomponent lol-test-suite-component ()
  ((test :component lol-test-simple-action :accessor test)
   (component :component lol-test-render :accessor component)))

(define-symbol-macro $test (test $body))

(define-symbol-macro $component (component $body))

(defmethod render ((self lol-test-suite-component))
  (<:H1 "Lisp On Lines Web test suite")
     (render (slot-value self 'test))
  (<:div 
   :style "border:1px solid black;"
   (render (slot-value self 'component))))

(defcomponent lol-test-render ()
  ((message :initform "test" :accessor message :initarg :message)))

(defmethod render ((self lol-test-render))
  (<:h3 :id "test-render" 
	(<:as-html (format nil "Hello ~A." (message self)))))

(defcomponent lol-test-simple-action ()
  ())

(defmethod render ((self lol-test-simple-action))
  (<:ul
   (<:li (<lol:a 
	  :function 
	  (lambda ()
	    (setf (message $component) 
		  (format nil "~A : ~A" (message $component) "FUNCTION")))
	  "Test <:A :FUNCTION type actions"))
   (<:li 
    (<lol:a 
     :action (setf (message $component) 
		   (format nil "~A : ~A" (message $component) "ACTION"))
     "Test <:A :ACTION type actions"))
   (<:li 
    (<lol:a 
     :action* (make-action 
	       (lambda ()
		 (setf (message $component) 
		       (format nil "~A : ~A" (message $component) "ACTION*"))))
     "Test <:A :ACTION* type actions"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-answer))
     "Test CALL-COMPONENT/ANSWER-COMPONENT"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-call-magic))
     "Test CALL/ANSWER MAGIC"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-call-answer-action-magic))
     "Test CALL/ANSWER ACTION MAGIC"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-simple-form))
     "Test Simple Form"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-multi-submit-form))
     "Test Multi Form"))
   (<:li 
    (<lol:a 
     :action (call-component $component (make-instance 'lol-test-input))
     "Test Form input"))
))

(defcomponent lol-test-answer (lol-test-render) ()
  (:default-initargs :message "CALL was ok. Go Back will answer"))

(defmethod render :wrapping ((self lol-test-answer))
  (call-next-method)
  (<lol:a :action (answer-component self nil) "Go Back."))

(defcomponent lol-test-simple-form (lol-test-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod render :wrapping ((self lol-test-simple-form))
  (call-next-method)
  (<lol:form 
   :action (setf (message self) "Form Submitted")
   (<:submit))
  (<lol:a :action (answer-component self nil) "Go Back."))

(defcomponent lol-test-multi-submit-form (lol-test-render) ()
  (:default-initargs :message "Testing Simple Form:"))

(defmethod render :wrapping ((self lol-test-multi-submit-form))
  (call-next-method)
  (<lol:form 
   :action (setf (message self) "Form Submitted")
   (<:submit)
   (<lol:submit :action (setf (message self) "Submit 2" )
		:value "2")
   (<lol:submit :action (setf (message self) "Submit 3")
		3))
  (<lol:a :action (answer-component self nil) "Go Back."))

(defcomponent lol-test-input (lol-test-render) 
 ()	      
  (:default-initargs :message "Testing INPUTS"))

(defmethod render :wrapping ((self lol-test-input))
  (call-next-method)
  (<lol:form 
   :function (constantly t)
   (<lol:input :type "text" :accessor (message self))
   
   (<:submit)
  )
  (<lol:a :action (answer-component self nil) "Go Back."))



(defcomponent lol-test-call-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Testing CALL magic."))

(defmethod render :wrapping ((self lol-test-call-magic))
  (call-next-method)
  (<lol:a :action (setf (message self) (call 'lol-test-answer-magic)) "Test CALL")
  (<:br)
  (<lol:a :action (answer-component self nil) "Go Back."))



(defcomponent lol-test-answer-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(defmethod render :wrapping ((self lol-test-answer-magic))
  (call-next-method)
  
  (<lol:a :action (answer "Ja, dat is vut ve answer" ) "IT! (hit here)"))

(defcomponent lol-test-call-answer-action-magic (lol-test-render) 
 ()	      
  (:default-initargs :message "Hit it to answer"))

(defaction test-call-component ()
  (call 'lol-test-call-answer-action-magic :message "We made it"))

(defaction test-answer-component ()
  (answer "We Made IT BACK!!!"))

(defmethod render :wrapping ((self lol-test-call-answer-action-magic))
  (call-next-method)
  (<lol:a :action (test-call-component) "Test CALL from ACTION")
  (<:br)  
  (<lol:a :action (test-answer-component) "Test ANSWER from ACTION"))


	    	      



