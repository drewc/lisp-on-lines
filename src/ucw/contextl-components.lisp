(in-package :lisp-on-lines-ucw)

(defmacro dlambda ((&rest args) &body body)
  (let ((env (gensym)))
    `(let ((,env (capture-dynamic-environment)))
       (lambda (,@args)
	 (with-dynamic-environment (,env)
	   ,@body)))))

(export '(dlambda) :lisp-on-lines-ucw)

(defclass contextl-application (standard-application) 
  ()
  (:default-initargs 

   :action-class 'contextl-action))

(defclass contextl-action (ucw-standard:standard-action) 
  ((dynamic-environment :accessor action-dynamic-environment
			:initform nil
			:initarg :dynamic-environment))
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod shared-initialize :after ((action contextl-action) slots &rest args)
  (declare (ignore slots args))
  (setf (action-dynamic-environment action) (capture-dynamic-environment)))

(defmethod ucw-core:handle-action :wrap-around ((action contextl-action) application session frame)
      (call-next-method))

(defmethod ucw-core:call-action :around ((action contextl-action) application session frame)
  (with-dynamic-environment ((action-dynamic-environment action))
    (call-next-method)))

(defmethod ucw-core:call-callbacks :around ((action contextl-action) frame request)
  (with-dynamic-environment ((action-dynamic-environment action))
    (call-next-method)))


(defclass contextl-component (standard-component) 
  ((component-dynamic-environment :accessor component-dynamic-environment
				  :initform nil))
  (:metaclass standard-component-class))

(defmethod render :wrap-around ((component contextl-component))
  (if (component-dynamic-environment component)
      (with-dynamic-environment ((component-dynamic-environment component))
	(call-next-method))
      (progn  (setf (component-dynamic-environment component) (capture-dynamic-environment))
	      (call-next-method))))

(defmethod/cc call-component :before ((from t) (to contextl-component))
 (setf (component-dynamic-environment to) (capture-dynamic-environment)))




#+LOL-TEST(progn 
  (defclass contextl-test-application (contextl-application)
    ()
    (:default-initargs :url-prefix "/contextl/"))

  (defparameter *context-test-application* (make-instance 'contextl-test-application))

  (register-application ucw-user:*example-server* *context-test-application*)
  
  (defentry-point "test.ucw" (:application *context-test-application*) ()
      (call 'contextl-test-component))
 
 (defdynamic foo 1)

(defclass contextl-test-component (contextl-component) ()
  (:metaclass standard-component-class))

(defmethod render ((component contextl-test-component))
  (<:As-html  (dynamic foo))
  (dlet ((foo (1+ (dynamic foo))))
    (<:as-html (dynamic foo))
    (<:br)
    (with-described-object (T nil)
      (let ((a (find-attribute *description* 'identity))) 
	(<ucw:a :action  (call 'contextl-test-component)
		"Call")))
    (<:br)
    (when (component.calling-component component)
      (<ucw:a :action (answer)
	      "Answer")))
))
















#+nil (defclass contextl-session-frame (ucw-core::standard-session-frame)
  ())

#+nil(defmethod ucw-core::register-callback-in-frame ((frame contextl-session-frame) callback &key &allow-other-keys)
  (let ((lambda (ucw::callback-lambda callback)))
    (let ((context (contextl:current-layer-context)))
      (setf (ucw::callback-lambda callback) 
	    (lambda (arg)
	      (contextl:funcall-with-layer-context context lambda arg)))
      (call-next-method))))

#+nil(setf ucw-core::*session-frame-class* 'contextl-session-frame)

  
