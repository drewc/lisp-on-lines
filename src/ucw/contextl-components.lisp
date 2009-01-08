(in-package :lisp-on-lines-ucw)

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

  
