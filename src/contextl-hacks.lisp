(in-package :contextl)


(defmethod contextl:layer-name :around (layer)
  (or (call-next-method) layer))

;;; HACK:
;;; Since i'm not using deflayer, ensure-layer etc, 
;;; There are a few places where contextl gets confused 
;;; trying to locate my description layers.

;;; TODO: investigate switching to deflayer!

(defun contextl::prepare-layer (layer)
  (if (symbolp layer)
      (if (eq (symbol-package layer)
	  (find-package :description-definers))
	  layer
	  (contextl::defining-layer layer))
      
      layer))

(defmethod find-layer-class :around ((layer symbol) &optional errorp environment)
  (if (eq (symbol-package layer)
	  (find-package :description-definers))
      (find-class layer)
      (call-next-method)))


;;; HACK: There are classes named NIL (partial classes) in the superclass list.
;;; These cannot be given the special object superclass when re-initializing
;;; is it will be in the subclasses superclasses AFTER this class, causing
;;; a confict.
;;; Since we don't care about these classes (?) this might work (?)

(defmethod initialize-instance :around
  ((class special-class) &rest initargs
   &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (or
       ;; HACK begins
       (not (ignore-errors (class-name class)))
       ;; ENDHACK
	  (loop for superclass in direct-superclasses
            thereis (ignore-errors (subtypep superclass 'special-object))))
    (call-next-method)
    (progn  (apply #'call-next-method class
           :direct-superclasses
           (append direct-superclasses
                   (list (find-class 'special-object)))
           initargs))))

(defmethod reinitialize-instance :around
  ((class special-class) &rest initargs
   &key (direct-superclasses () direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if direct-superclasses-p
      (if (or  ; Here comes the hack
	   (not (class-name class)) 
	       ;endhack
	   (loop for superclass in direct-superclasses
	      thereis (ignore-errors (subtypep superclass 'special-object))))
      (call-next-method)
      (apply #'call-next-method class
             :direct-superclasses
             (append direct-superclasses
                     (list 
		      (find-class 'special-object)))
             initargs)))
     (call-next-method))



(defun funcall-with-special-initargs (bindings thunk)
  (let ((arg-count 0))
  (special-symbol-progv
      (loop for (object . initargs) in bindings
            for initarg-keys = (loop for key in initargs by #'cddr 
				     collect key into keys
				    count t into count
				    finally (incf arg-count count)
				            (return keys))
            nconc (loop for slot in (class-slots (class-of object))
                        when (and (slot-definition-specialp slot)
                                  (intersection initarg-keys (slot-definition-initargs slot)))
                        collect (with-symbol-access
                                  (slot-value object (slot-definition-name slot)))))
      (make-list arg-count :initial-element nil)
    (loop for (object . initargs) in bindings
          do (apply #'shared-initialize object nil :allow-other-keys t initargs))
    (funcall thunk))))