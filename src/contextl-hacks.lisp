(in-package :contextl)

;;; HACK: We are ending up with classes named NIL in the superclass list.
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