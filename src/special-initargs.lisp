(in-package :lisp-on-lines)

(defmethod initargs.slot-names (object)
  "Returns ALIST of (initargs) . slot-name."
  (nreverse (mapcar #'(lambda (slot)
	      (cons (closer-mop:slot-definition-initargs slot)
		    (closer-mop:slot-definition-name slot)))
	  (closer-mop:class-slots (class-of object)))))

(defun find-slot-names-from-initargs-plist (object initargs-plist)
  "returns (VALUES SLOT-NAMES VALUES), Given a plist of initargs such as one would pass to :DEFAULT-INITARGS.
SLOT-NAMES contains the slot-names specified by the initarg, and VALUES the corresponding VALUE."
  (let (slot-names values
	(initargs.slot-names-alist (initargs.slot-names object))) 
    (loop for (initarg value) on initargs-plist
	  do (let ((slot-name
		    (cdr (assoc-if #'(lambda (x) (member initarg x))
				   initargs.slot-names-alist))))
	       (when slot-name ;ignore invalid initargs. (good idea/bad idea?)
		 (push slot-name slot-names)
		 (push value values)))
	  finally (return (values slot-names values)))))

(defun funcall-with-special-initargs (object initargs function &rest args)
  "Call FUNCTION with dynnamic bindings of the slots in OBJECT specified by the INITARGS plist"
  (multiple-value-bind (slot-names values)
	(find-slot-names-from-initargs-plist object initargs)
      (special-symbol-progv
	  (with-symbol-access
	    (loop for slot-name in slot-names
		  collect (slot-value object slot-name)))
	  values
	(apply function args))))

(defmacro with-special-initargs ((object &rest initargs) &body body)
  `(funcall-with-special-initargs ,object ,initargs
    #'(lambda ()
	,@body)))