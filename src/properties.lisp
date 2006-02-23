(in-package :lisp-on-lines)




;;;; PLIST Utilities.

(defun plist-nunion (new-props plist)
  "Destructive Merge of plists. PLIST is modified and returned. 
NEW-PROPS is merged into PLIST such that any properties
in both PLIST and NEW-PROPS get the value in NEW-PROPS. 
The other properties in PLIST are left untouched."
  (loop for cons on new-props by #'cddr
	do (setf (getf plist (first cons)) (second cons))
	finally (return plist))
  plist)

(defun plist-union (new-props plist)
  "Non-destructive version of plist-nunion"
		   (plist-nunion new-props (copy-list plist)))




    

(defun slots-as-properties (object)
  "Makes a plist by making a keyword from the ...ahh .. read the damn code"
  (mapcan 
   #'(lambda (slot-name)
       (when (slot-boundp object slot-name)
	 
	 (list (intern (symbol-name slot-name) 
		       (find-package :keyword))
	       (slot-value object slot-name))))
   (list-slots object)))

(defun properties-as-slots (plist)
  "takes a plist and turns it into slot-definitions, interning the key names in *package*"
  (loop for (key val) on plist by #'cddr
	collect (let ((name (intern (symbol-name key))))
		  `(,name :accessor ,name :initarg ,key :special t :initform ,val))))

(defmacro with-properties ((properties &optional (prefix '||))  &body body)
  (with-unique-names (p)
    (let ((get (intern (strcat prefix '.get)))
	  (set (intern (strcat prefix '.set)))
	  (props (intern (strcat prefix '.properties))))
      `(let ((,p ,properties))
	(flet ((,get  (p)
		 (getf ,p p))
	       (,set (p v)
		 (setf (getf ,p p) v))
	       (,props ()
		 ,p))
	  (declare (ignorable #',get #',set #',props))
	  ,@body)))))