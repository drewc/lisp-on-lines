(in-package :lol)

(defun find-many-to-many-class (slot-name instance)
  (let* ((imd (getf (meta-model::find-slot-metadata instance slot-name)
		    :db-info))
	 (jc (make-instance (getf imd :join-class)))
	 (jcmd (getf (meta-model::find-slot-metadata jc (getf imd :target-slot))
		     :db-info)))
    (getf jcmd :join-class)))


(defattribute many-to-many ()
  ())



(defdisplay (:description (attribute many-to-many))
  (let ((instances (select-instances object t))
	new-instance)
    (<:ul
     (<:li (<ucw:button :action (add-new-relation component object (getp slot-name))
			(<:as-html "Add New")))
     (<:li  (<ucw:button :action (add-new-relation component object new-instance)
			 (<:as-html "Add:"))
	    (<ucw:select :accessor new-instance
			 (arnesi:dolist* (i instances)
			   (<ucw:option
			    :value i
			    (display component i :type 'one-line)))))
     (dolist* (i (attribute-value object attribute))
       (<:li
	(<ucw:a :action (call-view ((car i) (action-view slot) (ucw::parent slot)))
		(<:as-html "(view) "))
	(<ucw:a :action (delete-relationship slot (second i) instance)
		(<:as-html "(remove) "))
	(display component object)))))
  (display component (mapcar #'car (slot-value object (getp :slot-name)))))
		      