(in-package :lisp-on-lines)

;;;; * Relational Attributes


;;;; ** has-a

(defattribute has-a ()
  ()
  (:default-properties
      :has-a nil))

(define-layered-method attribute-value (object (attribute has-a))
 (meta-model:explode-foreign-key object (slot-name attribute) :nilp t))		       
		       
(defdisplay ((attribute has-a) object)
  (let ((args (plist-union (description.properties attribute) (has-a attribute)))
	(val (attribute-value object attribute)))
    (when val
      (setf (getf args :type)
	    'lol::one-line))	    
    (apply #'display* val
	   args)))


;;;; ** Has-Many attribute

(defattribute has-many ()
  ()
  (:default-properties
      :add-new-label "Add New"
    :sort-arguments  (list #'< :key #'(lambda (x) (funcall (car (list-keys x)) x))))
  (:default-initargs
      :type 'lol::one-line))


(define-layered-method
    attribute-value (object (has-many has-many))
  (slot-value object (slot-name has-many)))

(defdisplay ((attribute has-many) object)
    ;
  ;(<ucw:submit :action (add-to-has-many slot instance) :value (add-new-label attribute))
	    
 (<:div  :style "clear:both;"
	 (let* ((i (apply #'sort (slot-value object (slot-name attribute))
			  (sort-arguments attribute))))
	   (<:ul 
	    (dolist* (x i)
	      (<:li (display* x
			      :type 'lol::one-line
			      :layers '(+ wrap-link - label-attributes))))))))


(defun find-many-to-many-class (slot-name instance)
  (let* ((imd (getf (meta-model::find-slot-metadata instance slot-name)
		    :db-info))
	 (jc (make-instance (getf imd :join-class)))
	 (jcmd (getf (meta-model::find-slot-metadata jc (getf imd :target-slot))
		     :db-info)))
    (getf jcmd :join-class)))


(defattribute many-to-many ()
  ())



(defdisplay ((attribute many-to-many) object)
  (<:as-html "ASDASD"))

  #+nil(let ((instances (select-instances object t))
	new-instance)
    (<:ul
     (<:li (<ucw:button :action (add-new-relation component object (.get slot-name))
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
  ;(display component (mapcar #'car (slot-value object (.get :slot-name))))
		      