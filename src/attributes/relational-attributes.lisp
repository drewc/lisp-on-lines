(in-package :lisp-on-lines)

;;;; * Relational Attributes


(defvar *parent-relations* nil)

;;;; ** has-a
;;;; Used for foreign keys, currently only works with clsql.

(defattribute relational-attribute ()
  ())

(defdisplay :wrap-around ((attribute relational-attribute) object)
	    (print (cons "parent-r" *parent-relations*))
 (dletf (((value attribute) (attribute-value object attribute)))
   (unless (find (value attribute) *parent-relations* :test #'meta-model::generic-equal)
     (call-next-method))))

(defattribute has-a (relational-attribute)
  ()
  (:default-properties
      :has-a nil
    :test 'meta-model::generic-equal))

;;
(define-layered-method attribute-value (object (attribute has-a))
 (multiple-value-bind (obj key class)
     (meta-model:explode-foreign-key object (slot-name attribute) :nilp t)		       
  (if (persistentp object)
      obj
      (first  (select class
		      :where [= [slot-value class key] (call-next-method)]
		      :flatp t
		      )))))		       

(define-layered-method (setf attribute-value) ((value standard-object) object (attribute has-a))
  (let ((val (slot-value value (find-if (curry #'primary-key-p value) (list-keys value)))))
    (setf (attribute-value object attribute) val)))

(define-layered-function find-all-foreign-objects (o a))

(define-layered-method find-all-foreign-objects (object (attribute has-a))
  (select (meta-model:find-join-class object (slot-name attribute)) :flatp t))
		       
(defdisplay ((attribute has-a) object)
  (let ((args (plist-union (description.properties attribute) (has-a attribute)))
	(val (attribute-value object attribute)))
    (when val
      (setf (getf args :type)
	    'lol::one-line))
    (apply #'display* val
	   args)))


(defdisplay
  :in-layer editor ((attribute has-a) object)
 (<ucw:select
  :accessor (attribute-value object attribute)

  :test (test attribute)
  (dolist* (obj (find-all-foreign-objects object attribute))
    (<ucw:option
     :value obj
     (display* obj :layers '(+ as-string))))))

;;;; ** Has-Many attribute

(defattribute has-many ()
  ()
  (:default-properties
      :add-new-label "Add New"
    :has-many nil
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
			  (sort-arguments attribute)))
		(*parent-relations* (cons object *parent-relations*)))

	   (apply #'display* i (has-many attribute)))))


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
		      