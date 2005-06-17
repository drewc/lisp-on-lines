(in-package :mewa)
 
(defparameter *default-type* :ucw)

;;; maps meta-model slot-types to slot-presentation
(defparameter *slot-type-map* '(number ucw:currency))

;;; an alist of model-class-name . attributes
;;; should really be a hash-table.
(defvar *attribute-map* (list)) 

;;; some utilities for merging plists

(defun plist-nunion (new-props plist)
  (loop for cons on new-props by #'cddr
	do (setf (getf plist (first cons)) (second cons))
	finally (return plist)))

(defun plist-union (new-props plist)
  "Non-destructive version of plist-nunion"
		   (plist-nunion new-props (copy-list plist)))

(defun gen-ptype (type)
  (or (getf *slot-type-map* type) type))

(defun gen-presentation-slots (instance)
  (mapcar #'(lambda (x) (gen-pslot (cadr x) 
				   (string (car x)) 
				   (car x))) 
	  (list-slot-types instance)))


(defun gen-pslot (type label slot-name)
  (copy-list `(,(gen-ptype type) 
	       :label ,label
	       :slot-name ,slot-name))) 

(defun gen-presentation-args (instance args)
  (declare (ignore instance))
  (if args args nil))


(defun find-or-create-attributes (class-name)
  "return an exisiting class attribute map or create one. 

A map is a cons of class-name . attributes. 
attributes is an alist keyed on the attribute nreeame."
  (or (assoc class-name *attribute-map*) 
      (progn 
	(setf *attribute-map* (acons class-name (list (list)) *attribute-map*)) 
	(assoc class-name *attribute-map*))))

(defgeneric find-class-attributes (class))

(defmethod find-class-attributes ((model t))
  (find-or-create-attributes (class-name (class-of model))))

(defmethod find-class-attributes ((model symbol))
  (find-or-create-attributes model))

(defmethod add-attribute ((model t) name def)
  (let ((map (find-class-attributes model)))
    (setf (cdr map) (acons name def (cdr map)))))

(defmethod find-attribute ((model t) name)
  (assoc name (cdr (find-class-attributes model))))

(defmethod (setf find-attribute) ((def list) (model t) name)
  (let ((attr (find-attribute model name)))
    (if attr
	(prog2 
	    (setf (cdr attr) def) 
	    attr)
        (prog2 
	    (add-attribute model name def) 
	    (find-attribute model name)))))

(defmethod set-attribute ((model t) name definition &key (inherit t))
  (setf (find-attribute model name) 
	(if inherit
	    (cons (car definition) 
		  (plist-union (cdr definition)
			 (cddr (find-attribute model name))))
	    definition)))


(defmethod default-attributes ((model t))
  (append (mapcar #'(lambda (s) 
		      (cons (car s) 
			    (gen-pslot 
			     (if (meta-model:foreign-key-p model
							   'ucw::foreign-key
							   (car s))
						   (cadr s))
						 (string (car s)) (car s)))) 
	  (meta-model:list-slot-types model))
	  (mapcar #'(lambda (s) (cons s (append (gen-pslot 'ucw::has-many (string s) s) `(:presentation (make-presentation ,model :type :one-line)))))
		  (meta-model:list-has-many model))))

(defmethod set-default-attributes ((model t))
  (mapcar #'(lambda (x) 
	      (setf (find-attribute model (car x)) (cdr x)))
	  (default-attributes model)))


(defgeneric attributes-getter (model))
	  
;;;presentations 




(defcomponent mewa ()
  ((attributes
    :initarg :attributes
    :accessor attributes
    :initform nil)
   (attributes-getter
    :accessor attributes-getter
    :initform #'get-attributes
    :initarg :attributes-getter)
   (global-properties
    :initarg :global-properties
    :accessor global-properties
    :initform nil)
   (classes 
    :initarg :classes 
    :accessor classes 
    :initform nil)
   (use-instance-class-p 
    :initarg :use-instance-class-p 
    :accessor use-instance-class-p 
    :initform t)
   (initializedp :initform nil)
   (modifiedp :accessor modifiedp :initform nil)
   (modifications :accessor modifications :initform nil)))


(defmethod attributes :around ((self mewa))
  (let ((a (call-next-method)))
    (or a (funcall (attributes-getter self) self))))

(defgeneric get-attributes (mewa))

(defmethod get-attributes ((self mewa))
  (if (instance self)
  (append (meta-model:list-slots (instance self))
	  (meta-model:list-has-many (instance self)))
  nil))


(defmethod find-instance-classes ((self mewa))
  (mapcar #'class-name 
	  (it.bese.arnesi.mopp:compute-class-precedence-list (class-of (instance self)))))

(defmethod find-all-attributes ((self mewa))
  (reduce #'append 
	  (mapcar #'(lambda (x) 
		      (cdr (find-class-attributes x)))
		  (classes self))))

(defun make-attribute (&rest props &key type &allow-other-keys)
	(remf props :type)
	(cons (gensym) (cons type props)))


(defmethod find-applicable-attributes ((self mewa))
  (let ((all-attributes (find-all-attributes self)))
    (flet ((gen-att (x) (let ((att (assoc x all-attributes)))
				     (when att 
				       (setf (cddr att) (plist-union (global-properties self) (cddr att)))
				       att))))
    (if (attributes self)
	(remove 'nil 
		(mapcar #'(lambda (x)
			    (cond 
			     ;;simple casee
			     ((symbolp x) 
			      (gen-att x))
			     ;;if the car is a keyword then this is an inline def
			     ((and (listp x) (keywordp (car x)))
			      (let ((att (apply #'make-attribute x)))
				(setf (cddr att) 
				      (plist-union (cddr att) (global-properties self)))
				att))
			     ;; if the plist has a :type	  
			     ((and (listp x) (getf (cdr x) :type))
			      (let ((new (cdr (apply #'make-attribute (cdr x))))
				    (def (gen-att (car x))))
				(setf (cdr new) (plist-union (cdr new) (cddr def)))
				(cons (car def) new)))
			     ;;finally if we are just overiding the props
			     ((and (listp x) (symbolp (car x)))
			      (let ((new (cdr (apply #'make-attribute (cdr x))))
				    (def (gen-att (car x))))
				(setf (cdr new) (plist-union (cdr new) (cddr def)))
				(cons (car def) (cons (second def) (cdr new)))))

			      )
			     )
				   
			(attributes self)))
      all-attributes))))

(defmethod find-slot-presentations ((self mewa))
  (mapcar #'(lambda (s)
	      (let ((class-name (or (gethash (second s) ucw::*slot-type-mapping*) 'mewa-object-presentation)))
	      (apply #'make-instance 
		     class-name
		     (append (cddr s) (list :parent self)))))
	  (find-applicable-attributes self)))



(defmethod initialize-slots ((self mewa))
  (when (use-instance-class-p self)
    (setf (classes self) 
	  (append (find-instance-classes self)
		  (classes self))))
  (setf (slots self) (find-slot-presentations   self)))
  

(defmethod make-presentation ((object t) &key (type :viewer) (initargs nil))
  (let* ((p (make-instance 'mewa-object-presentation))
	 (a (progn (setf (slot-value p 'instance) object)
		   (initialize-slots p) 
		   (assoc type (find-all-attributes p))))
	 
	 (i (apply #'make-instance (second a) (plist-union initargs (cddr a)))))
    (setf (slot-value i 'instance) object)
    i))

(defmethod make-presentation ((object t) &key (type :viewer) (initargs nil))
  (let* ((p (make-instance 'mewa-object-presentation))
	 (a (progn (setf (slot-value p 'instance) object)
		   (initialize-slots p) 
		   (assoc type (find-all-attributes p))))
	 
	 (i (apply #'make-instance (or (second a)
				       ;; if we didnt find the type, 
				       ;; use the symbol as a class. 
				       (if (eql (symbol-package type) 
						(find-package 'keyword))
					   (symbol-name type)
					   type))
				       (plist-union initargs (cddr a)))))
    (setf (slot-value i 'instance) object)
    (initialize-slots i)
    (setf (slot-value i 'initializedp) t)
    i))





(defmethod call-component :before ((from standard-component) (to mewa))
  (unless (slot-value to 'initializedp)
    (initialize-slots to))
  (setf (slot-value to 'initializedp) t)
  (setf (slots to) (mapcar #'(lambda (x) (prog2 
					     (setf (component.place x) (component.place from))
					     x))
			     (slots to))))

(defmacro call-presentation (object &rest args)
  `(present-object ,object :presentation (make-presentation ,object ,@args)))



(defaction cancel-save-instance ((self mewa))
  (answer nil))

(defaction save-instance ((self mewa))
  (meta-model:sync-instance (instance self))
   (setf (modifiedp self) nil)
       (answer self))


(defaction ok ((self mewa) &optional arg)
  "Returns the component if it has not been modified. if it has been, prompt user to save or cancel"
  (declare (ignore arg))
  (when (modifiedp self)
    (let ((message (format nil "Record has been modified, Do you wish to save the changes?<br/> ~a" (print (modifications self)))))
      (case (call 'option-dialog 
		  :message message
		  :options '((:save . "Save changes to Database")
			     (:cancel . "Cancel all changes")))
	(:cancel
	 (cancel-save-instance self))
	(:save 
	 (save-instance self)))))
  (answer self))



(defmethod (setf presentation-slot-value) :around (value (slot slot-presentation) instance)
  (let* ((old (prog1 
		 (presentation-slot-value slot instance)
	       (call-next-method)))
	(new (presentation-slot-value slot instance)))
  
  (unless (equal new old )
    (let ((self (ucw::parent slot)))
      (setf (modifiedp self) instance
	    (modifications self)  (append (list  (type-of new) (type-of old) (type-of value) slot instance )))))))