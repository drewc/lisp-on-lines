
(in-package :mewa)
 
(defparameter *default-type* :ucw)

;;; maps meta-model slot-types to slot-presentation
(defparameter *slot-type-map*
  '(boolean   ucw::mewa-boolean
    string    ucw::mewa-string
    number    ucw::mewa-currency
    integer   ucw::mewa-integer
    currency  ucw::mewa-currency
    ))

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
	  (meta-model:list-slot-types instance)))


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
attributes is an alist keyed on the attribute name."
  (or (assoc class-name *attribute-map*) 
      (progn 
	(setf *attribute-map* (acons class-name (list (list)) *attribute-map*)) 
	(assoc class-name *attribute-map*))))

(defgeneric find-class-attributes (class))

(defmethod find-class-attributes ((model t))
  (find-or-create-attributes (class-name (class-of model))))

(defmethod find-class-attributes ((model symbol))
  (find-or-create-attributes model))

(defmethod clear-class-attributes ((model t))
  (setf (cdr (find-class-attributes model)) nil))

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

(defmethod perform-set-attributes ((model t) definitions)
  (dolist (def definitions)
    (funcall #'set-attribute model (first def) (rest def))))

(defmethod set-attribute-properties ((model t) attribute properties)
  (let ((a (find-attribute model attribute)))
    (if a
	(setf (cddr a) (plist-nunion properties (cddr a)))
	(error "Attribute ~A does not exist" attribute) )))

(defmethod perform-set-attribute-properties ((model t) definitions)
  (dolist (def definitions)
    (funcall #'set-attribute-properties model (car def) (cdr def))))
  

(defmethod default-attributes ((model t))
  "return the default attributes for a given model using the meta-model's meta-data"
  (append (mapcar #'(lambda (s) 
		      (cons (car s) 
			    (gen-pslot 
			     (if (meta-model:foreign-key-p model (car s))
				 'ucw::foreign-key
				 (cadr s))
			     (string (car s)) (car s)))) 
		  (meta-model:list-slot-types model))
	  (mapcar #'(lambda (s) 
		      (cons s (append (gen-pslot 'ucw::has-many (string s) s) 
				      `(:presentation 
					(make-presentation 
					 ,model 
					 :type :one-line)))))
		  (meta-model:list-has-many model))))

(defmethod set-default-attributes ((model t))
  "Set the default attributes for MODEL"
  (clear-class-attributes model)
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
   (attribute-slot-map
    :accessor attribute-slot-map
    :initform nil)
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
   (modifiedp :accessor modifiedp :initform nil :initarg :modifiedp)
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

(defmethod find-slot-presentation-for-attribute ((self mewa) attribute)
  (let ((class-name 
	 (or (gethash (second attribute) ucw::*slot-type-mapping*) 
	     (error  "Can't find slot type for ~A" (second attribute)))))
		
	  (cons (first attribute) (apply #'make-instance 
	   class-name
	   (append (cddr attribute) (list :parent self :size 30))))))

(defmethod find-slot-presentations ((self mewa))
  (mapcar #'(lambda (a) (find-slot-presentation-for-attribute self a))
	  (find-applicable-attributes self)))

(defmethod find-attribute-slot ((self mewa) (attribute symbol))
  (cdr (assoc attribute (attribute-slot-map self))))

(defmethod initialize-slots ((self mewa))
  (when (use-instance-class-p self)
    (setf (classes self) 
	  (append (find-instance-classes self)
		  (classes self))))
  (setf (attribute-slot-map self) (find-slot-presentations self))
  (setf (slots self) (mapcar #'(lambda (x)(cdr x)) (attribute-slot-map self ))))
  

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


(defmethod initialize-slots-place ((place ucw::place) (mewa mewa))
  (setf (slots mewa) (mapcar #'(lambda (x) 
			       (prog1 x 
				 (setf (component.place x) place)))
                            (slots mewa))))
  
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


(defcomponent about-dialog (option-dialog)
  ((body :initarg :body)))

(defmethod render-on ((res response) (self about-dialog))
  (call-next-method)
  (render-on res (slot-value self 'body)))


(defmethod instance-is-stored-p ((instance clsql:standard-db-object))
  (slot-value instance 'clsql-sys::view-database))

(defmethod instance-is-stored-p ((mewa mewa))
  (instance-is-stored-p (instance mewa)))

(defaction cancel-save-instance ((self mewa))
  (cond  
    ((instance-is-stored-p (instance self))
      (meta-model::update-instance-from-records (instance self))
      (answer self))
    (t (answer nil))))

(defaction save-instance ((self mewa))
  (meta-model:sync-instance (instance self))
  (setf (modifiedp self) nil)
  (answer self))


(defaction ensure-instance-sync ((self mewa))
  (when (modifiedp self)
    (let ((message (format nil "Record has been modified, Do you wish to save the changes?")))
      (case (call 'about-dialog
                  :body (make-presentation (instance self) 
					   :type :viewer)
		  :message message
		  :options '((:save . "Save changes to Database")
			     (:cancel . "Cancel all changes")))
	(:cancel
	 (cancel-save-instance self))
	(:save 
	 (save-instance self))))))

(defaction ok ((self mewa) &optional arg)
  "Returns the component if it has not been modified. if it has been, prompt user to save or cancel"
  (declare (ignore arg))
  (ensure-instance-sync self)
  (answer self))

(defmethod (setf presentation-slot-value) :around (value (slot slot-presentation) instance)
  (let* ((old (prog1 
		 (presentation-slot-value slot instance)
	       (call-next-method)))
	(new (presentation-slot-value slot instance)))
  
  (unless (equal new old )
    (let ((self (ucw::parent slot)))
      (setf (modifiedp self) instance
	    (modifications self)  (append (list new old value slot instance) (modifications self)))))))

;;;; * Finally set up some defaults

(setf (find-attribute t :viewer) 
      '(mewa-object-presentation :global-properties (:editablep nil))
      (find-attribute t :editor)
      '(mewa-object-presentation :global-properties (:editablep t))
      (find-attribute t :one-line)
      '(mewa::mewa-one-line-presentation)
      (find-attribute t :listing)
      '(mewa::mewa-list-presentation :global-properties (:editablep nil) :editablep t)
      (find-attribute t :search-presentation)
      '(mewa-object-presentation))





;; This software is Copyright (c) Drew Crampsie, 2004-2005.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
