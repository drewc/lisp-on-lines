(in-package :lisp-on-lines)
 
(defparameter *default-type* :ucw)

;;; some utilities for merging plists

(defun plist-nunion (new-props plist)
  (loop for cons on new-props by #'cddr
	do (setf (getf plist (first cons)) (second cons))
	finally (return plist)))

(defun plist-union (new-props plist)
  "Non-destructive version of plist-nunion"
		   (plist-nunion new-props (copy-list plist)))


;;; an alist of model-class-name . attributes
;;; should really be a hash-table.
(defvar *attribute-map* (list))

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

(defmethod perform-define-attributes ((model t) attributes)
  (loop for attribute in attributes
	do (destructuring-bind (name type &rest args)
		  attribute
		(cond ((eq type t)
		       ;;use the existing (default) type
		       (set-attribute-properties model name args))
		      ((not (null type))
		       ;;set the type as well
		       (set-attribute model name (cons type args)))))))
		       
(defmacro define-attributes (models &body attribute-definitions)
  `(progn
    ,@(loop for model in models
	    collect `(perform-define-attributes (quote ,model) (quote ,attribute-definitions)))
  (mapcar #'find-class-attributes (quote ,models ))))

(defun find-presentation-attributes (model)
  (remove nil (mapcar #'(lambda (att)
	      (when (keywordp (car att))
		(copy-list att) ))
	  (cdr (find-class-attributes model)))))


;;;; ** Default Attributes


;;;; The default mewa class contains the types use as defaults.
;;;; maps meta-model slot-types to slot-presentation

(defvar *default-attributes-class-name* 'default)

(define-attributes (default)
  (boolean mewa-boolean)
  (string mewa-string)
  (number mewa-currency)
  (integer   mewa-integer)
  (currency  mewa-currency)
  (clsql:generalized-boolean mewa-boolean)
  (foreign-key foreign-key)
  (:viewer mewa-viewer)
  (:editor mewa-editor)
  (:creator mewa-creator)
  (:one-line mewa-one-line-presentation)
  (:listing mewa-list-presentation :global-properties (:editablep nil) :editablep t)
  (:search-model mewa-object-presentation))

  
(defun find-default-presentation-attributes ()
  (if (eql *default-attributes-class-name* 'default)
      (find-presentation-attributes 'default)
      (remove-duplicates (append
			  (find-presentation-attributes 'default)
			  (find-presentation-attributes
			   *default-attributes-class-name*)))))


(defmacro with-default-attributes ((model-name) &body body)
  `(let ((*default-attributes-class-name* ',model-name))
    ,@body))

(defun gen-ptype (type)
  (let ((type (if (consp type) (car type) type)))
  (or (second (find-attribute *default-attributes-class-name* type))
      (second (find-attribute 'default type))
      type)))

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


(defmethod find-default-attributes ((model t))
  "return the default attributes for a given model using the meta-model's meta-data"
  (append (mapcar #'(lambda (s) 
		      (cons (car s) 
			    (gen-pslot 
			     (if (meta-model:foreign-key-p model (car s))
				 'foreign-key
				 (cadr s))
			     (string (car s)) (car s)))) 
		  (meta-model:list-slot-types model))
	  (mapcar #'(lambda (s) 
		      (cons s (append (gen-pslot 'has-many (string s) s) 
				      `(:presentation 
					(make-presentation 
					 ,model 
					 :type :one-line)))))
		  (meta-model:list-has-many model))
	  (find-default-presentation-attributes)))

(defmethod set-default-attributes ((model t))
  "Set the default attributes for MODEL"
  (clear-class-attributes model)
  (mapcar #'(lambda (x) 
	      (setf (find-attribute model (car x)) (cdr x)))
	  (find-default-attributes model)))


(defgeneric attributes-getter (model))
	  
;;;presentations 

(defcomponent mewa ()
  ((instance :accessor instance :initarg :instance) 
   (attributes
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
	 (or (gethash (if (consp (second attribute))
			  (car (second attribute))
			  (second attribute))
		      *presentation-slot-type-mapping*) 
	     (error  "Can't find slot type for ~A in ~A" attribute self ))))
		
    (cons (first attribute) (apply #'make-instance 
				   class-name
				   (append (cddr attribute) (list :parent self :size 30))))))

(defmethod find-slot-presentations ((self mewa))
  (mapcar #'(lambda (a) (find-slot-presentation-for-attribute self a))
	  (find-applicable-attributes self)))

(defmethod find-attribute-slot ((self mewa) (attribute symbol))
  (cdr (assoc attribute (attribute-slot-map self))))

(defmethod initialize-slots ((self mewa))
  (when (instance self)
    (when (use-instance-class-p self)
      (setf (classes self) 
	    (append (find-instance-classes self)
		    (classes self))))
    (setf (attribute-slot-map self) (find-slot-presentations self))
    (setf (slots self) (mapcar #'(lambda (x)(cdr x)) (attribute-slot-map self )))))


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

(defmethod make-presentation ((list list) &key (type :listing) (initargs nil))  
  (let ((args (append
	       `(:type ,type) 
	       `(:initargs 
		 (:instances ,list
		  ,@initargs)))))
    
    (apply #'make-presentation (car list) args)))

(defmethod initialize-slots-place ((place ucw::place) (mewa mewa))
  (setf (slots mewa) (mapcar #'(lambda (x) 
			       (prog1 x 
				 (setf (component.place x) place)))
                            (slots mewa))))
  
(arnesi:defmethod/cc call-component :before ((from standard-component) (to mewa))
  (unless (slot-value to 'initializedp)
    (initialize-slots to))
  (setf (slot-value to 'initializedp) t)
  (initialize-slots-place (component.place from) to)
  to)



(defmacro call-presentation (object &rest args)
  `(present-object ,object :presentation (make-presentation ,object ,@args)))


(defcomponent about-dialog (option-dialog)
  ((body :initarg :body)))

(defmethod render-on ((res response) (self about-dialog))
  (call-next-method)
  (render-on res (slot-value self 'body)))




(defaction cancel-save-instance ((self mewa))
  (cond  
    ((meta-model::persistentp (instance self))
      (meta-model::update-instance-from-records (instance self))
      (answer self))
    (t (answer nil))))

(defaction save-instance ((self mewa))
  (meta-model:sync-instance (instance self))
  (setf (modifiedp self) nil)
  (answer self))

(defmethod confirm-sync-instance ((self mewa))
  nil)

(defaction ensure-instance-sync ((self mewa))
  (when (modifiedp self)
    (if nil
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
	     (save-instance self))))
	(save-instance self))))

(defaction sync-and-answer ((self mewa))
  (ensure-instance-sync self)
  (answer (instance self)))

(defaction ok ((self mewa) &optional arg)
  "Returns the component if it has not been modified. if it has been, prompt user to save or cancel"
  ;(declare (ignore arg))
  (sync-and-answer self))

(defmethod (setf presentation-slot-value) :around (value (slot slot-presentation) instance)
  (let* ((old (prog1 
		 (presentation-slot-value slot instance)
	       (call-next-method)))
	(new (presentation-slot-value slot instance)))
  
  (unless (equal new old )
    (let ((self (ucw::parent slot)))
      (setf (modifiedp self) instance
	    (modifications self)  (append (list new old value slot instance) (modifications self)))))))







;; This software is Copyright (c) Drew Crampsie, 2004-2005.
;; You are granted the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.
