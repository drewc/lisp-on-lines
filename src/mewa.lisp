(in-package :lisp-on-lines)
 
(defparameter *default-type* :ucw)

;;;; I think these are unused now
(defmethod perform-set-attributes ((occurence-name t) definitions)
  (dolist (def definitions)
    (funcall #'set-attribute occurence-name (first def) (rest def))))

(defmethod perform-set-attribute-properties ((occurence-name t) definitions)
  (dolist (def definitions)
    (funcall #'set-attribute-properties occurence-name (car def) (cdr def))))

;;;; PLIST Utilities.

(defun plist-nunion (new-props plist)
  "Destructive Merge of plists. PLIST is modified and returned. 
NEW-PROPS is merged into PLIST such that any properties
in both PLIST and NEW-PROPS get the value in NEW-PROPS. 
The other properties in PLIST are left untouched."
  (loop for cons on new-props by #'cddr
	do (setf (getf plist (first cons)) (second cons))
	finally (return plist)))

(defun plist-union (new-props plist)
  "Non-destructive version of plist-nunion"
		   (plist-nunion new-props (copy-list plist)))


;;;; * Occurences

(defvar *occurence-map* (make-hash-table)
  "Presentations are created by associating an 'occurence' 
with an instance of a class. This is usually keyed off class-name,
although an arbitrary occurence can be used with an arbitrary class.")

(define-layered-class
    standard-occurence ()
    ((attribute-map :accessor attribute-map :initform (make-hash-table)))
    (:documentation
     "an occurence holds the attributes like a class holds slot-definitions.
Attributes are the metadata used to display, validate, and otherwise manipulate actual values stored in lisp objects."))

(defun find-or-create-occurence (name)
  "Returns the occurence associated with this name."
  (let ((occurence (gethash name *occurence-map*)))
    (if occurence
	occurence
	(let ((new-occurence (make-instance 'standard-occurence)))
	  (setf (gethash name *occurence-map*) new-occurence)
	  new-occurence))))

(defun clear-occurence (occurence)
  "removes all attributes from the occurence"
  (setf (attribute-map occurence) (make-hash-table)))

(defgeneric find-occurence (name)
  (:method ((name symbol))
    (find-or-create-occurence name))
  (:method (instance)
    (find-or-create-occurence (class-name (class-of instance)))))


;;;; * Attributes

(define-layered-class
    standard-attribute ()
    ((name :layered-accessor attribute.name :initarg :name :initform "attribute")
     (type :layered-accessor attribute.type :initarg :type :initform t :type symbol)
     (plist :layered-accessor attribute.plist :initarg :plist :initform nil))
    (:documentation "Attributes are used to display a part of a thing, such as a slot of an object, a text label, the car of a list, etc."))


(defmethod print-object ((self standard-attribute) stream)
  (print-unreadable-object (self stream :type t)
    (with-slots (name type) self
      (format stream "~A ~A" name type))))

(define-layered-class
    presentation-attribute (standard-attribute)
    ()
    (:documentation "Presentation Attributes are used to display objects 
using the attributes defined in an occurence. Presentation Attributes are always named using keywords."))

(defun clear-attributes (name)
  "removes all attributes from an occurance"
  (clear-occurence (find-occurence name)))

(defmethod find-attribute-class-for-name (name)
  "presentation attributes are named using keywords"
  (if (keywordp name)
      'presentation-attribute
      'standard-attribute))

(defmethod ensure-attribute ((occurence standard-occurence) name type plist)
  "Creates an attribute in the given occurence"
  (setf (gethash name (attribute-map occurence))
	(make-instance (find-attribute-class-for-name name)
		       :name name :type type :plist plist)))

(defmethod find-attribute ((occurence standard-occurence) name)
  (gethash name (attribute-map occurence)))

(defmethod find-all-attributes ((occurence standard-occurence))
  (loop for att being the hash-values of (attribute-map occurence)
	collect att))

(defmethod ensure-attribute (occurence-name name type plist)
  (ensure-attribute
   (find-occurence occurence-name)
   name
   type
   plist)) 

;;;; The following functions make up the public interface to the
;;;; MEWA Attribute Occurence system.

(defmethod find-all-attributes (occurence-name)
  (find-all-attributes (find-occurence occurence-name)))

(defmethod find-attribute (occurence-name attribute-name)
  "Returns the ATTRIBUTE named by ATTRIBUTE-NAME in OCCURANCE-name"
  (find-attribute (find-occurence occurence-name) attribute-name))

(defmethod (setf find-attribute) ((def list) occurence-name attribute-name)
  (ensure-attribute occurence-name attribute-name (first def) (rest def)))

(defmethod set-attribute (occurence-name attribute-name definition &key (inherit t))
  (let ((att (find-attribute occurence-name attribute-name)))
      (setf (find-attribute occurence-name attribute-name) 
	(if (and att inherit) 
	    (cons (car definition) 
		  (plist-union (cdr definition)
			 (attribute.plist att)))
	    definition)))) 

(defmethod set-attribute-properties ((occurence-name t) attribute properties)
  (let ((a (find-attribute occurence-name attribute)))
    (if a
	(setf (attribute.plist a) (plist-nunion properties (attribute.plist a)))
	(error "Attribute ~A does not exist" attribute))))

(defmethod perform-define-attributes ((occurence-name t) attributes)
  (loop for attribute in attributes
	do (destructuring-bind (name type &rest args)
		  attribute
		(cond ((eq type t)
		       ;;use the existing (default) type
		       (set-attribute-properties occurence-name name args))
		      ((not (null type))
		       ;;set the type as well
		       (set-attribute occurence-name name (cons type args)))))))
		       
(defmacro define-attributes (occurence-names &body attribute-definitions)
  `(progn
    ,@(loop for occurence-name in occurence-names
	    collect `(perform-define-attributes (quote ,occurence-name) (quote ,attribute-definitions)))))


(defmethod setter (attribute)
  (let ((setter (getf (attribute.plist attribute) :setter))
	(slot-name (getf (attribute.plist attribute) :slot-name)))
    (cond (setter
	   setter)
	  (slot-name
	   #'(lambda (value object)
	       (setf (slot-value object slot-name) value)))
	  (t
	   #'(lambda (value object)
	     (warn "Can't find anywere to set ~A in ~A using ~A" value object attribute))))))
    
(defmethod getter (attribute)
  (let ((getter (getf (attribute.plist attribute) :getter))
	(slot-name (getf (attribute.plist attribute) :slot-name)))
    (cond (getter
	   getter)
	  (slot-name
	   #'(lambda (object)
	       (when (slot-boundp object slot-name)
		 (slot-value object slot-name)))))))

(defgeneric attribute-value (instance attribute)
  (:method (instance (attribute standard-attribute))
    (funcall (getter attribute) instance)))

(defgeneric (setf attribute-value) (value instance attribute)
  (:method (value instance (attribute standard-attribute))
    (funcall (setter attribute) value instance)))


;;;; ** Default Attributes


;;;; The default mewa class contains the types use as defaults.
;;;; maps meta-model slot-types to slot-presentation

(defvar *default-attributes-class-name* 'default)

(defmacro with-default-attributes ((occurence-name) &body body)
  `(let ((*default-attributes-class-name* ',occurence-name))
    ,@body))

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
  (:as-string mewa-one-line-presentation)
  (:one-line mewa-one-line-presentation)
  (:listing mewa-list-presentation :global-properties (:editablep nil) :editablep t)
  (:search-model mewa-object-presentation))

(defun find-presentation-attributes (occurence-name)
  (loop for att in (find-all-attributes occurence-name)
	when (typep att 'presentation-attribute)
	 collect att))

(defun attribute-to-definition (attribute)
  (nconc (list (attribute.name attribute)
	       (attribute.type attribute))
	 (attribute.plist attribute)))

(defun find-default-presentation-attribute-definitions ()
  (if (eql *default-attributes-class-name* 'default)
      (mapcar #'attribute-to-definition (find-presentation-attributes 'default)) 
      (remove-duplicates (mapcar #'attribute-to-definition
				 (append
				  (find-presentation-attributes 'default)
				  (find-presentation-attributes
				   *default-attributes-class-name*))))))
(defun gen-ptype (type)
  (let* ((type (if (consp type) (car type) type))
	 (possible-default (find-attribute *default-attributes-class-name* type))
	 (real-default (find-attribute 'default type)))
    (cond
      (possible-default
	(attribute.type possible-default))
       (real-default
	(attribute.type real-default))
       (t type))))

(defun gen-presentation-slots (instance)
  (mapcar #'(lambda (x) (gen-pslot (cadr x) 
				   (string (car x)) 
				   (car x))) 
	  (meta-model:list-slot-types instance)))


(defun gen-pslot (type label slot-name)
  (copy-list `(,(gen-ptype type) 
	       :label ,label
	       :slot-name ,slot-name))) 

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
	  (find-default-presentation-attribute-definitions)))

(defmethod set-default-attributes ((model t))
  "Set the default attributes for MODEL"
  (clear-attributes model)
  (mapcar #'(lambda (x) 
	      (setf (find-attribute model (car x)) (cdr x)))
	  (find-default-attributes model)))
	  
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

(defun make-attribute (&rest props &key type &allow-other-keys)
	(remf props :type)
	(cons (gensym) (cons type props)))

(defun make-presentation-for-attribute-list-item
    (occurence att-name plist parent-presentation &optional type)
  (declare (type list plist) (type symbol att-name))
  "This is a ucw specific function that will eventually be factored elsewhere."
  (let* ((attribute (find-attribute occurence att-name))
	 (type (when attribute (or type (attribute.type attribute))))
	 (class-name 
	  (or (gethash (if (consp type)
			   (car type)
			   type)
		       *presentation-slot-type-mapping*) 
	      (error  "Can't find slot type for ~A in ~A from ~A" att-name occurence parent-presentation))))
   
    (cons (attribute.name attribute) (apply #'make-instance 
				   class-name
				   (append (plist-nunion
					    plist
					    (plist-union
					     (global-properties parent-presentation)
					     (attribute.plist attribute)))
					   (list :size 30 :parent parent-presentation))))))

(defmethod find-applicable-attributes-using-attribute-list (occurence attribute-list)
  "Returns a list of functions that, when called with an object presentation, 
returns the ucw slot presentation that will be used to present this attribute 
in that object presentation."
    (loop for att in attribute-list
	  with funs = (list)
	  do (let ((att att)) (cond 
	       ;;simple casee
	       ((symbolp att) 
		(push #'(lambda (p)
			  (make-presentation-for-attribute-list-item occurence att nil p))
		      funs))
	       ;;if the car is a keyword then this is an inline def
	       ;; drewc nov 12 2005:
	       ;; i never used this, and never told anybody about it.
	       ;; removing it.
	       #+ (or) ((and (listp x) (keywordp (car x)))
			(let ((att (apply #'make-attribute x)))
			  (setf (cddr att) 
				(plist-union (cddr att) (global-properties self)))
			  att))
	     
	       ;; if the plist has a :type	  
	       ((and (listp att) (getf (cdr att) :type))
		(let ((type (getf (cdr att) :type)))
		  (push #'(lambda (p)
			    (make-presentation-for-attribute-list-item
			     occurence (first att)
			     (cdr att)
			     p
			     type))
			funs)))
	       ;;finally if we are just overiding the props
	       ((and (listp att) (symbolp (car att)))
		(push #'(lambda (p)
			  (make-presentation-for-attribute-list-item occurence (first att) (rest att) p))
		      funs))))
	  finally (return (nreverse funs))))


(defun find-attribute-names (mewa)
  (mapcar #'(lambda (x)
	      (if (listp x)
		  (first x)
		  x))
	  (attributes mewa)))

(defmethod find-applicable-attributes ((self mewa))
  (if (attributes self)
      (find-applicable-attributes-using-attribute-list (instance self) (attributes self))
      (find-applicable-attributes-using-attribute-list (instance (get-attributes self)))))


(defmethod find-slot-presentations ((self mewa))
  (mapcar #'(lambda (a) (funcall a self))
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
  ;(warn "Initargs : ~A" initargs)
  (let* ((a (find-attribute object type))   
	 (i (apply #'make-instance
		   (if a
		       (attribute.type a)
		       type) 
		   (plist-union initargs (when a
					   (attribute.plist a))))))
    
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
