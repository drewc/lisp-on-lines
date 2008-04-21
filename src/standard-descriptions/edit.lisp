(in-package :lisp-on-lines)


(define-description editable ()
  ()
  (:mixinp t))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'editable)
  ()
  ((edit-attribute-p 
    :initform :inherit 
    :layered-accessor attribute-editp
    :initarg :editp
    :layered t)
   (setter
    :initarg :setter
    :layered t
    :accessor attribute-setter
    :initform nil)
   (attribute-editor 
    :initarg :input 
    :layered t
    :accessor attribute-editor
    :initform nil
    :documentation "This ones a bit odd")))

(defmethod attribute-editor :around (attribute)
  (flet ((find-editor-class (spec)
	   (let ((class (getf spec :class))
		 (type (getf spec :type)))
	     (or class (when (and type (symbolp type)) 
			 (intern (format nil "~A-~A" type 'attribute-editor)))
		 'string-attribute-editor))))
  (let ((editor? (call-next-method)))
    (if (listp editor?)
	(setf (attribute-editor attribute)
	      (apply #'make-instance (find-editor-class editor?) 
		     editor?))
	(call-next-method)))))


(defclass attribute-editor ()
    ((type :initarg :type
	   :initform 'string)
     (parser :initarg :parse-using
	     :initform 'identity
	     :accessor attribute-editor-parsing-function)
     (prompt :initarg :prompt 
	     :initform nil)))

(defclass string-attribute-editor (attribute-editor) ())
(defclass text-attribute-editor (string-attribute-editor) ())
(defclass password-attribute-editor (string-attribute-editor) ())

(defclass number-attribute-editor (attribute-editor) ()
  (:default-initargs 
   :parse-using 'parse-number:PARSE-NUMBER
   :type 'number))

(defun parse-attribute-value (attribute value)
  (funcall (attribute-editor-parsing-function 
		    (attribute-editor attribute))
	   value))

(define-layered-function display-attribute-editor (attribute)
  (:method (attribute)
    (setf (attribute-value attribute) 
	  (funcall (attribute-editor-parsing-function 
		    (attribute-editor attribute))
		   (read-line)))))

(define-description T ()
  ((editp :label "Edit by Default?"
	  :value nil 
	  :editp nil)
   (identity :editp nil)
   (type :editp nil)
   (class :editp nil))
  (:in-description editable))

(define-layered-method (setf attribute-value-using-object)
 :in-layer #.(defining-description 'editable)(value object attribute)

 (let ((setter (attribute-setter attribute)))
   (if setter
       (funcall setter value object)
       (error "No setter in ~A for ~A" attribute object))))


(define-layered-function attribute-editp (attribute)
  (:method (attribute) nil))

(define-layered-method attribute-editp 
  :in-layer #.(defining-description 'editable)
  ((attribute standard-attribute))
  (let ((edit?       (call-next-method)))
    (if (eq :inherit edit?)
	(attribute-value (find-attribute 
			  (attribute-description attribute) 
			  'editp))
	edit?)))
		       

(define-layered-method display-attribute-value  
  :in-layer #.(defining-description 'editable)
  ((attribute standard-attribute))  
  (if (attribute-editp attribute)
      (display-attribute-editor attribute)
      (call-next-method)))





		       