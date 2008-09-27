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
    :initarg :editor
    :layered t
    :accessor attribute-editor
    :initform (make-instance 'attribute-editor)
    :documentation "This ones a bit odd")))

(defmethod shared-initialize :after ((object standard-attribute) 
				      slots &rest args &key input &allow-other-keys)

  (when input 
    (setf (attribute-editor object) 
	  (apply #'make-instance (find-editor-class input)
		 input))))

      
(defun find-editor-class (spec)
  (let ((class (getf spec :class))
	(type (getf spec :type)))
    (or class (when 
		  (and type (symbolp type)) 
		(let ((name (format nil "~A-~A" type 'attribute-editor)))
		  (or (find-class (intern name (symbol-package type)) nil)
		      (find-class (intern name) nil)
		      'string-attribute-editor))))))

(defclass attribute-editor ()
    ((class :initarg :class)
     (type :initarg :type
	   :initform 'string
	   :accessor attribute-editor-type)
     (parser :initarg :parse-using
	     :initform 'identity
	     :accessor attribute-editor-parsing-function)
     (prompt :initarg :prompt 
	     :initform nil)
     (unbound-value
	 :initarg :unbound-value
       :initform "")))



(defclass string-attribute-editor (attribute-editor) ())
(defclass text-attribute-editor (string-attribute-editor) ())

(deftype password () 'string)

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
  (let ((value (attribute-value attribute)))
  (unless (or (unbound-slot-value-p value)
	      (typep value 
		     (attribute-editor-type 
		      (attribute-editor attribute))))
    (return-from attribute-editp nil)))
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





		       