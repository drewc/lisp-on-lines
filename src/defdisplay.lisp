(in-package :lisp-on-lines)

(defmethod find-properties (object)
  (list))

(defmethod find-properties ((attribute standard-attribute))
  (warn "atttributre properties ~A" (attribute.properties attribute))
  (attribute.properties attribute))

(defmacro with-properties ((properties &optional prefix)  &body body)
  (with-unique-names (p)
    (let ((get (intern (string-upcase (if prefix (strcat prefix '-getp) "GETP"))))
	  (set (intern (string-upcase (if prefix (strcat prefix '-setp) "SETP"))))
	  (props (intern (string-upcase (if prefix (strcat prefix '-properties) "PROPERTIES")))))
      `(let ((,p ,properties))
	(flet ((,get  (p)
		 (getf ,p p))
	       (,set (p v)
		 (setf (getf ,p p) v))
	       (,props ()
		 ,p))
	  (declare (ignorable #',get #',set #',props))
	  ,@body)))))


;;;;; Macros
(defmacro do-attributes ((var occurence attributes) &body body)
  (with-unique-names (att properties type)
    `(loop for ,att in ,attributes
      do (let* ((,att (ensure-list ,att))
                (,properties (rest ,att))
                (,type (getf ,properties :type))
                (,var (if ,type
                          (make-attribute :name (first ,att) :type ,type :properties ,properties)
                          (find-attribute ,occurence (first ,att)))))
           (with-properties ((plist-union (rest ,att) (find-properties ,var)) ,var)
             ,@body)))))




(defmacro defdisplay (object (&key in-layer combination
				   (description t
						description-supplied-p)
				   (component 'component
					      component-supplied-p))
		      &body body)
  (with-unique-names (d c p)
    (let ((obj (car (ensure-list object))))
      `(define-layered-method display-using-description
	,@(when in-layer `(:in-layer ,in-layer))
	,@(when combination`(,combination))
	(,(cond
	   (description-supplied-p
	    (setf d description))
	   ((null description)
	    d)
	   (t
	    `(,d standard-occurence)))
	 ,(cond
	   (component-supplied-p
	    (setf c component))
	   ((null component)
	    c)
	   (t
	    `(,c component)))
	 ,object ,p)
	(with-component (,c) 
	  (with-properties ((plist-union ,p (find-properties ,(car (ensure-list d) ))))
	    ,(if (not description-supplied-p)
		 `(progn
		   
		   (setp :attributes (or (getp :attributes) (list-slots ,obj)))		   
		   (macrolet ((do-attributes* ((var &optional attributes) &body body)
				`(do-attributes (,var ,',d (or ,attributes (getp :attributes)))
				  
				  (flet ((display-current-attribute ()
					   (display-using-description* ,var ,',obj (,(intern (strcat var "-PROPERTIES"))))))
				  ,@body))))
		     ,@body))
		 `(progn ,@body))))))))