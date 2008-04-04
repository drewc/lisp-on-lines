(in-package :lisp-on-lines)

(export '(html-description) (find-package :lisp-on-lines))

(defvar *escape-html* t)

(defmethod generic-format ((display lol-ucw:component) string &rest args)
  (<:as-html (with-output-to-string (stream)
	       (apply #'call-next-method stream string args))))
      

(define-description html-description ()
  ())

(define-description t ()
  ((css-class  :value "lol-description" :activep nil)
   (dom-id :function (lambda (x)
		       (declare (ignore x))
		       (symbol-name 
			(gensym "DOM-ID-")))
	   :activep nil))
  (:in-description html-description))

(define-layered-class html-attribute ()
  ((css-class :accessor attribute-css-class 
	      :initform "lol-attribute")
   (dom-id :accessor attribute-dom-id :initform nil)))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'html-description)
 (html-attribute)
 ())

(define-layered-function display-html-attribute-label (object attribute)
  (:method (object attribute)
    (let ((label (attribute-label attribute)))
      	   (<:label 
	    :class "lol-attribute-label"
	    (when label 
	      (<:as-html 
	       (with-output-to-string (*display*)
		 (display-attribute-label attribute)))))))
  (:method 
      :in-layer #.(defining-description 'inline)
      (object attribute)
    (let ((label (attribute-label attribute)))
      (when label
		 (<:as-html 
	  (with-output-to-string (*display*)
	    (display-attribute-label attribute)))))))

(define-layered-function display-html-attribute-value (object attribute)
  (:method (object attribute)
    (<:span 
	:class "lol-attribute-value"
	(<:as-html   
	 (display-attribute-value attribute))))

  (:method 
    :in-layer #.(defining-description 'inline) (object attribute)
    (display-attribute-value attribute)))

(define-layered-function display-html-attribute (object attribute)
  
  (:method (object attribute)
    (<:div 
     :class (attribute-css-class attribute)
     (when (attribute-dom-id attribute) 
       :id (attribute-dom-id attribute))
     (display-html-attribute-label object attribute)
     (display-html-attribute-value object attribute)))
  
  (:method 
      :in-layer #.(defining-description 'inline) 
      (object attribute)
      (<:span 
       :class (attribute-css-class attribute)
       (when (attribute-dom-id attribute) 
	 :id (attribute-dom-id attribute))
       (display-html-attribute-label object attribute)
       (display-html-attribute-value object attribute))))

(define-layered-method display-using-description 
  :in-layer #.(defining-description 'html-description)
  :around ((attribute standard-attribute) display object &rest args)
 (declare (ignore args))
 (display-html-attribute object attribute))



(define-layered-method display-html-attribute-value 
  :in-layer #.(defining-description 'editable) (object attribute)

    (<:span 
	:class "lol-attribute-value"
    (if (attribute-editp object attribute)	
    (<lol:input :reader (attribute-value attribute)
		:writer (let ((obj (described-object (attribute-description attribute))))
			  (lambda (val)
			    (dletf (((described-object attribute) obj))
			      (setf (attribute-value attribute) val)))))
    (call-next-method))
))		

(define-layered-function display-html-description (description display object &optional next-method)
  (:method (description display object &optional (next-method #'display-using-description))
    (<:style
     (<:as-html "

div.lol-description .lol-attribute-label, 
div.lol-description .lol-attribute-value {
      display: block;
      width: 69%;
      float: left;
      margin-bottom: 1em;

}
div.lol-description 
.lol-attribute-label {
     text-align: right;
     width: 24%;
     padding-right: 20px;
}


div.lol-description 
br {
clear: left;
}"))
		       
    (with-attributes (css-class dom-id) description
   

      (<:div 
       :class (list (attribute-value css-class) "lol-description" "t")
       :id    (attribute-value dom-id)
       (funcall next-method)))))
		       

(define-layered-method display-html-description 
  :in-layer #.(defining-description 'inline) (description display object &optional next-method)
  (with-attributes (css-class dom-id) description
    (<:span
     :class (list (attribute-value css-class) "lol-description")
     :id    (attribute-value dom-id)
     (funcall next-method))))


(define-display 
  :in-description html-description ((description t) 
				    (display lol-ucw:component) 
				    object)
  (display-html-description description display object (lambda ()
							 (call-next-method))))





     
      
  
		
  
