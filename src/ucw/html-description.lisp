(in-package :lisp-on-lines)

(export '(html-description) (find-package :lisp-on-lines))

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
		 (display-attribute-label object attribute))))))))

(define-layered-function display-html-attribute-value (object attribute)
  (:method (object attribute)
    (<:span 
	:class "lol-attribute-value"
	(<:as-html   
	 (with-output-to-string (*display*)
	   (display-attribute-value object attribute))))
))

(define-layered-function display-html-attribute (object attribute)
  (:method (object attribute)
 (<:div 
       :class (attribute-css-class attribute)
       (when (attribute-dom-id attribute) 
	 :id (attribute-dom-id attribute))
       (display-html-attribute-label object attribute)
       (display-html-attribute-value object attribute)
       (<:br)))
  (:method :in-layer #.(defining-description 'inline) 
	   (object attribute)
 (<:span 
       :class (attribute-css-class attribute)
       (when (attribute-dom-id attribute) 
	 :id (attribute-dom-id attribute))
       (display-html-attribute-label object attribute)
       (<:as-html " ")
       (display-html-attribute-value object attribute)
       (<:as-html " "))))

(define-layered-method display-html-attribute-value 
  :in-layer #.(defining-description 'editable) (object attribute)

    (<:span 
	:class "lol-attribute-value"
    (if (attribute-editp object attribute)	
    (<lol:input :reader (attribute-value object attribute)
		:writer (lambda (val)
			  (setf (attribute-value object attribute) val)))
    (call-next-method))
))		

(define-layered-function display-html-description (description display object)
  (:method (description display object)
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
       :class (list (attribute-value* css-class) "lol-description" "t")
       :id    (attribute-value* dom-id)
       (unless *object* (error "Object is nil .. why?"))
       (dolist (attribute (attributes description))
	 (display-html-attribute *object* attribute))))))
		       

(define-layered-method display-html-description 
  :in-layer #.(defining-description 'inline) (description display object)
  
  (with-attributes (css-class dom-id) description
   

    (<:span
     :class (list (attribute-value* css-class) "lol-description")
     :id    (attribute-value* dom-id)
     (unless *object* (error "Object is nil .. why?"))
     (dolist (attribute (attributes description))
       (display-html-attribute *object* attribute))))
  )

(define-display 
  :in-description html-description ((description t) 
				    (display lol-ucw:component) 
				    object)
  (display-html-description description display object))
     
      
  
		
  
