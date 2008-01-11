(in-package :lisp-on-lines)

(export '(html-description))

(define-description html-description ()
  ((css-class  :value "lol-description")
   (dom-id :function (lambda (x)
		       (declare (ignore x))
		       (symbol-name 
			(gensym "DOM-ID-")))))
  (:mixinp t))


(define-description t (html-description)
  ()
  (:in-description html-description))

(define-layered-class html-attribute ()
  ((css-class :accessor attribute-css-class 
	      :initform "lol-attribute")
   (dom-id :accessor attribute-dom-id :initform nil)))

(define-layered-class standard-attribute
  :in-layer #.(defining-description 'html-description)
 (html-attribute)
 ())

(define-display 
  :in-description html-description ((description t))
 (with-attributes (css-class dom-id) description
   
   (<:div 
    :class (attribute-value* css-class)
    :id    (attribute-value* dom-id)
    (dolist (attribute (attributes description))
      (<:div 
       :class (attribute-css-class attribute)
       (when (attribute-dom-id attribute) 
	 :id (attribute-dom-id attribute))
       (<:span 
	:class "lol-attribute-label"
	(<:as-html (attribute-label attribute)))
       (<:span 
	:class "lol-attribute-value"
	(<:as-html (attribute-value* attribute))))))))
     
      
  
		
  
