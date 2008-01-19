(in-package :lisp-on-lines)

(export '(html-description) (find-package :lisp-on-lines))

(define-description html-description ()
  ())


(define-description t ()
  ((css-class  :value "lol-description")
   (dom-id :function (lambda (x)
		       (declare (ignore x))
		       (symbol-name 
			(gensym "DOM-ID-")))))
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
   (<:style
    (<:as-html "

.lol-attribute-label, .lol-attribute-value {
      display: block;
      width: 70%;
      float: left;
      margin-bottom: 10px;

}
.lol-attribute-label {
     text-align: right;
     width: 24%;
     padding-right: 20px;
}

.lol-attribute-value {
  
  }

br {
clear: left;
}"))

   (<:div 
    :class (list (attribute-value* css-class) "lol-description")
    :id    (attribute-value* dom-id)
    (dolist (attribute (attributes description))
      (<:div 
       :class (attribute-css-class attribute)
       (when (attribute-dom-id attribute) 
	 :id (attribute-dom-id attribute))
       (let ((label (attribute-label attribute)))
	 (when label
	   (<:label 
	    :class "lol-attribute-label"
	    (<:as-html label))))
       (<:span 
	:class "lol-attribute-value"
	(<:as-html (format nil "~A" (attribute-value* attribute))))
       (<:br))))))
     
      
  
		
  
