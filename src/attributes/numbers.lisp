(in-package :lisp-on-lines)

(defattribute number-attribute (base-attribute)
  ()
  (:type-name number))

;;;; INTEGER
(defattribute integer-attribute (number-attribute)
  ()
  (:type-name integer))

(defattribute integer-attribute (number-attribute integer-field)
  ()
  (:in-layer editor)
  (:default-initargs
      :default-value ""
    :default-value-predicate (complement #'numberp))
  (:type-name integer))


(define-layered-method (setf attribute-value) ((value string) object (attribute integer-attribute)) 	       
  (let ((*read-eval* nil))
    (unless (string= "" value)
      (let ((value (read-from-string value)))
	(when (numberp value)
	  (setf (attribute-value object attribute) value))))))

;;;; REALS

(defattribute real-attribute (number-attribute)
  ()
  (:type-name real))

(define-layered-method (setf attribute-value) ((value string) object (attribute real-attribute))
  (let ((*read-eval* nil))
    (unless (string= "" value)
      (let ((value (read-from-string value)))
	(when (numberp value)
	  (setf (attribute-value object attribute) value))))))


;;;; Currency
(defattribute currency-attribute (real-attribute)
  ()
  (:type-name currency))


(defdisplay
  :in-layer t
   ((currency currency-attribute) object)

   (<:as-html (format nil "$~$" (or (attribute-value object currency) ""))))

(defdisplay
  :in-layer editor
  ((currency currency-attribute) object)
    (LET ((value (attribute-value (object currency) currency)))
    (<:input
     :NAME
     (callback currency)
     :VALUE (escape-as-html (strcat (display-value currency value)))
     :TYPE
     "text"))
  )
