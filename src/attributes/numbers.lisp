(in-package :lisp-on-lines)

(defattribute number-attribute (base-attribute)
  ()
  (:type-name number))

;;;; INTEGER
(defattribute integer-attribute (base-attribute)
  ()
  (:type-name integer))

;;;; REALS

(defattribute real-attribute (base-attribute)
  ()
  (:type-name real))


;;;; Currency
(defattribute currency-attribute (base-attribute)
  ()
  (:type-name currency))

(defdisplay
   ((currency currency-attribute) object)
 (<:as-html (format nil "$~$" (attribute-value object currency))))
