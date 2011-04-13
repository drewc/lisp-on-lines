(in-package :lol-ucw)

;;; Not sure what the intent of this is, unused in maxclaims --clinton

(defclass lol-component ()
  ()
  (:metaclass standard-component-class))

(defmethod output-component ((self lol-component))
  self)

(defmethod render ((self lol-component))
  (display (output-component self) self))




