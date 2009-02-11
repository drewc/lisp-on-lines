(in-package :lol-ucw)

(defclass lol-component ()
  ()
  (:metaclass standard-component-class))

(defmethod output-component ((self lol-component))
  self)

(defmethod render ((self lol-component))
  (display (output-component self) self))




