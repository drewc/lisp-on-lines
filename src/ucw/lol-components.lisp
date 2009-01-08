(in-package :lol-ucw)

(defcomponent lol-component ()
  ())

(defmethod output-component ((self lol-component))
  self)

(defmethod render ((self lol-component))
  (display (output-component self) self))




