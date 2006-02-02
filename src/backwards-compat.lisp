(in-package :lisp-on-lines)

;;;; This file contains various hacks that maintain backwards
;;;; compat for programs written in older versions of LoL.

;;;; While we try to maintain this, some things just require breaking
;;;; with the past. You learn to live with it.


;;!legacy string
(defmethod find-attribute-class-for-type ((type (eql 'mewa-string)))
  'string-attribute)

