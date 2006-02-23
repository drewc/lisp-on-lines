(in-package :lisp-on-lines)

;;;; This file contains various hacks that maintain backwards
;;;; compat for programs written in older versions of LoL.

;;;; While we try to maintain this, some things just require breaking
;;;; with the past. You learn to live with it.


(defmethod find-old-type (type)
  type)

;;!legacy string
(defmethod find-attribute-class-for-type ((type (eql 'mewa-string)))
  'string-attribute)
;; legacy int
(defmethod find-attribute-class-for-type ((type (eql 'mewa-integer)))
  'integer-attribute)

;; currency
(defmethod find-attribute-class-for-type ((type (eql 'mewa-currency)))
  'currency-attribute)
;; legacy relations

(defmethod find-attribute-class-for-type ((type (eql 'ajax-foreign-key)))
  'lol::has-a)


(defmethod find-attribute-class-for-type ((type (eql 'foreign-key)))
  'lol::has-a)

(defmethod find-layer-for-type ((type (eql 'mewa-one-line-presentation)))
  'one-line)

(defmethod find-old-type ((type (eql 'one-line)))
  'mewa-one-line-presentation)

(defmethod find-old-type ((type (eql 'one-line)))
  'mewa-one-line-presentation)





