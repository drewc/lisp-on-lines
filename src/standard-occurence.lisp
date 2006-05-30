
(in-package :lisp-on-lines)

;;;; STRINGS

(find-or-create-occurence 'string)

(defmethod find-occurence ((string string))
  (find-occurence 'string))

(set-attribute 'string 'identity `(string :getter ,#'(lambda (x)
						       (identity x))))
(set-default-attributes 'string)

;;;; LISTS

(find-or-create-occurence 'list)

(defmethod find-occurence ((list list))
  (find-occurence 'list))

(set-attribute 'list 'identity `(string :getter ,#'(lambda (x)
						       (identity x))))
(set-default-attributes 'string)

