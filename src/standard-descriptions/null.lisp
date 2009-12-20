(in-package :lisp-on-lines)

(define-description null (symbol list)
  ())

(define-layered-method description-of ((object null))
 (find-description 'null))
