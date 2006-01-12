;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.systems)
    (defpackage :coop.tech.systems
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.systems)

(defsystem :lisp-on-lines
    :components ((:static-file "lisp-on-lines.asd")
		 (:file "src/packages")
		 (:module :src
			  :components ((:file "static-presentations")
				       (:file "mewa")
				       (:file "validation")
				       (:file "validation/email-address")
				       (:file "lisp-on-lines")
				       (:file "presentations")
				       (:file "slot-presentations")
				       (:file "slot-presentations/date")
				       (:file "standard-display")
				       (:file "standard-attributes")
				       (:file "relational-attributes"))
			  :serial t)
		 (:module :components
			  :pathname "src/components/"
			  :components ((:file "range-list")
				       (:file "ajax")
				       (:file "dojo"))))
    :serial t
    :depends-on (:arnesi :ucw :meta-model :split-sequence :contextl :cl-ppcre))

(defsystem :lisp-on-lines.example
    :components (
		 (:file "reddit-example"))

    :depends-on (:lisp-on-lines))
