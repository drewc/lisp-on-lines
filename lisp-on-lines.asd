;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.systems)
    (defpackage :coop.tech.systems
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.systems)

(defsystem :lisp-on-lines
  :components ((:static-file "lisp-on-lines.asd")
	       (:module :patches			  
			:components ((:file "yaclml")))
	       (:module :src
			:components ((:file "packages")
				     (:file "special-initargs")
				     (:file "properties")				       
				     (:file "mewa")
				     (:file "validation")
			       
				     (:file "lisp-on-lines")  
				     (:file "defdisplay")
				     (:file "standard-display")
				     (:file "standard-occurence")
				     (:file "standard-attributes")
				     (:file "dojo-attributes")
				     (:file "standard-wrappers")
				     (:file "relational-attributes")

				     (:file "lines")

				     (:file "backwards-compat")
				     (:module :attributes

					      :components ((:file "numbers"))))
			:serial t))
  :serial t
  :depends-on (:arnesi :ucw :meta-model :split-sequence :contextl :cl-ppcre :cl-fad))

(defsystem :lisp-on-lines.example
    :components (
		 (:file "reddit-example"))

    :depends-on (:lisp-on-lines))
