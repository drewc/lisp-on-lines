(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.systems)
    (defpackage :coop.tech.systems
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.systems)

(defsystem :lisp-on-lines-ucw
  :components ((:module :src
			:components
			((:module :ucw
				  :components ((:file "packages")
					       (:file "standard-components")
					       (:file "lol-tags"))
			
				  :serial t))))
  :serial t


  :depends-on (:lisp-on-lines :ucw :puri))