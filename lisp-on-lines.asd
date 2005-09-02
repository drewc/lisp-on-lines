;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.systems)
    (defpackage :coop.tech.systems
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.systems)


(defsystem :mewa
  :components ((:module :src 
		:pathname "src/mewa/"
		:components 
		((:file "packages")
		 (:file "static-presentations")
		 (:file "mewa")
		 (:file "presentations" )
		 (:file "slot-presentations"))
		:serial t))
  :depends-on (:ucw :meta-model))
	  
(defsystem :lisp-on-lines
  :components ((:static-file "lisp-on-lines.asd")
	       (:module :src
			:components ((:file "packages")
				     (:file "lisp-on-lines"))
			:serial t)
			
	       (:module :components
		:pathname "src/components/"
                :components ((:file "range-list")
			     (:file "ajax"))))
:depends-on (:meta-model :mewa))
