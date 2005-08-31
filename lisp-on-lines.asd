;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.lisp-on-lines.system)
    (defpackage :coop.tech.lisp-on-lines.system
      (:documentation "ASDF System package for meta-model.")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.lisp-on-lines.system)

(defsystem :meta-model
  :components (
               (:module :src
                :components ((:file "packages")
                             (:file "meta-model" :depends-on ("packages"))))
	       (:module :backend
		 :depends-on (:src)	 
		:pathname "src/backend/"
		:components ((:file "clsql"))))
  :depends-on (:clsql :clsql-pg-introspect))

;; this is no longer used
(defsystem :meta-model-clsql
  :components ()
  :depends-on  (:meta-model :clsql ))

(defsystem :mewa
  :components ((:module :src 
		:pathname "src/mewa/"
		:components 
		  ((:file "mewa")
		   (:file "presentations" :depends-on ("mewa"))
		   (:file "slot-presentations" :depends-on ("presentations")))))
  :depends-on (:ucw :meta-model))
	  
(defsystem :lisp-on-lines
  :components ((:static-file "lisp-on-lines.asd")
	       (:module :src
			:components ((:file "lisp-on-lines")))
	       (:module :components
		:pathname "src/components/"
                :components ((:file "range-list"))))
:depends-on (:meta-model :mewa))
