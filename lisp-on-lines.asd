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
  :depends-on (:clsql))

;; this is no longer used
(defsystem :meta-model-clsql
  :components ()
  :depends-on  (:meta-model :clsql))

(defsystem :mewa
  :components ((:module :src 
		:pathname "src/"
		:components 
		  ((:file "mewa")
		   (:file "ucw" :depends-on ("mewa")))))
  :depends-on (:ucw :meta-model))
	  
(defsystem :lisp-on-lines
  :components ((:static-file "lisp-on-lines.asd"))
:depends-on (:meta-model :mewa))
