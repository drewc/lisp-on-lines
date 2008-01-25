;;; -*- lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :coop.tech.systems)
    (defpackage :coop.tech.systems
      (:documentation "ASDF System package for Lisp On Lines")
      (:use :common-lisp :asdf))))

(in-package :coop.tech.systems)

(defsystem :lisp-on-lines
  :license 
  "Copyright (c) 2004-2007 Drew Crampsie

Contains portions of ContextL: 
Copyright (c) 2005 - 2007 Pascal Costanza

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the \"Software\"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE."
  :components ((:static-file "lisp-on-lines.asd")
	       
	       (:module :src
			:components ((:file "contextl-hacks")
				     (:file "packages")
				     
				     (:file "rofl")
				     (:file "utilities")
				     
				     (:file "display")
				     
				     (:file "attribute")
				    
				     (:file "description-class")
				     (:file "description")



				    (:module :standard-descriptions
					      :components ((:file "t")
							   (:file "inline")
							   (:file "edit")
							   (:file "symbol")
							   (:file "list")
							   (:file "clos"))
							   
					      :serial t))
				     
				     :serial t))
  :serial t
  :depends-on (:contextl :arnesi :alexandria :postmodern))




(defsystem :lisp-on-lines.test
  :components ((:module :src
			:components ((:file "packages-test")
				     (:file "description-test")
				     (:file "attribute-test")
				     (:file "display-test")
				     (:module :ucw
				      :components ((:file "ucw-test"))
				      :serial t))
			:serial t)
	       (:module :tests
			:components ((:module :bug
					      :components ((:file "0"))))))
  :serial t


  :depends-on (:lisp-on-lines :lisp-on-lines-ucw :stefil))


(if (asdf:find-system :asdf-system-connections nil)
    (asdf:oos 'asdf:load-op :ucw-system-connections)
    (#+sbcl sb-int:style-warn #-sbcl warn "UCW suggests asdf-system-connections in order to optionally integrate some other libraries. See http://www.cliki.net/asdf-system-connections for details and download instructions."))
