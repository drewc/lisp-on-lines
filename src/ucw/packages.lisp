
(defpackage lisp-on-lines-ucw
  (:documentation "An LoL Layer over ucw.basic")
  (:nicknames #:lol-ucw)
  (:use #:lisp-on-lines #:ucw :ucw-core :common-lisp :arnesi)

  (:shadowing-import-from :js
   #:new)  
  (:shadowing-import-from :ucw-core
   #:parent )
  (:import-from :ucw-standard 
	#:call #:answer	#:defaction #:*source-component*)
  

		
  (:export 

   ;;; First, LOL-UCW exports. The rest are from UCW.
   #:lol-component
   
   #:described-component-class))

