
(defpackage lisp-on-lines-ucw
  (:documentation "An LoL Layer over ucw.basic")
  (:nicknames #:lol-ucw)
  (:use #:lisp-on-lines #:ucw-core :common-lisp :arnesi :yaclml :js :contextl)

  (:shadowing-import-from :js
   #:new)  
  (:shadowing-import-from :ucw-core
   #:parent )
  (:import-from :ucw-standard 
	#:call #:answer	#:defaction #:*source-component*)
  

		
  (:export 

   ;;; First, LOL-UCW exports. The rest are from UCW.
   #:lol-component
   #:*source-component*
   #:defcomponent

   #:uri.query
   
   ;; Standard Server
   #:standard-server
   #:startup-server
   #:shutdown-server


   ;; Sessions
   #:get-session-value
   ;; Standard Application
   #:standard-application
   #:register-application
   #:service

   ;; Standard Request Context
   #:*context*
   #:context.current-frame
   #:context.window-component
   #:*current-component*

   ;; Actions
   #:call
   #:answer
   #:make-action
   #:find-action
   #:defaction
   #:defmethod/cc

   #:call-component
   #:answer-component

   ;; Entry Points   
   #:defentry-point

   ;; Standard Components
   #:render
   #:render-html-body
   #:component

   #:standard-component-class
   #:described-component-class

   #:container
   #:find-component
   
   #:standard-window-component ;*
   #:window-body
   #:info-message

   ))

(defpackage :lisp-on-lines-tags
  (:documentation "LoL convience yaclml tags.")
  (:use)
  (:nicknames #:<lol)
  (:export
   #:component-body
   #:render-component
   #:a
   #:area
   #:form
   #:input
   #:button
   #:simple-select
   #:select
   #:option
   #:textarea

   #:integer-range-select
   #:month-day-select
   #:month-select

   #:text
   #:password
   #:submit
   #:simple-form
   #:simple-submit

   #:localized
   #:script))