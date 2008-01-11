
(defpackage lisp-on-lines-ucw
  (:documentation "An LoL Layer over ucw.basic")
  (:nicknames #:lol-ucw)
  (:use #:lisp-on-lines #:ucw :common-lisp :arnesi :yaclml :puri)
  (:shadow 
   #:standard-window-component
   #:make-action
   #:standard-action
   #:uri-parse-error
   #:standard-application)

  (:shadowing-import-from :ucw
   #:parent)
  
  (:import-from :ucw
   #:register-action-in-frame
   #:+action-parameter-name+
   #:context.current-frame
   #:uri.query
   #:*current-component*
   #:find-action
   #:service)
		
  (:export 
   ;;; Symbols marked ";*" are not from UCW 
   ;;; but either shadowed or created for lol. 

   #:defcomponent

   #:uri.query
   
   ;; Standard Server
   #:standard-server
   #:startup-server
   #:shutdown-server


   ;; Standard Application
   #:standard-application
   #:register-application
   #:service

   ;; Standard Request Context
   #:*context*
   #:context.current-frame
   #:context.window-component


   ;; Actions
   #:call
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
   #:component
   #:standard-component-class
   
   #:standard-window-component ;*
   #:window-body

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