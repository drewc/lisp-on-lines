(defpackage :lisp-on-lines
  (:use :arnesi
	:iterate
	:meta-model
	:common-lisp
	:it.bese.ucw
	:clsql
	:contextl)
  (:nicknames :lol :mewa)
  
  (:shadowing-import-from
   :iterate
   :with)

  (:shadowing-import-from
   :clsql
   :time-difference
   :make-time
   :time-ymd
   :date-ymd
   :date
   :get-time
   :time-element
   :time+
   :date-element)
  
  (:export 
   ;;;; CLSQL meta-model/default attributes definers
   ;;;; TODO: should be moved to meta-model,
   ;;;; with lol specific function implemented like the
   ;;;; CLOS meta-model
   :define-view-for-table
   :define-views-for-database


   ;;;; The LoL 3 displays
   #:defdisplay
   #:defdescription
   #:defattribute
   #:display
   #:display*
   #:display-using-description
   #:display-attribute
   #:call-display

   #:standard-attribute
   #:attribute.name

;;;; Standard Layers

   #:editor
   #:one-line
   #:as-string
   #:as-table
;;;; Wrapping layers
   #:wrap-div
   #:wrap-link
   
   #:show-attribute-labels
   ;;;; "Lines", the newest creation.
   :defline

   ;;;; A macro shortcut for creating ucw actions
   :action

   ;;;; Mewa Exports
   :find-occurence

   ;;attributes
   :attributes
   :attribute-value
   :define-attributes
   :with-default-attributes
   :set-default-attributes
   :set-attribute
   :find-attribute

   ;; Some CRUD
   #:crud
   #:crud-editor
   #:crud-viewer
   #:crud-summary
   #:crud-database
   #:instance))