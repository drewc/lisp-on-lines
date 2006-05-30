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
   :defdisplay
   :defattribute
   :display
   :display-using-description
   :call-display

   ;;;; Standard Layers

   :editor
   :one-line
   :as-string
   :as-table
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

   ;;;; Meta Model Exports))
   :define-meta-model
   :def-view-class-from-table
   :def-view-class/meta
   :list-slot-types
   ))