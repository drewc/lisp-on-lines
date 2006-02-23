(in-package :lol)


;;;; * Dojo Javascript Components
(defcomponent dojo-component ()
  ((requires :accessor requires :initarg :requires :initform nil)))

(defmethod render-requires ((self dojo-component))
  (<ucw:script `(progn ,@(loop for r in (requires self)
			       collect `(dojo.require ,r)))))

(defmethod render :wrapping ( (self dojo-component))
  (render-requires self)
  (call-next-method))

(defmethod lol::present :around ((self dojo-component))
  (render-requires self)
  (call-next-method))

(defcomponent dojo-ajax-output-component (window-component)
  ((component :accessor component :initarg :component :component dojo-component)))

(defmethod render ((self dojo-ajax-output-component))
  (lol::present self))

(defmethod lol::present ((self dojo-ajax-output-component))
  (present-output (component self)))

(defcomponent dojo-input-component-mixin ()
  ((input-id
    :accessor input-id
    :initform (arnesi:random-string 32 arnesi::+ALPHANUMERIC-ASCII-ALPHABET+))))

(defcomponent dojo-output-component-mixin ()
  ((output-id
    :accessor output-id
    :initform (arnesi:random-string 32 arnesi::+ALPHANUMERIC-ASCII-ALPHABET+))
   (output-component
    :accessor output-component
    :component dojo-ajax-output-component)))

(defmethod shared-initialize :after ((self dojo-output-component-mixin) slots &rest args)
  (declare (ignore slots args))
  (setf (component (output-component self)) self))


(defmacro with-ajax ((component) &body args)  
  (multiple-value-bind (actions callbacks args output)
      (loop for arg in args
	    if (eql (car arg) :action)
	    nconc (cdr arg) into actions
	    else if (eql (car arg) :callback)
	    collect (cdr arg) into callbacks
	    else if (eql (car arg) :output-to)
	    nconc (cdr arg) into output
	    else
	    nconc arg into args
	    finally (return (values actions callbacks args output)))
      `(js:with-unique-js-names (js-callbacks)
	`(progn
	  (setf ,js-callbacks (array))
	  ,,@(loop for c in callbacks
		   for i upfrom 0
		   collect 
		   ``(setf (aref ,js-callbacks ,,i)
		      (lambda () ,,(third c))))
	  (dojo.io.bind
		(create
		 ,@(unless
		    ,(getf args :url)
		    `(:url
		      ,(lol::make-action-url
			,component
			(progn
			  ,@actions))))
		 ,@ (unless
			,(getf args :post-content)
		      `(:post-content (+ ,,@(loop for c in callbacks
						  for n upfrom 0
						  nconc `((ucw::make-new-callback
							   
							   (lambda (,(car c))
							     ,(second c)))
							  "="
							  `(encode-u-r-i-component ((aref ,js-callbacks ,,n)))
							  "&")))))
		 ,@ (unless
			,(or (getf args :load) (not output) ) 
		      `(:load
			(lambda (evt data)
			  (setf (slot-value (document.get-element-by-id ,,@output) inner-h-t-m-l) data))))
		 ,,:method "post"
		 ,,@args))))))





;;;; ** Editor

(defcomponent dojo-editor (dojo-component dojo-input-component-mixin)
  ((document :accessor document :initarg :document :initform "test"))
  (:default-initargs
      :requires '("dojo.event.*" "dojo.widget.Editor" "dojo.io.*" "dojo.widget.RichText")))

(defmethod save-document ((self dojo-editor))
  t)

(defmethod js-on-load ((self dojo-editor))
  `(lambda (x)
    (setf document.location
	  ,(lol::make-action-url
	    self
	    (answer self)))))

(defmethod render-editor ((self dojo-editor))
  (<ucw:script
   `(dojo.add-on-load
     (lambda ()
       (setf div (document.get-element-by-id ,(input-id self)))
       (setf editor (dojo.widget.from-script
		     "Editor"
		     (create) div))
       (setf save
	     (create
	      :save-to-server
	      (lambda ()
		(dojo.io.bind
		 (create
		  :method "post"
		  :post-content (+
				 ,(ucw::make-new-callback
				   
				   (lambda (x)
				     (setf (document self) x)))
				 "="
				 (encode-u-r-i-component (editor.get-html)))
		  :url 
		  ,(lol::make-action-url
		    self
		    (save-document self))
				     
		  :load ,(js-on-load self))))))
       (dojo.event.kw-connect
	(create :type "before"
		:src-obj editor
		:src-func "onSave"
		:target-obj save
		:target-func "saveToServer")))))
  (<:div :class "editor"
	 (<:div
   :id (input-id self)
   (<:as-is (document self)))))

(defmethod render ((self dojo-editor))
  (render-editor self))

(defcomponent dojo-editor-presentation (dojo-editor mewa::mewa-editor)
  ())

(lol::defslot-presentation dojo-editor-slot-presentation (dojo-editor mewa::mewa-string-slot-presentation)
  ((document :accessor document :initarg :document)
   (instance :accessor instance))
  (:type-name dojo-editor))

(defmethod save-document ((self dojo-editor-slot-presentation))
  (setf (lol::presentation-slot-value self (instance self)) (document self)))

(defmethod lol::present-slot ((slot dojo-editor-slot-presentation) instance)
  (setf (document slot) (lol::presentation-slot-value slot instance))
  (setf (instance slot) instance)
  (render-requires slot)
  (render-editor slot))

(defmethod js-on-load ((self dojo-editor-slot-presentation))
 `(lambda (x)
    (setf document.location
	  ,(lol::make-action-url
	    self
	    (answer-component (ucw::parent self) self)))))


(defcomponent sortable-list-editor (lol::mewa-list-presentation
				    dojo-component
				    dojo-input-component-mixin
				    dojo-output-component-mixin)
  ()
  (:default-initargs
      :requires '("dojo.event.*" "dojo.dnd.*" "dojo.io.*")))

(defmethod present-output ((self sortable-list-editor))
  (loop for li in (mewa::instances self)
	 for n upfrom 0
	 do
	 (let ((li li))
	   (<:li :id (format nil "~A~A" (input-id self) n)
		 (<:as-html (lol:present-view (li :one-line)))
		 (<:br)
		 (<ucw:a :action (lol:call-view (li :editor (call-from self)))
			 (<:as-html "(edit)"))
		 (<ucw:a :action (lol:call-view (li :editor))
			 (<:as-html "(remove)"))))))

(defmethod lol::present ((self sortable-list-editor))
  (<:div (<:as-html "Drag and Drop list items to change the order"))
  (<:ul
   :id (input-id self)
   (present-output self))
  (<:ul (<:li
	 (<ucw:a :action (answer (mewa::instances self))
		 (<:as-html "*Save*")))
   
	(<:li 
	 (<ucw:a :action (add-list-item self)
		 (<:as-html "*Add Item*")))
	(<:li 
	 (<ucw:a :action (answer nil)
		 (<:as-html "*Cancel*"))))

  (<ucw:script
   ;;;; The Dojo example :
   ;;;; var dl = byId("dragList3");
   ;;;; new dojo.dnd.HtmlDropTarget(dl, ["li2"]);
   ;;;; var lis = dl.getElementsByTagName("li");
   ;;;; for(var x=0; x<lis.length; x++){
   ;;;;   new dojo.dnd.HtmlDragSource(lis[x], "li2");}

   ;;;; and the parenscript	
   `(dojo.event.connect dojo "loaded"
     (lambda ()
       (setf make-sortable
	     (lambda (x)
	       (setf ulist (document.get-element-by-id x))
	       (setf drop (new (dojo.dnd.*html-drop-target ulist (array x))))
	       (setf list-items (ulist.get-elements-by-tag-name "li" ))
	       (dolist (li list-items)
		 (new (dojo.dnd.*html-drag-source li x)))))
       (make-sortable ,(input-id self))
       
       (dojo.event.connect
	drop "onDrop"
	(lambda ()
	  (dolist (li list-items)
	    (new (dojo.dnd.*html-drag-source li ,(input-id self))))
	  ,
	  (with-ajax (self)
	    (:action nil)
	    (:callback d (let ((list-order
				(mapcar #'(lambda (x)
					    (parse-integer (subseq x (length (input-id self)))))
					(read-from-string d))))
			   (setf (mewa::instances self) (reorder-list (mewa::instances self) list-order))) 
		       `(progn
			 (setf my-list "(")
			 (dolist (li list-items)
			   (setf my-list (+ my-list "\"" li.id "\"" " ")))
			 (setf my-list (+ my-list ")"))
			 (return my-list)))
	    (:load `(lambda (x data)
		     (setf (slot-value (document.get-element-by-id ,(input-id self)) inner-h-t-m-l) data)
		     (make-sortable ,(input-id self)))))))))))


;(defcomponent dojo-combo-box )