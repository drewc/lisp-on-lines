(in-package :lisp-on-lines-ucw)

;;; * Lisp on Lines YACLML tags. 

;;; * Utilities

(defun gen-id (string)
  `(js:gen-js-name-string :prefix ,string))

;;; ** ACTION tags

;;; These tags take UCW "actions" and create the appropriate HTML
;;; tag to signal their execution.

(defmacro %with-action-unique-names (&body body)
  "These magic macros."
  `(with-unique-names (url action-object action-id current-frame)
    (assert (xor action action* function) nil
	    "Must supply only one of ACTION,  ACTION* or FUNCTION")
    (rebinding (id)
      `(let* ((,current-frame (context.current-frame *context*)) 
	      (,action-object  ,(or action* 
				   `(lol-ucw:make-action 
				     ,(or function
					  `(lambda ()
					     (with-call/cc ,action))))))
	      (,action-id  (register-action-in-frame 
			   ,current-frame 
			   ,action-object))
						      
				
              (,url (compute-url ,action-object *current-component*)))
	 (declare (ignorable ,action-id ,url))
         ,,@body))))


(deftag-macro <lol:a (&attribute (id (gen-id "lol-action"))
				    action action* function
				    &allow-other-attributes others
				    &body body)
  "A Simple <:A which does not require javascript."
  (%with-action-unique-names 
   `(<:a :href (print-uri-to-string ,url)
	 :id ,id
	   ,@others
	   ,@body)))

(deftag-macro <lol:form (&attribute (id (gen-id "lol-form"))
				    action action* function
				    &allow-other-attributes others
				    &body body)
  "A Simple form which does not require javascript. "
  (%with-action-unique-names 
   `(<:form :action (print-uri-to-string-sans-query ,url)
	   :id ,id
	   ,@others
	   (dolist (query (uri.query ,url))
	     (if (string= ,+action-parameter-name+ (car query))
		 (<:input :type "hidden" :name ,+action-parameter-name+
			  :value (cdr query)
			  :id ,action-id)
	       (<:input :type "hidden" :name (car query) :value (cdr query))))
	   ,@body)))

(deftag-macro <lol:submit (&attribute (id (gen-id "lol-submit"))
				    action action* function value
				    &allow-other-attributes others
				    &body body)
  (%with-action-unique-names 
    `(<:input :type "submit" 
	      :value (or ,value ,@body)
	      :name (format nil "~A~A~A" 
			    ,+action-parameter-name+
			    ,+action-compound-name-delimiter+
			    ,action-id))))

;;; * CALLBACK tags

;;; All these tags take some kind of input, and execute a UCW callback.

(deftag-macro <lol:input (&attribute accessor reader writer
                                     (id (gen-id "lol-input"))
                          &allow-other-attributes others)
  (let ((reader (or reader accessor))
	(writer (or writer `(lambda (v)
			      (setf ,accessor v)))))
    
  `(<:input :value ,reader
	    :name (register-callback ,writer)
	    ,@others)))
  
  
  

  


			    

