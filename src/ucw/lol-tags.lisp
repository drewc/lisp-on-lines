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


(deftag-macro <lol::%select (&attribute writer accessor 
					(test '#'eql) 
					(key '#'identity)
					name (id (js:gen-js-name-string :prefix "sel"))
                             &allow-other-attributes others
                             &body body)
  "The implementation of <ucw:select and tal tags with a ucw:accessor (or ucw:writer) attribute."
            "You need to supply either an accessor or a writer to <ucw:select"
    (with-unique-names (id-value v val values)
      (let ((writer (or writer `(lambda (,v) (setf ,accessor ,v)))))
        `(let ((%current-select-value ,accessor)
               (%current-select-test ,test)
               (%current-select-key ,key)
               (%select-table nil)
               (,id-value ,id))
          (declare (ignorable %current-select-value %current-select-test %current-select-key
                    %select-table ))
          (<:select :name (register-callback
                           (flet ((get-associated-value (v)
                                    (let ((v (assoc v %select-table :test #'string=)))
                                      (if v
                                          (cdr v)
                                          (error "Unknown option value: ~S." v)))))
			     (lambda (,v) (funcall ,writer (get-associated-value ,v))))
                           :id ,name)
                    :id ,id-value
                    ,@others
                    ,@body)))))

(deftag-macro <lol::%select-action (&attribute writer accessor 
					(test '#'eql) 
					(key '#'identity)
					name (id (js:gen-js-name-string :prefix "sel"))
                             &allow-other-attributes others
                             &body body)
  "The implementation of <ucw:select and tal tags with a ucw:accessor (or ucw:writer) attribute."
            "You need to supply either an accessor or a writer to <ucw:select"
    (with-unique-names (id-value v val values)
      (let ((writer (or writer `(lambda (,v) (setf ,accessor ,v)))))
        `(let ((%current-select-value ,accessor)
               (%current-select-test ,test)
               (%current-select-key ,key)
               (%select-table nil)
               (,id-value ,id))
          (declare (ignorable %current-select-value %current-select-test %current-select-key
                    %select-table ))
          (<:select :name (register-callback
                           (flet ((get-associated-value (v)
                                    (let ((v (assoc v %select-table :test #'string=)))
                                      (if v
                                          (cdr v)
                                          (error "Unknown option value: ~S." v)))))
			     (lambda (,v) (funcall ,writer (get-associated-value ,v))))
                           :id ,name)
                    :id ,id-value
                    ,@others
                    ,@body)))))

(deftag-macro <lol:select (&allow-other-attributes others
                           &body body)
  `(<lol::%select ,@others ,@body))

(deftag-macro <lol::%option (&attribute value &allow-other-attributes others &body body)
  (with-unique-names (value-id)
    (rebinding (value)
      `(let ((,value-id (random-string 10)))
        (push (cons ,value-id ,value) %select-table)
        (<:option :value ,value-id
         ;;NB: we are applying key to both the option value being rendered,
         ;; as well as the selected value(s).
         ;;That was how the code worked previously, I don't know if it is desirable.
         ;;I think the alternative would be to apply the key to ",value" that is
         ;; the option being rendered, and remove the :key argument from find.

         ;;The logical operation we are trying to accomplish is
         ;;(mapcar #'add-selected-attribute
         ;;          (find-all %current-select-value(s)
         ;;                    (list-of-collected-<lol::%option-calls)
         ;;                    :key %current-select-key))
                  :selected (when (find
                                   (funcall %current-select-key ,value) ;key applied to an option
                                   (if nil ;%multiple
                                       %current-select-value
                                       (list %current-select-value))
                                   :test %current-select-test
                                   :key %current-select-key)
                              T)
         ,@others ,@body)))))

(deftag-macro <lol:option (&allow-other-attributes others &body body)
  "Replacement for the standard OPTION tag, must be used with
  <LOL:SELECT tag. Unlike \"regular\" OPTION tags the :value
  attribute can be any lisp object (printable or not)."
  `(<lol::%option ,@others ,@body))
  
  
  

  


			    

