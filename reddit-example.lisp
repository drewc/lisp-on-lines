(in-package :lol)

(defvar *lol-example-application*
  (make-instance 'cookie-session-application
                 :url-prefix "/lisp-on-lines/"
                 :tal-generator (make-instance 'yaclml:file-system-generator
                                               :cachep t
                                               :root-directories (list *ucw-tal-root*))
                 :www-roots (list (merge-pathnames "./" *ucw-tal-root*))
                 :debug-on-error t))

(defentry-point "reddit" (:application *lol-example-application*) ()
  (call 'front-page))

(defcomponent front-page (simple-window-component)
  ()
  (:default-initargs
      :javascript "/dojo/dojo.js"))

(defmethod render ((self front-page))
  (with-component (self)
    (<:h1 (<:as-html "Lisp on Lines : Reddit Example"))
  
    (<ucw:a :action (add-link self)
	    (<:as-html "Add Lispy Link"))
    (<:div
     :class "main"
     (display (find-links)
	      :attributes '(link
			    (submitter :label "Submitted By :")
			    (score :label "Score :")
			    buttons)))))

(defclass/meta link ()
  ((url :accessor url :initarg :url :type string)
   (title :accessor title :initarg :title :type string)
   (submitter :accessor submitter :initarg :submitter :type string)
   (score :accessor score :initarg :score :type integer :initform 0)))

(define-attributes (link)
  (link link :label "")
  (buttons score-buttons :label ""))

(defvar *links* (list))

(defaction add-link ((self component))
  (let ((l (call-display (make-instance 'link)
		:type 'editor)))
    (when l (push l *links*))))

(defun find-links ()
  (sort (copy-list *links*) #'> :key #'score))

(defattribute link-attribute ()
  ()
  (:type-name link))

(defdisplay (:description (link link-attribute))
  (<:a :href (url object)
       (<:as-html (title object))))

(defattribute score-buttons ()
  ()
  (:type-name score-buttons))

(defdisplay (:description (score score-buttons))
  (<ucw:a
   :action (incf (score object))
	  (<:as-html "Up " ))
    (<ucw:a
     :action (decf (score object))
	  (<:as-html " Down" )))


(defdisplay (:combination :around :in-layer editor :class link)
  (with-component (component)
    
    (<ucw:form
     :action (refresh-component component)
     (<:h2 (<:as-html "Add a new Link"))
     (call-next-method)
     (<ucw:submit
      :action (answer link)
      :value "Ok")
     (<ucw:submit
      :action (answer nil)
      :value "Cancel"))))


;;;;  We are going to use a POSTGRES database.
;;;;  It's a good idea to have created it already.

;; template1=# CREATE USER lol PASSWORD 'lol';
;; CREATE USER
;; template1=# CREATE DATABASE lol OWNER lol; 
;; CREATE DATABASE
;; template1=# 


