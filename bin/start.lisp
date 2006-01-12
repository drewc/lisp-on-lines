;; -*- lisp -*-

(in-package :common-lisp-user)

#+cmu
(defun init-cmu-mp ()
  ;; this isn't strictly necessary, but scheduling feels very coarse
  ;; without startup-idle-and-top-level-loops, leading to answer delays
  ;; of about 1s per request.
  (unless (find-if
           #'(lambda (proc) (string= (mp:process-name proc) "Top Level Loop"))
           (mp:all-processes))
    (mp::startup-idle-and-top-level-loops)))

#+cmu
(init-cmu-mp)

;;;; * UCW server initialization "script" 

;;;; This file is meant to be loaded by ucwctl, but you can use it a
;;;; general "startup ucw" file as well. You should customize this
;;;; script to load/prepare your application.

;;;; ** Loadup dependencies

;;;; Load arnesi first so we can set arnesi::*call/cc-returns* before
;;;; ucw is compiled and loaded.
(asdf:oos 'asdf:load-op :arnesi)
(setf arnesi::*call/cc-returns* nil)

;;;; Load up UCW itself
(asdf:oos 'asdf:load-op :ucw)

(in-package :it.bese.ucw-user)

#+(and sbcl sb-unicode)
(setf (external-format-for :slime) :utf-8-unix
      (external-format-for :url)   :utf-8
      (external-format-for :http-emacsen)  :utf-8-unix
	  (external-format-for :http-lispish)  :utf-8)

;;;; Load the default applications systems

(asdf:oos 'asdf:load-op :ucw.examples)
(asdf:oos 'asdf:load-op :ucw.admin)
(asdf:oos 'asdf:load-op :lisp-on-lines)
(asdf:oos 'asdf:load-op :lisp-on-lines.example)

;;;; Let there be swank.
(swank:create-server :port 4007)

;;;; Finally startup the server

;;;; ** Finally startup the server

(ucw:create-server :backend :araneida

		   ;; :httpd
		   ;; :mod-lisp
		   ;; :aserve
                   :host "merlin.tech.coop"
                   :port 8082
                   :applications (list 
                                       lol::*lol-example-application*)
                   :inspect-components nil
                   :log-root-directory (make-pathname :name nil :type nil
                                                      :directory (append (pathname-directory *load-truename*)
                                                                         (list :up "logs"))
                                                      :defaults *load-truename*)
                   :log-level +info+
                   :start-p t)

;;;; ** Allocate one database connection per thread :

(defmethod araneida:handle-request-response :around ((handler ucw::ucw-handler) method request)
  (clsql:with-database (my-db '("localhost" "lol" "lol" "lol") :pool t)
    (clsql:with-default-database (my-db)
      (call-next-method))))

(publish-directory (server.backend *default-server*) #P"/home/drewc/src/site/lisp-on-lines/wwwroot/dojo/" "/dojo/")
(publish-directory (server.backend *default-server*) #P"/home/drewc/src/site/lisp-on-lines/wwwroot/prototype/" "/prototype/")

(publish-directory (server.backend *default-server*) #P"/home/drewc/src/sunrise/wwwroot/" "/")



