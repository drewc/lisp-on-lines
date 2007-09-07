(in-package :lisp-on-lines)

(defun make-enclosing-package (name)
  (make-package name :use '()))

(defgeneric enclose-symbol (symbol package)
  (:method ((symbol symbol)
            (package package))
   (if (symbol-package symbol)
     (intern (format nil "~A::~A"
                     (package-name (symbol-package symbol))
                     (symbol-name symbol))
             package)
     (or (get symbol package)
         (setf (get symbol package) (gensym))))))

#|
Descriptoons are represented as ContextL classes and layers. To avoid nameclashes with other classes or layers, the name of a description is actually mappend to an internal unambiguous name which is used instead of the regular name.
|#


(defvar *description-definers*
  (make-enclosing-package "DESCRIPTION-DEFINERS"))

(defun defining-description (name)
  "Takes the name of a layer and returns its internal name."
  (case name
    ((t) 't)
    ((nil) (error "NIL is not a valid description name."))
    (otherwise (enclose-symbol name *description-definers*))))


(defmethod initargs.slot-names (class)
  "Returns ALIST of (initargs) . slot-name."
  (nreverse (mapcar #'(lambda (slot)
	      (cons (closer-mop:slot-definition-initargs slot)
		    (closer-mop:slot-definition-name slot)))
	  (closer-mop:class-slots class))))

(defun find-slot-name-from-initarg (class initarg)
  (cdr (assoc-if #'(lambda (x) (member initarg x))
				   (initargs.slot-names class))))
  

;;;!-- TODO: this has been so mangled that, while working, it's ooogly! 
(defun initargs-plist->special-slot-bindings (class initargs-plist)
  "returns a list of (slot-name value) Given a plist of initargs such as one would pass to :DEFAULT-INITARGS."
  (let ((initargs.slot-names-alist (initargs.slot-names class)))
    (loop for (initarg value) on initargs-plist
	  nconc (let ((slot-name
		    ))
		  (when slot-name ;ignore invalid initargs. (good idea/bad idea?)
		    (list slot-name value))))))



