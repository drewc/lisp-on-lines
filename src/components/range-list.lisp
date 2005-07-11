(defgeneric make-range-list-generator (instance &key query chunk-size &allow-other-keys)
  (:documentation "Produced generator must obeys the following interface:
GENERATOR (:first|:last|:next|:previous|:current &optional offset) =>
 (ITEMS LIST)
GENERATOR :offset &optional (offset integer) => (new-offset integer)
GENERATOR :chunk-size (size integer) => (new-chunk-size integer)
GENERATOR :chunks => (total-number-of-chunks integer)
GENERATOR (:have-previous-p|:have-next-p) => (v boolean)."))

(defmethod make-range-list-generator ((instance clsql:standard-db-object) &key query (chunk-size 20) (offset 0))
  (let ((view-class (class-of instance))
        (current-offset offset)
        (last-select-size 0))
    (labels ((guess-total-size ()
               (car
                (apply #'clsql:select
                       (clsql:sql-count (clsql:sql-expression :attribute '*))
                       :from (clsql:sql-expression :table (slot-value view-class 'clsql:view-table))
                       (append query '(:flatp t)))))
             (select-items (offset size)
               (apply #'clsql:select (class-name view-class)
                      (append query 
                              `(:limit ,(+ size 1) :offset ,(* (- offset 1) size) :flatp t))))
             (chunks ()
               (multiple-value-bind (q rem)
                   (floor (guess-total-size) chunk-size)
                 (if (zerop rem) q (+ q 1)))))
      (lambda (cmd &optional num)
        (setf current-offset
              (case cmd
                (:first 1)
                (:last (chunks))
                (:next (+ 1 current-offset))
                (:previous (max 1 (- current-offset 1)))
                ((:current :offset) (if num (max 1 num) current-offset))
                (otherwise current-offset)))
        (ecase cmd
          ((:first :last :next :previous :current)
           (let ((items (select-items current-offset chunk-size)))
             (setf last-select-size (length items))
             (when (> last-select-size chunk-size)
               (nbutlast items))
             items))
          (:chunks (chunks))
          (:chunk-size (when num (setf chunk-size num))
                       chunk-size)
          (:offset current-offset)
          (:have-previous-p (> current-offset 1))
          (:have-next-p (> last-select-size chunk-size)))))))

(defcomponent range-list (mewa::mewa-list-presentation)
  ((offset :accessor range-list.offset
           :initform 0
           :backtrack t
           :documentation "Which of the windows we're currently looking at.")
   (window-size :accessor range-list.window-size :initform 20 :initarg :window-size)
   (generator :reader range-list.generator)
   (generator-args :reader range-list.generator-args :initarg generator-args :initform nil))
  (:documentation "Component for showing the user a set of data one \"window\" at a time.

The data set is presented one \"window\" at a time with links to
the the first, previous, next and last window. Each window shows
at most WINDOW-SIZE elements of the data.
The GENERATOR is used to get a data to display every time.
It is produced by MAKE-RANGE-LIST-GENERATOR as
MAKE-RANGE-LIST-GENERATOR INSTANCE :chunk-size WINDOW-SIZE GENERATOR-ARGS"))

(defmethod range-list.generator :before ((self range-list))
  (unless (slot-boundp self 'generator)
    (create-generator self)))

(defmethod create-generator ((self range-list) &rest args)
  (with-slots (instance generator generator-args window-size offset)
      self
    (when args
      (setf generator-args args))
    (setf generator
          (apply 'make-range-list-generator instance :chunk-size window-size generator-args)
          offset 0)
    (funcall generator :offset offset)))

(defmethod range-list.have-previous-p ((self range-list))
  "Returns true if we have a window before the current one."
  (funcall (range-list.generator self) :have-previous-p))

(defmethod range-list.have-next-p ((self range-list))
  "Returns true if we have a window after the current one."
  (funcall (range-list.generator self) :have-next-p))

(defmethod range-list.fetch-items ((self range-list) op)
  (prog2
      (ecase op ((:first :last :current :next :previous) t))
      (funcall (range-list.generator self) op)
    (setf (range-list.offset self)
          (funcall (range-list.generator self) :offset))))

(defaction scroll ((self range-list) op)
    (funcall (range-list.generator self) :offset (range-list.offset self))
    (setf (mewa::instances self)
          (range-list.fetch-items self op)))

(defaction scroll-to-page ((self range-list) window-number)
  (setf (range-list.offset self) window-number)
  (scroll self :current))

(defmethod present ((self range-list))
  (when (zerop (range-list.offset self))
    (scroll self :current))
  (<:table :class (css-class self)
     (<:tr
        (<:td (call-next-method)))
     (<:tr 
        (<:td
         (<:table :class "range-list-navigation"
           (<:tr
             (<:td
               (<ucw:a :action (scroll self :first)
                       (<:tt (<:as-html "<<"))))
             (<:td
              (if (range-list.have-previous-p self)
                  (<ucw:a :action (scroll self :previous)
                          (<:tt (<:as-html "<")))
                  (<:tt (<:as-html "<"))))
             (<:td
              (if (range-list.have-next-p self)
                  (<ucw:a :action (scroll self :next)
                          (<:tt (<:as-html ">")))
                  (<:tt (<:as-html ">"))))
             (<:td
              (<ucw:a :action (scroll self :last)
                      (<:tt (<:as-html ">>"))))))))))
