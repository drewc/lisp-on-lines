(in-package :lol-test)

(defsuite :mao)
(in-suite :mao)

(defdescription test-empty-description ())

(defdescription property-speed-test ()
  ((attribute :value t)))

(defdescription property-speed-test ()
  ((attribute :value t))
  (:in-description test-empty-description))

(defdescription property-speed-test-many-attributes ()
  ((attribute :value t)
   (attribute2 :value t)
   (attribute3 :value t)
   (attribute4 :value t)
   (attribute5 :value t)
   (attribute6 :value t)
   (attribute7 :value t)
   (attribute8 :value t)
   (attribute9 :value t)
   (attributea :value t)
   (attributeb :value t)
   (attributec :value t)
   (attributed :value t)
   (attributee :value t)
   (attributef :value t)
   (attributeg :value t)
   (attributeh :value t)
   (attributei :value t)
   (attributej :value t)
   (attributek :value t)
   (attributel :value t)
   (attributem :value t)
   (attributen :value t)
   (attributeo :value t)
   )
  )




(defun attribute-property-speed-test (n &optional (description 'property-speed-test) (attribute 'attribute))
  (with-described-object (nil (find-description description))
    (let ((attribute (find-attribute (current-description) 'attributeo)))
      
      (loop repeat n do (attribute-value attribute)))))

