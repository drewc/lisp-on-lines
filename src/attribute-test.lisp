(in-package :lol-test)

(in-suite lisp-on-lines)

(deftest test-attribute-value ()
  (eval 
   '(progn 
     (define-description attribute-test-description ()
       ((attribute-1 :value "VALUE")
	(attribute-2 :function (constantly "VALUE"))))

     (deflayer attribute-test)

     (define-description attribute-test-description ()
       ((attribute-1 :value "VALUE2")
	(attribute-2 :function (constantly "VALUE2")))
       (:in-layer . attribute-test))))

  (let ((d (find-description 'attribute-test-description)))
    (dletf (((described-object d) nil))
    (is (equalp "VALUE" (slot-value (find-attribute d 'attribute-1) 'lol::value)))
		
    (with-active-layers (attribute-test)
      (is (equalp (attribute-value (find-attribute d 'attribute-1))
		  (attribute-value (find-attribute d 'attribute-2))))
      (is (equalp "VALUE2" (attribute-value (find-attribute d 'attribute-1))))))))

(deftest test-attribute-property-inheriting ()
  (test-attribute-value)
  (eval '(progn
	  (deflayer attribute-property-test)
	  (define-description attribute-test-description ()
	    ((attribute-1 :label "attribute1")
	     (attribute-2 :label "attribute2"))
	    (:in-layer . attribute-property-test))))
  (with-active-layers (attribute-property-test)
    (let ((d (find-description 'attribute-test-description)))
      (dletf (((described-object d) nil))
    
	(is (equalp "VALUE" (slot-value (find-attribute d 'attribute-1) 'lol::value)))

	(is (equalp "attribute1" (attribute-label (find-attribute d 'attribute-1))))
	(is (equalp "attribute2" (attribute-label (find-attribute d 'attribute-2))))
		

	(with-active-layers (attribute-test)
	  (is (equalp (attribute-value (find-attribute d 'attribute-1))
		      (attribute-value (find-attribute d 'attribute-2))))
	  (is (equalp "VALUE2" (attribute-value (find-attribute d 'attribute-1)))))))))

(deftest (test-attribute-with-different-class :compile-before-run t) ()
  (eval '(progn 
;;;; We cannot ever redefine this class ic think... 
;;; as attributes are also slot meta-objects.


	  (define-layered-class
		test-attribute-class (lol::standard-attribute)
		((some-slot :initarg :some-slot 
			    :layered t 
			    :layered-accessor some-slot)))
	  
	  (define-description test-attribute-with-different-class-description ()
	    ((attribute-with-different-class :attribute-class test-attribute-class :some-slot "BRILLANT!")))))

  (let* ((d (find-description 'test-attribute-with-different-class-description))

	 (a (find-attribute d 'attribute-with-different-class)))
    (is (eq (class-of a)
	    (find-class 'test-attribute-class)))
    (is (equalp "BRILLANT!" (some-slot a)))))


	      
    


	  