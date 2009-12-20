(in-package :lol-test)

(in-suite lisp-on-lines)

(deftest test-attribute-value ()
  (eval 
   '(progn 
     (define-description attribute-test-description ()
       ((attribute-1 :value "VALUE")
	(attribute-2 :function (constantly "VALUE"))))

     (define-description attribute-test)

     (define-description attribute-test-description ()
       ((attribute-1 :value "VALUE2")
	(attribute-2 :function (constantly "VALUE2")))
       (:in-description attribute-test))))
  
  (funcall-with-described-object 
   (lambda (&aux 
	    (a1 (find-attribute *description* 'attribute-1))
	    (a2 (find-attribute *description* 'attribute-2))
	    )
     (is (equalp "VALUE" (attribute-value a1)))
     (is (equalp "VALUE" (attribute-value a2)))
     (with-active-descriptions (attribute-test)
       (is (equalp "VALUE2" (attribute-value a1)))
       (is (equalp "VALUE2" (attribute-value a2)))))
   nil 
   (find-description 'attribute-test-description)))

(deftest test-attribute-property-inheriting ()
  (test-attribute-value)
  (eval '(progn
	  (define-description attribute-property-test)
	  (define-description attribute-test-description ()
	    ((attribute-1 :label "attribute1")
	     (attribute-2 :label "attribute2"))
	    (:in-description attribute-property-test))))
  
  (with-active-descriptions (attribute-property-test)
    (with-described-object (nil (find-description 'attribute-test-description))
          (let ((d (dynamic description)))
	    	(is (equalp "VALUE" (slot-value (find-attribute d 'attribute-1) 'lol::value)))

	(is (equalp "attribute1" (attribute-label (find-attribute d 'attribute-1))))
	(is (equalp "attribute2" (attribute-label (find-attribute d 'attribute-2))))
		

	(with-active-descriptions (attribute-test)
	  (is (equalp (attribute-value (find-attribute d 'attribute-1))
		      (attribute-value (find-attribute d 'attribute-2))))
	  (is (equalp "VALUE2" (attribute-value (find-attribute d 'attribute-1)))))))
))

(deftest (test-attribute-with-different-class :compile-before-run t) ()
  (eval '(progn 
	  (define-layered-class
	      test-attribute-class (standard-attribute)
	      ((some-slot :initarg :some-slot 
			  :layered t
			  :special t
			  :layered-accessor some-slot)))
	  
	  (define-description test-attribute-with-different-class-description ()
	    ((attribute-with-different-class :attribute-class test-attribute-class :some-slot "BRILLANT!")))))

  (let* ((d (find-description 'test-attribute-with-different-class-description))

	 (a (find-attribute d 'attribute-with-different-class)))
    (is (eq (class-of a)
	    (find-class 'test-attribute-class)))
    (is (equalp "BRILLANT!" (some-slot a)))))

(deftest (test-attribute-with-different-class-and-subclassed-description :compile-before-run t) ()
  (test-attribute-with-different-class)
  (eval '(progn 	  
	  (define-description test-attribute-with-different-class-description-sub 
	      (test-attribute-with-different-class-description)
	    ())))

  (let* ((d (find-description 'test-attribute-with-different-class-description-sub))

	 (a (find-attribute d 'attribute-with-different-class)))
    (is (eq (class-of a)
	    (find-class 'test-attribute-class)))
    (is (equalp "BRILLANT!" (some-slot a)))))


	      
    


	  