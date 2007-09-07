(in-package :lol-test)

(in-suite lisp-on-lines)

(deftest test-define-display ()
  (test-attribute-property-inheriting)

  (deflayer test-display)

  (define-display 
    :in-layer test-display ((description attribute-test-2))
    (format *display* "BRILLANT!"))

  (let ((before (display-using-description 
		 (find-description 'attribute-test-2) 
		 nil *object*)))
    (with-active-layers (test-display)
      (is (equalp "BRILLANT!" (display-using-description 
			       (find-description 'attribute-test-2) 
			       nil *object*))))))
		  