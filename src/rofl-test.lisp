(in-package :lol-test)

;;;; CREATE USER rofl_test PASSWORD 'rofl_test';
;;;; CREATE DATABASE rofl_test OWNER rofl_test;


(defmacro db (&body body)
 `(postmodern:with-connection '("rofl_test" "rofl_test" "rofl_test" "localhost")
    ,@body))
   
(deftest test-create-table ()
  (finishes (db 
    (postmodern:query (:DROP-TABLE 'rofl_test_base))

    (postmodern:query (:CREATE-TABLE rofl_test_base 
		       ((rofl_test_base_id :type SERIAL :primary-key t)
                        (test_string :type string) 
			(test_integer :type integer)))))))

(deftest test-simple-insert ()
  (test-create-table)
  (let ((plist '(test-string "Test Entry" test-integer 1)))
    (finishes (db
		(postmodern:execute 
		 (postmodern:sql-compile  `(:insert-into rofl-test-base :set ,@plist)))))))

(deftest test-rofl-select ()
  (test-simple-insert)
  (db 
  (finishes 
    (let* ((result (first (select '* :from 'rofl-test-base))))
      (is (equalp '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "Test Entry" :TEST-INTEGER 1) result))))))

(deftest test-rofl-select-only-1 ()
  (test-simple-insert)
  (db 
  (finishes 
    (let* ((result (select-only 1 '* :from 'rofl-test-base)))
      (is (equalp '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "Test Entry" :TEST-INTEGER 1) result))))))

(deftest test-rofl-insert ()
  (test-create-table)
  (db 
    (finishes (insert-into 'rofl-test-base :test-integer 2 :test-string "a"))
    (finishes (insert-into 'rofl-test-base :test-integer 3 :test-string "b"))
    (finishes (insert-into 'rofl-test-base :test-integer 4 :test-string "c"))
    
    (let ((r (select '* :from 'rofl-test-base)))
      (is (equal 3 (length r))))))

(deftest test-rofl-class-creation ()
  (finishes (eval '(progn 
		    (setf (find-class 'rofl-test-base) nil)
		    (defclass rofl-test-base ()
		      ((rofl-test-base-id :primary-key t)
		       test-integer test-string)
		      (:metaclass standard-db-access-class))))))


(deftest test-rofl-make-object-from-plist ()
  (test-rofl-class-creation)
  (let* ((plist '(:ROFL-TEST-BASE-ID 1 :TEST-STRING "a" :TEST-INTEGER 2))
	 (object (make-object-from-plist 'rofl-test-base plist)))
    (is (equal (slot-value object 'rofl-test-base-id) 1))))
    

(deftest test-rofl-select-objects ()
  (test-create-table)
  (test-rofl-class-creation)
  (test-rofl-insert)

  (db (finishes 
    (let ((objects (select-objects 'rofl-test-base  
				 :where '(:= rofl-test-base-id 1))))
      (is (equal (slot-value (first objects) 'rofl-test-base-id) 1))))))

(deftest test-rofl-create-references-tables ()
  (finishes 
    (db 
      (ignore-errors (postmodern:query (:DROP-TABLE 'rofl_test_child)))
      (ignore-errors (postmodern:query (:DROP-TABLE 'rofl_test_parent)))
      
      (postmodern:query (:CREATE-TABLE rofl_test_parent 
				       ((rofl_test_parent_id 
					 :type SERIAL 
				 	 :primary-key t)
					(test_string 
					 :type string) 
						(test_integer 
						 :type integer))))
    


	      (postmodern:query (:CREATE-TABLE rofl_test_child 
					       ((rofl_test_child_id 
						 :type SERIAL 
						 :primary-key t)
						(rofl_test_parent_id 
						 :type integer
						 :references (rofl_test_parent))
						(test_string 
						 :type string) 
						(test_integer 
						 :type integer)))))))

(deftest test-rofl-def-references-classes ()
  (finishes 
    (eval 
     '(progn
       (defclass rofl-test-parent ()
	 ((rofl-test-parent-id 
	  :primary-key t)
	  (test-string)
	  (test-integer))
	 (:metaclass standard-db-access-class))

       ;;; three ways to get to the parent.
       ;;; The should all point to the same object.

       (defclass rofl-test-child ()
	 ((rofl-test-child-id 
	  :primary-key t)       ((rofl_test_child_id 
						 :type SERIAL 
						 :primary-key t)
						(rofl_test_parent_id 
						 :type integer
						 :references (rofl_test_parent))
						(test_string 
						 :type string) 
						(test_integer 
						 :type integer)))))))

)


(deftest test-rofl-def-references ()
  (finishes 
    (eval 
     '(progn
       (defclass rofl-test-parent ()
	 ((rofl-test-parent-id 
	  :primary-key t)
	  (test-string)
	  (test-integer))
	 (:metaclass standard-db-access-class))

       ;;; three ways to get to the parent.
       ;;; The should all point to the same object.

  (test-rofl-def-references-classes)
  (db 
  (finishes 
    (insert-into 'rofl-test-parent :test-string "Parent" :test-integer 1)
    (insert-into 'rofl-test-child :test-string "Child 1" :test-integer 1
		 :rofl-test-parent-id 
		 (slot-value (first (select-objects 'rofl-test-parent)) 'rofl-test-parent-id)))
  (let* ((child (select-only-n-objects 1 'rofl-test-child))
	 (parent-same-slot-name/fkey (slot-value child 'rofl-test-parent-id))
	 (parent-column-same-fkey (slot-value child 'parent))
	 (parent-column-table-and-key (slot-value child 'same-parent)))

    (is (eql 1 (slot-value child 'test-integer)))
    
    (is (equal 1 (slot-value parent-same-slot-name/fkey 'test-integer)))
    (is (equal 1 (slot-value parent-column-same-fkey 'test-integer)))
    (is (equal 1 (slot-value parent-column-table-and-key 'test-integer)))))))))


 
	 
		    

  


  

    
  
  


		   

   
    

  


  