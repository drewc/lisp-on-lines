(in-package :lol-test)

(defsuite lisp-on-lines)

(in-suite lisp-on-lines)

(defclass lol-test-class ()
  ((string-slot 
    :accessor string-slot 
    :initform "test"
    :type string)
   (number-slot 
    :accessor number-slot 
    :initform 12345
    :type number)
   (symbol-slot 
    :accessor symbol-slot
    :initform 'symbol
    :type symbol)))

(deftest test-simple-define-description ()
  (eval '(lol:define-description test-description ()
	  ((test-attribute :label "BRILLANT!"))))
  
  (eval '(deflayer test-description-layer))

  (eval '(lol:define-description test-description ()
	  ((test-attribute :label "BRILLANT-IN-LAYER"))
	  (:in-layer . test-description-layer))))

(deftest test-T-description ()
  (let ((d (find-description t)))
    (is (find-attribute d 'identity))))

(deftest test-simple-attributes ()
  (test-simple-define-description)
  (let* ((desc (find-description 'test-description))
	 (att (find-attribute desc 'test-attribute)))
    (is (equal "BRILLANT!" (slot-value att 'lol::label)))
    (with-active-layers (test-description-layer)
      (is (equal "BRILLANT-IN-LAYER" (slot-value att 'lol::label))))))

(deftest test-special-slot-values ()
  (test-simple-attributes)
  (is (equalp '(lol::label "BRILLANT!") 
		(lol::special-slot-values 
		 (find-description 'test-description) 'test-attribute))))

(defparameter *atomic-type-specifiers* 
  '(arithmetic-error                  function            simple-condition           
    array                             generic-function    simple-error               
    atom                              hash-table          simple-string              
    base-char                         integer             simple-type-error          
    base-string                       keyword             simple-vector              
    bignum                            list                simple-warning             
    bit                               logical-pathname    single-float               
    bit-vector                        long-float          standard-char              
    broadcast-stream                  method              standard-class             
    built-in-class                    method-combination  standard-generic-function  
    cell-error                        nil                 standard-method            
    character                         null                standard-object            
    class                             number              storage-condition          
    compiled-function                 package             stream                     
    complex                           package-error       stream-error               
    concatenated-stream               parse-error         string                     
    condition                         pathname            string-stream              
    cons                              print-not-readable  structure-class            
    control-error                     program-error       structure-object           
    division-by-zero                  random-state        style-warning              
    double-float                      ratio               symbol                     
    echo-stream                       rational            synonym-stream             
    end-of-file                       reader-error        t                          
    error                             readtable           two-way-stream             
    extended-char                     real                type-error                 
    file-error                        restart             unbound-slot               
    file-stream                       sequence            unbound-variable           
    fixnum                            serious-condition   undefined-function         
    float                             short-float         unsigned-byte              
    floating-point-inexact            signed-byte         vector                     
    floating-point-invalid-operation  simple-array        warning                    
    floating-point-overflow           simple-base-string                             
    floating-point-underflow          simple-bit-vector))

(deftest test-basic-types-description-of ()
  (let* ((symbol 'symbol)
	(string "string")
	(number 0) 
	(list (list symbol string number)))))
	





  