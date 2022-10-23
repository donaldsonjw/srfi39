(module srfi39test
   (main main)
   (library srfi39 btest))


(define radix (make-parameter 10))


(define-test-suite srfi39-tests
   
   (define radix (make-parameter 10))

   (test "(radix) is 10"
      (assert= (radix) 10))
   
    

   (test "(radix 2) set radix to 2"
      (radix 2)
      (assert= (radix) 2))

   (test "parameterize works"
      (parameterize ((radix 16))
         (assert= (radix) 16))
      (assert= (radix) 2)))


(define (main args)
    (let ((tr (instantiate::terminal-test-runner (suite srfi39-tests))))
       (if (test-runner-execute tr #t) 0 -1)))