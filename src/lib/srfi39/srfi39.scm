;;;; Bigloo adaption
;;;; Copyright (C) Joseph Donaldson 2021
;;;; Original Srfi39 implementation 

;;;; Copyright (C) Marc Feeley 2002. All Rights Reserved.

;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;;; SOFTWARE.
(module srfi39
   (static
      (class <parameter>
         val
         param::procedure))
   (export (make-parameter::procedure init #!optional (converter (lambda (x) x)))
           dynamic-bind))


(define (dynamic-env-local-get)
   (or (thread-parameter 'srfi39-local-env) '()))

(define (dynamic-env-local-set! new-env)
   (thread-parameter-set! 'srfi39-local-env new-env))


(define (make-<parameter> val
           #!key (param (lambda args
                           (error "<parameter>" "invalidly initialized" '()))))
   (instantiate::<parameter> (val val)
                             (param param)))

(define (make-parameter::procedure init #!optional (converter (lambda (x) x)))
   (let ((global-config::<parameter> (make-<parameter> (converter init)))
         (mutex (make-mutex)))
      (letrec ((parameter
                  (lambda new-val
                     (let ((config::<parameter>
                              (dynamic-lookup parameter global-config)))
                        ;; we only need mutual exclusion for the global parameter
                        ;; config. The local-configs are per thread. 
                        (cond ((null? new-val)
                               (if (eq? config global-config)
                                   (synchronize mutex
                                      (-> config val))
                                   (-> config val)))
                              ((null? (cdr new-val))
                               (if (eq? config global-config)
                                   (synchronize mutex
                                      (set! (-> config val) (converter (car new-val))))
                                   (set! (-> config val) (converter (car new-val)))))
                              (else ; this case is needed for parameterize
                               (converter (car new-val))))))))
         (set! (-> global-config param) parameter)
         parameter)))

(define dynamic-bind
   (lambda (parameters values body)
      (let* ((old-local
                (dynamic-env-local-get))
             (new-params
                (map (lambda (parameter value)
                        (make-<parameter>  (parameter value #f) :param parameter))
                   parameters
                   values))
             (new-local
                (append new-params old-local)))
         (dynamic-wind
            (lambda () (dynamic-env-local-set! new-local))
            body
            (lambda () (dynamic-env-local-set! old-local))))))

(define dynamic-lookup
   (lambda (parameter global-<parameter>)
      (or (find (lambda (p::<parameter>)
                   (eq? (-> p param) parameter))
             (dynamic-env-local-get))
          global-<parameter>)))
