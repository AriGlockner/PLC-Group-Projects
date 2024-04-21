#lang racket

(require "interpreter.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))

; part 2 canvas integration tests
(check-equal? (interpret "tests/p2_t1.bad") 20)
(check-equal? (interpret "tests/p2_t2.bad") 164) 
(check-equal? (interpret "tests/p2_t3.bad") 32)
(check-equal? (interpret "tests/p2_t4.bad") 2) 
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t5.bad")))
(check-equal? (interpret "tests/p2_t6.bad") 25)
(check-equal? (interpret "tests/p2_t7.bad") 21)
(check-equal? (interpret "tests/p2_t8.bad") 6)
(check-equal? (interpret "tests/p2_t9.bad") -1)
(check-equal? (interpret "tests/p2_t10.bad") 789)
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t11.bad")))
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t12.bad")))
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t13.bad")))
(check-equal? (interpret "tests/p2_t14.bad") 12)
(check-equal? (interpret "tests/p2_t15.bad") 125)
(check-equal? (interpret "tests/p2_t16.bad") 110)
(check-equal? (interpret "tests/p2_t17.bad") 2000400)
(check-equal? (interpret "tests/p2_t18.bad") 101)
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t19.bad")))
(check-equal? (interpret "tests/p2_tNestedTry.bad") 18002)
(check-equal? (interpret "tests/p2_tNestedTry2.bad") 0)

;; optional extra
;; (check-equal? (interpret "tests/p2_t20.bad") 21) ; fail but optional


;;(interpret "tests/p2_t17.bad")

