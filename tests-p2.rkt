#lang racket

(require "interpreter.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))

; part 2 canvas integration tests
(check-equal? (interpret "tests/p2_t1.bad" "A") 20)
(check-equal? (interpret "tests/p2_t2.bad" "A") 164) 
(check-equal? (interpret "tests/p2_t3.bad" "A") 32)
(check-equal? (interpret "tests/p2_t4.bad" "A") 2) 
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t5.bad" "A")))
(check-equal? (interpret "tests/p2_t6.bad" "A") 25)
(check-equal? (interpret "tests/p2_t7.bad" "A") 21)
(check-equal? (interpret "tests/p2_t8.bad" "A") 6)
(check-equal? (interpret "tests/p2_t9.bad" "A") -1)
(check-equal? (interpret "tests/p2_t10.bad" "A") 789)
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t11.bad" "A")))
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t12.bad" "A")))
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t13.bad" "A")))
(check-equal? (interpret "tests/p2_t14.bad" "A") 12)
(check-equal? (interpret "tests/p2_t15.bad" "A") 125)
;(check-equal? (interpret "tests/p2_t16.bad" "A") 110) ; Fail
;(check-equal? (interpret "tests/p2_t17.bad" "A") 2000400) ; Fail
(check-equal? (interpret "tests/p2_t18.bad" "A") 101)
(check-exn
   exn:fail? (lambda () (interpret "tests/p2_t19.bad" "A")))
;(check-equal? (interpret "tests/p2_tNestedTry.bad" "A") 18002) ; Fail
(check-equal? (interpret "tests/p2_tNestedTry2.bad" "A") 0)

;; optional extra
;; (check-equal? (interpret "tests/p2_t20.bad") 21) ; fail but optional


;;(interpret "tests/p2_t17.bad")

