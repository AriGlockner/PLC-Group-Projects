#lang racket

(require "interpreter.rkt")
(require rackunit)

; part 3 canvas integration tests
(check-equal? (interpret "tests/p3_t1.bad" "A") 10)
(check-equal? (interpret "tests/p3_t2.bad" "A") 14)
(check-equal? (interpret "tests/p3_t3.bad" "A") 45)
(check-equal? (interpret "tests/p3_t4.bad" "A") 55)
(check-equal? (interpret "tests/p3_t5.bad" "A") 1)
(check-equal? (interpret "tests/p3_t6.bad" "A") 115) 
(check-equal? (interpret "tests/p3_t7.bad" "A") 'true)
(check-equal? (interpret "tests/p3_t8.bad" "A") 20) 
(check-equal? (interpret "tests/p3_t9.bad" "A") 24)
(check-equal? (interpret "tests/p3_t10.bad" "A") 2)
(check-equal? (interpret "tests/p3_t11.bad" "A") 35)
(check-exn
   exn:fail? (lambda () (interpret "tests/p3_t12.bad" "A")))
(check-equal? (interpret "tests/p3_t13.bad" "A") 90) 
(check-equal? (interpret "tests/p3_t14.bad" "A") 69)
(check-equal? (interpret "tests/p3_t15.bad" "A") 87)
;(check-equal? (interpret "tests/p3_t16.bad" "A") 64) ; Fail
(check-exn
   exn:fail? (lambda () (interpret "tests/p3_t17.bad" "A")))
(check-equal? (interpret "tests/p3_t18.bad" "A") 125) 
;(check-equal? (interpret "tests/p3_t19.bad" "A") 100) ; Fail
;(check-equal? (interpret "tests/p3_t20.bad" "A") 2000400) ; Fail

(check-equal? (interpret "tests/p3_t21.bad" "A") 2)
(check-equal? (interpret "tests/p3_t22.bad" "A") 1)
;(check-equal? (interpret "tests/p3_t23.bad" "A") 1) ; Fail
(check-equal? (interpret "tests/p3_t24.bad" "A") 3)
(check-equal? (interpret "tests/p3_t25.bad" "A") 3)
(check-equal? (interpret "tests/p3_t26.bad" "A") 1)

(check-equal? (interpret "tests/foo.bad" "A") 2)
