#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))

; part 3 canvas integration tests
(interpret "tests/p3_t1.bad")
;(check-equal? (interpret "tests/p3_t1.bad") 10)
;(check-equal? (interpret "tests/p3_t2.bad") 14)
;(check-equal? (interpret "tests/p3_t3.bad") 45)
;(check-equal? (interpret "tests/p3_t4.bad") 55)
;(check-equal? (interpret "tests/p3_t5.bad") 1)
;(check-equal? (interpret "tests/p3_t6.bad") 115)
;(check-equal? (interpret "tests/p3_t7.bad") true)
;(check-equal? (interpret "tests/p3_t8.bad") 20)
;(check-equal? (interpret "tests/p3_t9.bad") 24)
;(check-equal? (interpret "tests/p3_t10.bad") 2)
;(check-equal? (interpret "tests/p3_t11.bad") 35)
;(check-equal? (interpret "tests/p3_t12.bad") error)
;(check-equal? (interpret "tests/p3_t13.bad") 90)
;(check-equal? (interpret "tests/p3_t14.bad") 69)
;(check-equal? (interpret "tests/p3_t15.bad") 87)
;(check-equal? (interpret "tests/p3_t16.bad") 64)
;(check-equal? (interpret "tests/p3_t17.bad") error "b out of scope")
;(check-equal? (interpret "tests/p3_t18.bad") 125)
;(check-equal? (interpret "tests/p3_t19.bad") 100)
;(check-equal? (interpret "tests/p3_t20.bad") 2000400)

