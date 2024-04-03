#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require rackunit)

;(define state '(((x y a) (5 12 true))))
;(define global '((a) (1)))
(M_environment '(() ()) '((a b) (2 3)))

; part 3 canvas integration tests
;(check-equal? (interpret "tests/p3_t1.bad") 10) ; Fail
;(check-equal? (interpret "tests/p3_t2.bad") 14) ; Fail
;(check-equal? (interpret "tests/p3_t3.bad") 45) ; Fail
;(check-equal? (interpret "tests/p3_t4.bad") 55) ; Fail
;(check-equal? (interpret "tests/p3_t5.bad") 1) ; Fail
;(check-equal? (interpret "tests/p3_t6.bad") 115) ; Fail
;(check-equal? (interpret "tests/p3_t7.bad") true) ; Fail
;(check-equal? (interpret "tests/p3_t8.bad") 20) ; Fail
;(check-equal? (interpret "tests/p3_t9.bad") 24) ; Fail
;(check-equal? (interpret "tests/p3_t10.bad") 2) ; Fail
;(check-equal? (interpret "tests/p3_t11.bad") 35) ; Fail
;(check-equal? (interpret "tests/p3_t12.bad") error) ; Fail
;(check-equal? (interpret "tests/p3_t13.bad") 90) ; Fail
;(check-equal? (interpret "tests/p3_t14.bad") 69) ; Fail
;(check-equal? (interpret "tests/p3_t15.bad") 87) ; Fail
;(check-equal? (interpret "tests/p3_t16.bad") 64) ; Fail
;(check-equal? (interpret "tests/p3_t17.bad") error "b out of scope") ; Fail
;(check-equal? (interpret "tests/p3_t18.bad") 125) ; Fail
;(check-equal? (interpret "tests/p3_t19.bad") 100) ; Fail
;(check-equal? (interpret "tests/p3_t20.bad") 2000400) ; Fail

