#lang racket

(require "interpreter.rkt")
(require rackunit)

;(check-equal? (interpret "tests/p4_t1.bad" "A") 15)
;(check-equal? (interpret "tests/p4_t2.bad" "A") 12)
;(check-equal? (interpret "tests/p4_t3.bad" "A") 125)
;(check-equal? (interpret "tests/p4_t4.bad" "A") 36)
;(check-equal? (interpret "tests/p4_t5.bad" "A") 54)
;(check-equal? (interpret "tests/p4_t6.bad" "A") 110)
;(check-equal? (interpret "tests/p4_t7.bad" "C") 26)
;(check-equal? (interpret "tests/p4_t8.bad" "Square") 117)
;(check-equal? (interpret "tests/p4_t9.bad" "Square") 32)
;(check-equal? (interpret "tests/p4_t10.bad" "List") 15)
;(check-equal? (interpret "tests/p4_t11.bad" "List") 123456)
;(check-equal? (interpret "tests/p4_t12.bad" "List") 5285)
;(check-equal? (interpret "tests/p4_t13.bad" "C") -716)
;(check-equal? (interpret "tests/p4_t14.bad" "A") 530) ; Test Case 21
;(check-equal? (interpret "tests/p4_t15.bad" "B") 66) ; Test Case 22
;(check-equal? (interpret "tests/p4_t16.bad" "A") 1026) ; Test Case 23
;(check-equal? (interpret "tests/p4_t17.bad" "A") 2045) ; Test Case 24