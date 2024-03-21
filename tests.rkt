#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))

; part 1 canvas integration tests
(check-equal? '() '())
;(check-equal? (interpret "tests/test1.bad") 150)
;(check-equal? (interpret "tests/test2.bad") -4)
;(check-equal? (interpret "tests/test3.bad") 10)
;(check-equal? (interpret "tests/test4.bad") 16)
;(check-equal? (interpret "tests/test5.bad") 220)
;(check-equal? (interpret "tests/test6.bad") 5)
;(check-equal? (interpret "tests/test7.bad") 6)
;(check-equal? (interpret "tests/test8.bad") 10)
;(check-equal? (interpret "tests/test9.bad") 5)
;(check-equal? (interpret "tests/test10.bad") -39)
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test11.bad")))
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test12.bad")))
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test13.bad")))
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test14.bad")))
;(check-equal? (interpret "tests/test15.bad") 'true)
;(check-equal? (interpret "tests/test16.bad") 100)
;(check-equal? (interpret "tests/test17.bad") 'false)
;(check-equal? (interpret "tests/test18.bad") 'true)
;(check-equal? (interpret "tests/test19.bad") 128)
;(check-equal? (interpret "tests/test20.bad") 12)

; part 2 canvas integration tests
;(check-equal? (interpret "tests/p2_t1.bad") 20)
;(check-equal? (interpret "tests/p2_t2.bad") 164)
;(check-equal? (interpret "tests/p2_t3.bad") 32)
;(check-equal? (interpret "tests/p2_t4.bad") 2)
;(check-equal? (interpret "tests/p2_t5.bad") 'error)
;(check-equal? (interpret "tests/p2_t6.bad") 25)
;(check-equal? (interpret "tests/p2_t7.bad") 21)
;(check-equal? (interpret "tests/p2_t8.bad") 6)
;(check-equal? (interpret "tests/p2_t9.bad") -1)
;(check-equal? (interpret "tests/p2_t10.bad") 789)
;(check-equal? (interpret "tests/p2_t11.bad") 'error)
;(check-equal? (interpret "tests/p2_t12.bad") 'error)
;(check-equal? (interpret "tests/p2_t13.bad") 'error)
;(check-equal? (interpret "tests/p2_t14.bad") 12)
;(check-equal? (interpret "tests/p2_t15.bad") 125)
;(check-equal? (interpret "tests/p2_t16.bad") 110)
;(check-equal? (interpret "tests/p2_t17.bad") 2000400)
;(check-equal? (interpret "tests/p2_t18.bad") 101)
;(check-equal? (interpret "tests/p2_t19.bad") 'error)


; break tests

;(M_state_while '(< x 9) '(begin (= x (+ x 1)) (= y 8))  state (lambda (v) v)) ; '(((x y a) (9 8 true)))
;(M_state_while '(< x 9) '(begin (= x (+ x 1)) (break) (= y 8))  state (lambda (v) v)) ; '(((x y a) (6 12 true)))

;(M_state_if '(if (< x 9) (begin (= x (+ x 1)) (= y 8))) state (lambda (v) v) (lambda (v) v) (lambda (v) v)) ; '(((x y a) (6 8 true)))
;(M_state_if '(if (< x 9) (begin (= x (+ x 1)) (break) (= y 8))) state (lambda (v) v) (lambda (v) v) (lambda (v) v)) ; ''(((x y a) (6 12 true)))


; continue tests

(M_state_while '(< x 9) '(begin (= x (+ x 1)) (= y 8))  state (lambda (v) v)) ; '(((x y a) (9 8 true)))
(M_state_while '(< x 9) '(begin (= x (+ x 1)) (continue) (= y 8))  state (lambda (v) v)) ; '(((x y a) (9 12 true)))

(M_state_if '(if (< x 9) (begin (= x (+ x 1)) (= y 8))) state (lambda (v) v) (lambda (v) v) (lambda (v) v)) ; '(((x y a) (6 8 true)))
(M_state_if '(if (< x 9) (begin (= x (+ x 1)) (continue) (= y 8))) state (lambda (v) v) (lambda (v) v) (lambda (v) v)) ; '(((x y a) (6 12 true)))

