#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))


; part 1 canvas integration tests
;(check-equal? '() '())
;(check-equal? (interpret "tests/test1.bad") 150)
;(check-equal? (interpret "tests/test2.bad") -4)
;(interpret "tests/test3.bad")
;(check-equal? (interpret "tests/test3.bad") 10)
;(check-equal? (interpret "tests/test4.bad") 16)
;(check-equal? (interpret "tests/test5.bad") 220)




;(interpret "tests/test6_x.bad")


;(interpret "tests/p2_t1.bad")


;(interpret "tests/p2_t11.bad")
;(interpret "tests/p2_t12.bad")
(interpret "tests/p2_t17.bad")



;(check-equal? (interpret "tests/test6.bad") 5)
;(check-equal? (interpret "tests/test7.bad") 6)
;(check-equal? (interpret "tests/test8.bad") 10)
;(check-equal? (interpret "tests/test9.bad") 5)
;(check-equal? (interpret "tests/test10.bad") -39)
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test11.bad")))
;(check-equal? (interpret "tests/test12.bad") 'error)
;(check-equal? (interpret "tests/test12.bad") 'error)
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test13.bad")))
;;(check-exn ;; not a required error
;;   exn:fail? (lambda () (interpret "tests/test14.bad")))
;(check-equal? (interpret "tests/test15.bad") 'true)
;(check-equal? (interpret "tests/test16.bad") 100)
;(check-equal? (interpret "tests/test17.bad") 'false)
;(check-equal? (interpret "tests/test18.bad") 'true)
;(check-equal? (interpret "tests/test19.bad") 128)
;(check-equal? (interpret "tests/test20.bad") 12)
