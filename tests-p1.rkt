#lang racket

(require "interpreter.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))


; part 1 canvas integration tests
(check-equal? '() '())
(check-equal? (interpret "tests/test1.bad" "A") 150)
(check-equal? (interpret "tests/test2.bad" "A") -4)
(check-equal? (interpret "tests/test3.bad" "A") 10)
(check-equal? (interpret "tests/test4.bad" "A") 16)
(check-equal? (interpret "tests/test5.bad" "A") 220)
(check-equal? (interpret "tests/test6.bad" "A") 5)
(check-equal? (interpret "tests/test7.bad" "A") 6)
(check-equal? (interpret "tests/test8.bad" "A") 10)
(check-equal? (interpret "tests/test9.bad" "A") 5)
(check-equal? (interpret "tests/test10.bad" "A") -39)
(check-exn
   exn:fail? (lambda () (interpret "tests/test11.bad" "A")))
(check-exn
   exn:fail? (lambda () (interpret "tests/test12.bad" "A")))
(check-exn
   exn:fail? (lambda () (interpret "tests/test12.bad" "A")))
(check-exn
   exn:fail? (lambda () (interpret "tests/test13.bad" "A")))
;;(check-exn ;; not a required error
;;   exn:fail? (lambda () (interpret "tests/test14.bad")))
(check-equal? (interpret "tests/test15.bad" "A") 'true)
(check-equal? (interpret "tests/test16.bad" "A") 100)
(check-equal? (interpret "tests/test17.bad" "A") 'false)
(check-equal? (interpret "tests/test18.bad" "A") 'true)
(check-equal? (interpret "tests/test19.bad" "A") 128)
(check-equal? (interpret "tests/test20.bad" "A") 12)
