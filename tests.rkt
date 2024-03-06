#lang racket

(require "interpreter.rkt")
(require rackunit)

(check-equal? '() '())
(check-equal? (interpret "test1.bad") 150)
(check-equal? (interpret "test2.bad") -4)
(check-equal? (interpret "test3.bad") 10)
(check-equal? (interpret "test4.bad") 16)
(check-equal? (interpret "test5.bad") 220)
(check-equal? (interpret "test6.bad") 5)
(check-equal? (interpret "test7.bad") 6)
(check-equal? (interpret "test8.bad") 10)
(check-equal? (interpret "test9.bad") 5)
(check-equal? (interpret "test10.bad") -39)
(check-exn
   exn:fail? (lambda () (interpret "test11.bad")))
(check-exn
   exn:fail? (lambda () (interpret "test12.bad")))
(check-exn
   exn:fail? (lambda () (interpret "test13.bad")))
(check-exn
   exn:fail? (lambda () (interpret "test14.bad")))
(check-equal? (interpret "test15.bad") 'true)
(check-equal? (interpret "test16.bad") 100)
(check-equal? (interpret "test17.bad") 'false)
(check-equal? (interpret "test18.bad") 'true)
(check-equal? (interpret "test19.bad") 128)
(check-equal? (interpret "test20.bad") 12)
