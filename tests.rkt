#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
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


; update-binding tests
(check-equal? (update-binding 'b 'true '(((b) (false)) ((z) (1)) ((x a) (10 2)))) '(((b) (true)) ((z) (1)) ((x a) (10 2))))
(check-equal? (update-binding 'z 3 '(((z) (1)) ((x a) (10 2)))) '(((z) (3)) ((x a) (10 2))))
(check-equal? (update-binding 'a 7 '(((x a) (10 2)))) '(((x a) (10 7))))
(check-equal? (update-binding 'x 12 '(((x) (10)))) '(((x) (12))))
(check-equal? (update-binding 'a 9 '(((z) (1)) ((x a) (10 2)))) '(((z) (1)) ((x a) (10 9))))
(check-exn
   exn:fail? (lambda () (check-equal? (update-binding 'x 12 '(((a) (10)))) 'error)))

; lookup tests
(check-equal? (lookup 'x '(((x) (10)))) 10)
(check-equal? (lookup 'a '(((x a) (10 2)))) 2)
(check-equal? (lookup 'a '(((z) (1)) ((x a) (10 2)))) 2)
(check-equal? (lookup 'z '(((z) (1)) ((x a) (10 2)))) 1)
(check-equal? (lookup 'z '(((z) (false)) ((x a) (10 2)))) 'false)
(check-equal? (lookup 'x '(((z) (false)) ((x a) (10 2)))) 10)

; add-layer tests
(check-equal? (add-layer '((() ()))) '((() ()) (() ())))
(check-equal? (add-layer '(((x a) (10 2)))) '((() ()) ((x a) (10 2))))
(check-equal? (add-layer '(((z) (1)) ((x a) (10 2)))) '((() ()) ((z) (1)) ((x a) (10 2))))

; remove-layer tests
(check-equal? (remove-layer '((() ()) (() ()))) '((() ())))
(check-equal? (remove-layer '((() ()) ((x a) (10 2)))) '(((x a) (10 2))))
(check-equal? (remove-layer '((() ()) ((z) (1)) ((x a) (10 2)))) '(((z) (1)) ((x a) (10 2))))
(check-equal? (remove-layer '(((z) (1)) ((x a) (10 2)))) '(((x a) (10 2))))

; Add-Binding Tests
(check-equal? (add-binding 'a 5 null) '(((a) (5))))
(check-equal? (add-binding 'x 10 '((() ()))) '(((x) (10))))
(check-equal? (add-binding 'a 2 '(((x) (10)))) '(((x a) (10 2))))
(check-equal? (add-binding 'z 1 '((() ()) ((x a) (10 2)))) '(((z) (1)) ((x a) (10 2))))
(check-equal? (add-binding 'b 'false '((() ()) ((z) (1)) ((x a) (10 2)))) '(((b) (false)) ((z) (1)) ((x a) (10 2))))

; Remove-Binding Tests