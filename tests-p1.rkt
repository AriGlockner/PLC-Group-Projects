#lang racket

(require "interpreter.rkt")
(require "utils.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require rackunit)

(define state '(((x y a) (5 12 true))))


; part 1 canvas integration tests
(check-equal? '() '())
(check-equal? (interpret "tests/test1.bad") 150)
(check-equal? (interpret "tests/test2.bad") -4)
(check-equal? (interpret "tests/test3.bad") 10)
(check-equal? (interpret "tests/test4.bad") 16)
(check-equal? (interpret "tests/test5.bad") 220)
(check-equal? (interpret "tests/test6.bad") 5)
(check-equal? (interpret "tests/test7.bad") 6)
(check-equal? (interpret "tests/test8.bad") 10)
(check-equal? (interpret "tests/test9.bad") 5)
(check-equal? (interpret "tests/test10.bad") -39)
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test11.bad")))
(check-equal? (interpret "tests/test12.bad") 'error)
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test12.bad")))
(check-exn
   exn:fail? (lambda () (interpret "tests/test13.bad")))
;(check-exn
;   exn:fail? (lambda () (interpret "tests/test14.bad")))
(check-equal? (interpret "tests/test15.bad") 'true)
(check-equal? (interpret "tests/test16.bad") 100)
(check-equal? (interpret "tests/test17.bad") 'false)
(check-equal? (interpret "tests/test18.bad") 'true)
(check-equal? (interpret "tests/test19.bad") 128)
(check-equal? (interpret "tests/test20.bad") 12)


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
(check-equal? (lookup 'b '(((z) (false)) ((x a) (10 2)))) 'error)

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
(check-equal? (add-binding 'a 2 '(((x) (10)))) '(((a x) (2 10))))
(check-equal? (add-binding 'z 1 '((() ()) ((x a) (10 2)))) '(((z) (1)) ((x a) (10 2))))
(check-equal? (add-binding 'b 'false '((() ()) ((z) (1)) ((x a) (10 2)))) '(((b) (false)) ((z) (1)) ((x a) (10 2))))

; assign tests
;(check-equal? (M_state '(= x 10) '(((x) (null))) (lambda (v) v)) '(((x) (10))))
(check-equal? (M_state_assign 'x 10 '(((x) (null))) (lambda (v) v) (lambda (v) v)) '(((x) (10))))
(check-equal? (M_state_assign 'x '(* 2 4) '(((x) (10))) (lambda (v) v) (lambda (v) v)) '(((x) (8))))
(check-equal? (M_state_assign 'z 'x '(((z y x) (null 5 10))) (lambda (v) v) (lambda (v) v)) '(((z y x) (10 5 10))))
(check-equal? (M_state_assign 'z '(* x y) '(((z y x) (null 5 10))) (lambda (v) v) (lambda (v) v)) '(((z y x) (50 5 10))))

; declare tests
; Test case: Declare a variable without an initial value
(check-equal? (M_state_declare '(var newVar) state (lambda (v) v) (lambda (v) v)) '(((newVar x y a) (null 5 12 true))))
; Test case: Declare a variable with an initial value
(check-equal? (M_state_declare '(var anotherVar 42) state (lambda (v) v) (lambda (v) v)) '(((anotherVar x y a) (42 5 12 true))))
; Test case: Declare a variable with an expression initial value
(check-equal? (M_state_declare '(var exprVar (+ x y)) state (lambda (v) v) (lambda (v) v)) '(((exprVar x y a) (17 5 12 true))))

; Declaration tests
(check-equal? (M_state '(var foo) state  (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((foo x y a) (null 5 12 true))))
(check-equal? (M_state '(var bar true) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((bar x y a) (true 5 12 true))))

; M_state_if tests
(check-equal? (M_state_if '(if (< x y) (= x (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (17 12 true))))
(check-equal? (M_state_if '(if (> x y) (= x (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) state)
(check-equal? (M_state_if '(if (> x y) (= x (+ x y)) (= y (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (5 17 true))))
(check-equal? (M_state_if '(if true (= x (+ x y)) (= y (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (17 12 true))))
(check-equal? (M_state_if '(if false (= x (+ x y)) (= y (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (5 17 true))))
(check-equal? (M_state_if '(if (&& a (|| true false)) (= x (+ x y)) (= y (+ x y))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (17 12 true))))

(check-equal? (M_state_if_1 '(> x y) '(= x (+ x y)) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) state)
(check-equal? (M_state_if_1 '(< x y) '(= x (+ x y)) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (17 12 true))))
(check-equal? (M_state_if_2 '(> x y) '(= x (+ x y)) '(= y (+ x y)) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (5 17 true))))

 (check-equal? (M_state_if '(if (< x 9) (begin (= x (+ x 1)))) state (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (6 12 true)))) ;; error


; M_state_while tests
(check-equal? (M_state_while '(!= (% y x) 3) '(= y (+ y 1)) state  (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (5 13 true))))
(check-equal? (M_state_while '(!= x 3) '(= x (- x 1)) state  (lambda (v) v) (lambda (v) v) (lambda (v) v))  '(((x y a) (3 12 true))))
(check-equal? (M_state_while 'a '(= a (! a)) state (lambda (v) v)  (lambda (v) v) (lambda (v) v)) '(((x y a) (5 12 false))))

(check-equal? (M_state_while '(< x 9) '(begin (= x (+ x 1))) state (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x y a) (9 12 true)))) 


; M_bool tests
(check-equal? (M_bool '(> x 5) state (lambda (v) v)) 'false)
(check-equal? (M_bool '(< x 5) state (lambda (v) v)) 'false)
(check-equal? (M_bool '(>= x 5) state (lambda (v) v)) 'true)
(check-equal? (M_bool '(<= x 5) state (lambda (v) v)) 'true)
(check-equal? (M_bool '(== x 5) state (lambda (v) v)) 'true)
(check-equal? (M_bool '(!= x 5) state (lambda (v) v)) 'false)

; M_value tests
(check-equal? (M_value '(true) state (lambda (v) v)) 'true)
(check-equal? (M_value 'true state (lambda (v) v)) 'true)
(check-equal? (M_value '(4) state (lambda (v) v)) 4)
(check-equal? (M_value '4 state (lambda (v) v)) 4)

; test cases for M_state_block
(check-equal? (M_state_block '(begin (var y 2) (= x y)) '(((x) (10))) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x) (2))))
(check-equal? (M_state_block '(begin (var y 2) (var z (* x y)) (= x z)) '(((x) (10))) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((x) (20))))
(check-equal? (M_state_block '(begin (var temp a) (= a b) (= b temp)) '(((b a) (1476 31160))) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((b a) (31160 1476))))
(check-equal? (M_state_block '(begin (= a b) (= b r) (= r (% a b))) '(((r b a) (1 2 3))) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) '(((r b a) (0 1 2))))

; return tests
;(M_state '(return 6) '((() ()))  (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))
;(M_state '(return x) '(((x) (5)))  (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v))
;;; (M_state '((= x 2) (+ x 1) (= x 9)) '(((x) (5))) (lambda (v) v) (lambda (v) v) (lambda (v) v) (lambda (v) v)) ; FAIL
