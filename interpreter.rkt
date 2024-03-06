; Ethan Hansen, Gabriel Wolf, Ari Glockner
; CSDS 345 Programming Language Concepts
; Interpreter Project 1
; Feb 2024

#lang racket

(require "simpleParser.rkt")
(require "stateFunctions.rkt")
(require "valueFunctions.rkt")
(require "utils.rkt")


; putting it all together, take a filename, parse the file, and interpret the results
(define interpret
  (lambda (filename)
    (cond
      ((string=? filename "") (error "need a non-empty filename"))
      (else
       (let* ((expressions (parser filename))
              (final-state (foldl (lambda (exp state) (M_state exp state)) '() expressions)))
         final-state)))))


;(M_state '(var foo) state)
;(M_state '(var bar true) state)
;(M_state '(while (!= (% y x) 3) (= y (+ y 1))) state)
;(M_state '(return 4) state)
;(M_state '(return true) state)

; Test case: Assign a new value to an existing variable
;(M_state '(= x 10) state) ; Output: ((x 10) (y 12) (z 3) (a true))
; Test case: Assign the result of an expression to a variable
;(M_state '(= y (+ x 20)) state) ; Output: ((x 5) (y 25) (z 3) (a true))
; Test case: Assign a boolean expression to a variable
;(M_state '(= z (> x 2)) state) ; Output: ((x 5) (y 12) (z #t) (a true))
; Test case: Assign a boolean expression involving existing variables
;(M_state '(= flag (<= x y)) state) ; Output: ((x 5) (y 12) (z 3) (a true) (flag #t))
; Test case: Assign a new value to an existing variable using an invalid expression
;(M_state '(= x (/ x 0)) state) ; Output: 'error (division by zero)
; Test case: Assign a new value to an existing variable using an expression with undeclared variables
;(M_state '(= x (+ x undeclaredVar)) state) ; Output: 'error (undeclaredVar is not declared)
; Test case: Attempt to assign a value to a variable that doesn't exist
;(M_state '(= nonexistentVar 7) state) ; Output: 'error

;(M_state '(return (* x (+ x x))) state)
;(M_state '(return (- y 1)) state)
;(M_state '(= z 10) '((z error)))

;(M_state_if_1 '(> x y) '(= x (+ x y)) state) ; false 
;(M_state_if_1 '(< x y) '(= x (+ x y)) state) ;  true x = 17
;(M_state_if_2 '(> x y) '(= x (+ x y)) '(= y (+ x y)) state) ; false y = 17

;(M_state_while '(!= (% y x) 3) '(= y (+ y 1)) state) ; i think works?
;(M_state_while '(!= x 3) '(= x (- x 1)) state) ; works x = 3
;(M_state_while 'a '(= a (! a)) state)

;(M_bool '(> x 5) state)
;(M_bool '(< x 5) state)
;(M_bool '(>= x 5) state)
;(M_bool '(<= x 5) state)
;(M_bool '(== x 5) state)
;(M_bool '(!= x 5) state)

;(M_state_if '(if (< x y) (= x (+ x y))) state)
;(M_state_if '(if (> x y) (= x (+ x y))) state)
;(M_state_if '(if (> x y) (= x (+ x y)) (= y (+ x y))) state)
;(M_state_if '(if true (= x (+ x y)) (= y (+ x y))) state)
;(M_state_if '(if false (= x (+ x y)) (= y (+ x y))) state)
;(M_state_if '(if (&& a (|| true false)) (= x (+ x y)) (= y (+ x y))) state)

; Test case: Assign a new value to an existing variable
;(M_state_assign 'x 10 state) ; Output: ((x 10) (y 12) (z 3) (a true))
; Test case: Assign a value to a new variable
;(M_state_assign 'newVar 42 state) ; Output: ((x 5) (y 12) (z 3) (a true) (newVar 42))
; Test case: Assign the result of an expression to a variable
;(M_state_assign 'y '(+ x 20) state) ; Output: ((x 5) (y 25) (z 3) (a true))
; Test case: Assign a boolean expression to a variable
;(M_state_assign 'z '(> x 2) state) ; Output: ((x 5) (y 12) (z #t) (a true))
; Test case: Assign an arithmetic expression involving existing variables
;(M_state_assign 'result '(+ x y z) state) ; Output: ((x 5) (y 12) (z 3) (a true) (result 20))
; Test case: Assign a boolean expression involving existing variables
;(M_state_assign 'flag '(<= x y) state) ; Output: ((x 5) (y 12) (z 3) (a true) (flag #t))
; Test case: Attempt to assign a value to a variable that doesn't exist
;(M_state_assign 'nonexistentVar 7 state) ; Output: 'error
;(M_state_assign 'x 10 state) ; x = 10

; Test case: Declare a variable without an initial value
;(M_state_declare '(var newVar) state) ; Output: ((x 5) (y 12) (z 3) (a true) (newVar error))
; Test case: Declare a variable with an initial value
;(M_state_declare '(var anotherVar 42) state) ; Output: ((x 5) (y 12) (z 3) (a true) (anotherVar 42))
; Test case: Attempt to declare a variable with an invalid initial value
;(M_state_declare '(var invalidVar (+ x y)) state) ; Output: 'error

; Test case: Lookup an existing variable with a value
;(lookup 'x state) ; Output: 5
; Test case: Lookup an existing variable with a boolean value
;(lookup 'a state) ; Output: true
; Test case: Lookup a non-existent variable
;(lookup 'nonexistentVar state) ; Output: 'error
; Test case: Lookup a variable with a null value
;(lookup 'z state) ; Output: 3

;(M_value '(true) state)
;(M_value 'true state)
;(M_value '(4) state)
;(M_value '4 state)