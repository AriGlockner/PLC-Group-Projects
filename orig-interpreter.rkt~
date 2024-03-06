; Ethan Hansen, Gabriel Wolf, Ari Glockner
; CSDS 345 Programming Language Concepts
; Interpreter Project 1
; Feb 2024

#lang racket

(require "simpleParser.rkt")
(require rackunit)

; Some abstraction functions
(define operator car)
(define keyword car)
(define leftoperand cadr)
(define rightside cddr)
(define rightoperand caddr)

; useful for determining if an element is an atom or not
(define (atom? x) (not (pair? x)))

; given a key and the state of the program, return the key's value
(define lookup
  (lambda (key state)
  (cond
    ((null? state) (error "variable used before declared")) ; if state is empty
    ((eq? key (caar state)) (cadar state)) ; return if key found
    (else (lookup key (cdr state))) ; continue to search rest of list if not found
  )))


; (<op> <int exp> <int exp>) OR (<op> <int exp>)
(define (M_int ls state)
  (cond
    ((number? ls)           ls)
    ((atom? ls)             (lookup ls state))
    ((and (eq? (operator ls) '-) (null? (rightside ls))) (* (M_int (leftoperand ls) state) -1))
    ((eq? (operator ls) '+) (+ (M_int (leftoperand ls) state) (M_int (rightoperand ls) state)))
    ((eq? (operator ls) '-) (- (M_int (leftoperand ls) state) (M_int (rightoperand ls) state)))
    ((eq? (operator ls) '*) (* (M_int (leftoperand ls) state) (M_int (rightoperand ls) state)))
    ((eq? (operator ls) '/) (quotient (M_int (leftoperand ls) state) (M_int (rightoperand ls) state)))
    ((eq? (operator ls) '%) (remainder (M_int (leftoperand ls) state) (M_int (rightoperand ls) state)))
    (else                   'error)))


;(<op> <bool exp> <bool exp>) OR (<op> <bool exp>)
(define (M_bool ls state)
  (cond
    ; base cases
    ((eq? 'true ls)                                            'true)
    ((eq? 'false ls)                                           'false)
    ((atom? ls)                                                (lookup ls state))
    ; if we're dealing with the unary operator and the expression is...
    ((and (and (eq? (operator ls) '!) (null? (rightside ls)))
          (eq? (M_bool (leftoperand ls) state) 'true))
                                                                'false) ; ...true, then return false
    ((and (and (eq? (operator ls) '!) (null? (rightside ls)))
          (eq? (M_bool (leftoperand ls) state) 'false))
                                                                'true) ; ...false, then return true
    ; Can't just compare #t and #f, because we need to return 'true and 'false
    ; That means we need to check all cases for each operator that could return 'true or 'false
    ((and (eq? (operator ls) '&&)
          (and (eq? (M_bool (leftoperand ls) state) 'true)
               (eq? (M_bool (rightoperand ls) state) 'true)))   'true)
    ((and (eq? (operator ls) '&&)
          (or (eq? (M_bool (leftoperand ls) state) 'false)
              (eq? (M_bool (rightoperand ls) state) 'false)))   'false)
    ((and (eq? (operator ls) '||) (eq? (M_bool (leftoperand ls) state) 'true))
                                                                'true)
    ((and (eq? (operator ls) '||) (eq? (M_bool (rightoperand ls) state) 'true))
                                                                'true)
    ((and (eq? (operator ls) '||) (and (eq? (M_bool (rightoperand ls) state) 'false)
                                       (eq? (M_bool (leftoperand ls) state) 'false))
                                                                'false))
    ((and (eq? (operator ls) '==) (eq? (M_int (rightoperand ls) state)
                                       (M_int (leftoperand ls) state)))
                                                                'true)
    ((and (eq? (operator ls) '==) (not (eq? (M_int (rightoperand ls) state)
                                            (M_int (leftoperand ls) state))))
                                                                'false)
    ((and (eq? (operator ls) '!=) (not (eq? (M_int (rightoperand ls) state)
                                            (M_int (leftoperand ls) state))))
                                                                'true)
    ((and (eq? (operator ls) '!=) (eq? (M_value (rightoperand ls) state)
                                       (M_value (leftoperand ls) state)))
                                                                'false)
    ((and (eq? (operator ls) '>) (> (M_int (leftoperand ls) state)
                                    (M_int (rightoperand ls) state)))
                                                                'true)
    ((and (eq? (operator ls) '>) (not (> (M_int (leftoperand ls) state)
                                         (M_int (rightoperand ls) state))))
                                                                'false)
    ((and (eq? (operator ls) '<) (< (M_int (leftoperand ls) state)
                                    (M_int (rightoperand ls) state)))
                                                                'true)
    ((and (eq? (operator ls) '<) (not (< (M_int (leftoperand ls) state)
                                         (M_int (rightoperand ls) state))))
                                                                'false)
    ((and (eq? (operator ls) '<=) (<= (M_int (leftoperand ls) state)
                                      (M_int (rightoperand ls) state)))
                                                                'true)
    ((and (eq? (operator ls) '<=) (not (<= (M_int (leftoperand ls) state)
                                           (M_int (rightoperand ls) state))))
                                                                'false)
    ((and (eq? (operator ls) '>=) (>= (M_int (leftoperand ls) state)
                                      (M_int (rightoperand ls) state)))
                                                                'true)
    ((and (eq? (operator ls) '>=) (not (>= (M_int (leftoperand ls) state)
                                           (M_int (rightoperand ls) state))))
                                                                'false)
    (else                                                       'error)))


; Given some arbitrary expression, determine the value of the expression
(define (M_value ls state)
  (cond
    ; first two are useful just to catch here, so we don't always have to go to M_bool
    ((eq? ls 'true)                       'true)
    ((eq? ls 'false)                      'false)
    ((number? ls)                         (M_int ls state))
    ; Not a simple expression, try getting a value from M_int or M_bool
    ((not (eq? (M_int ls state) 'error))  (M_int ls state))
    ((not (eq? (M_bool ls state) 'error)) (M_bool ls state))
    ; For handling cases like (M_bool '((false)) state), without this line just gives error
    ((list? ls)                           (M_value (car ls) state))
    (else                                 'error)))

; given an arbitrary expression, determine the state of the program after the expression
(define M_state
  (lambda (exp state)
    (cond
      ; if we have a list at the beginning, need to recurse on both car and cdr
      ((and (list? exp) (list? (keyword exp))) (M_state (cdr exp) (M_state (keyword exp) state)))
      ; otherwise, match this expression to the type of expression and handle that with
      ;   the appropriate function
      ((eq? 'var (keyword exp)) (M_state_declare exp state))
      ((eq? '= (keyword exp)) (M_state_assign (cadr exp) (caddr exp) state))
      ((eq? 'if (keyword exp)) (M_state_if exp state))
      ((eq? 'while (keyword exp)) (M_state_while (cadr exp) (caddr exp) state))
      ((eq? 'return (keyword exp)) (M_value (cadr exp) state))
      (else ('error)))))


; Add Binding to the state
(define add-binding
  (lambda (name value state)
    (if (null? state)
        (cons (cons name (cons value '())) '())
        (cons (car state) (add-binding name value (cdr state))))
    ))

; Remove Binding from the state
(define remove-binding
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? (caar state) name) (cdr state)]
      [else (cons (car state) (remove-binding name (cdr state)))]
      )))

; assign (=) operation
(define (M_state_assign var expr state)
  (if (or (eq? (M_value expr state) 'error) (eq? (lookup var state) 'error))
      'error
      (let* ((s1 (remove-binding var state))
             (s2 (add-binding var (M_value expr state) s1)))
        s2)))

; handle if when we have 2 statements (then and else)
(define (M_state_if_2 condition statement1 statement2 state)
  (if (or (eq? (M_bool condition state) 'error)
          (eq? (M_state statement1 state) 'error)
          (eq? (M_state statement2 state) 'error))
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state statement1 state)
          (M_state statement2 state))))

; handles if when we only have 1 statement
(define (M_state_if_1 condition statement1 state)
  (if (or (eq? (M_bool condition state) 'error)
          (eq? (M_state statement1 state) 'error))
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state statement1 state)
          state))) ; return the current state if the condition is false

; if operation
(define (M_state_if exp state)
  (cond
    ((eq? (length exp) 3)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp)))
       (M_state_if_1 condition statement1 state)))
    ((eq? (length exp) 4)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp))
            (statement2 (cadddr exp)))
       (if (eq? statement2 #f)  
           (M_state_if_1 condition statement1 state)
           (M_state_if_2 condition statement1 statement2 state))))
    (else 'error)))

; while operation
(define (M_state_while condition statement state)
  (if (or (eq? (M_bool condition state) 'error)
          (eq? (M_state statement state) 'error))
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state_while condition statement (M_state statement state))
          state)))

; declare (var) operation
(define (M_state_declare expr state)
  (let ((var (cadr expr)))
    (if (assoc var state)
        (error "Variable is already defined")
        (if (null? (rightside expr))
            (let* ((s1 (add-binding var 'null state))) s1)
            (let* ((s1 (add-binding var (M_value (car (rightside expr)) state) state))) s1)))))


; putting it all together, take a filename, parse the file, and interpret the results
(define interpret
  (lambda (filename)
    (cond
      ((string=? filename "") (error "need a non-empty filename"))
      (else
       (let* ((expressions (parser filename))
              (final-state (foldl (lambda (exp state) (M_state exp state)) '() expressions)))
         final-state)))))

; useful for getting the final state as a single output rather than a list
(define foldl
  (lambda (f acc lst)
    (if (null? lst)
        acc
        (foldl f (f (car lst) acc) (cdr lst)))))


(check-equal? (interpret "test1.bad") 150)


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