#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")

; useful for calling the right thing once we're sure we have the expression (exp)
; in the right format
(define (M_state_keyword_helper exp state return next break continue throw)
  (cond
    ((eq? 'var (keyword exp))
       (M_state_declare exp state return next))
    ((eq? '= (keyword exp)) (M_state_assign (cadr exp) (caddr exp) state return next))
    ((eq? 'if (keyword exp)) (M_state_if exp state return next break continue throw))
    ((eq? 'while (keyword exp)) (M_state_while (cadr exp) (caddr exp) state return next throw))
    ((eq? 'return (keyword exp)) (return (M_value (cadr exp) state return)))
    ((eq? 'break (keyword exp)) (break (check_break state)))
    ((eq? 'continue (keyword exp)) (continue state))
    ((eq? 'begin (keyword exp)) (M_state_block exp state return next break continue throw))
    ((eq? 'try (keyword exp)) (M_state_try exp state return next break continue throw))
    ((eq? 'throw (keyword exp)) (throw (M_value (cadr exp) state throw) state))
    ((eq? 'finally (keyword exp)) (M_state (cdr exp) (add-layer state) return next break continue throw))
    (else 'error)))

; given an arbitrary expression, determine the state of the program after the expression
; (first, needs to make sure we set next properly (or leave it alone) and whatnot
(define (M_state exp state return next break continue throw)
  (cond
    ((null? exp) (if (eq? next 'invalid_next)
                     (remove-layer state)
                     (next (remove-layer state))))
    ((null? exp)       (next state))
    ((null? (cdr exp)) (M_state_keyword_helper exp state return next break continue throw))
    ((list? (car exp)) (M_state_keyword_helper exp state return (lambda (s) (M_state (cdr exp) s return (lambda (v) v) (lambda (v) v) (lambda (v) v) throw)) break continue throw))
    (else              (M_state_keyword_helper exp state return next break continue throw))))

; block statements
(define (M_state_block ls state return next break continue throw)
  (M_statements (cdr ls)
                (add-layer state)
                return
                (lambda (s) (next (remove-layer s)))
                (lambda (s) (break (remove-layer s)))
                (lambda (s) (continue (remove-layer s)))
                (lambda (exception s) (throw exception (remove-layer s)))))

; try catch finally
(define M_state_try
  (lambda (exp state return next break continue throw)
    (let* (
           (try_exp (add_begin_try (cadr exp)))
           (finally_exp (add_begin_finally (cadddr exp)))
           (new_next (lambda (s) (M_state_block finally_exp s return next break continue throw)))
           (new_return (lambda (s) (M_state_block finally_exp s return return break continue throw)))
           (new_break (lambda (s) (M_state_block finally_exp s return break break continue throw)))
           (new_continue (lambda (s) (M_state_block finally_exp s return continue break continue throw)))
           (new_throw (throw-helper (caddr exp) state return next break continue throw finally_exp))
           )
    (M_state_block try_exp state new_return new_next new_break new_continue new_throw))))
          
; helper function for when we see throw inside a try (so we can move to catch)
(define throw-helper
  (lambda (exp state return next break continue throw finally)
    (cond
      ((null? exp) (lambda (e s) (M_state_block finally s return (lambda (s) (throw e s)) break continue throw)))
      ((not (eq? (car exp) 'catch)) (error "bad catch"))
      (else
       (lambda (exception curr_state) (M_statements
                             (caddr exp) ; catch expression/block
                             (add-binding (caadr exp) exception (add-layer curr_state)) ; bind the catch variable to the thrown exception value
                             return
                             (lambda (new_state) (M_state_block finally (remove-layer new_state) return next break continue throw)) ; next
                             (lambda (new_state) (break (remove-layer new_state))) ; break
                             (lambda (new_state) (continue (remove-layer new_state))) ; continue
                             (lambda (new_exception new_state) (throw new_exception (remove-layer new_state)))))))))

; deal with an expression formatted as a list of statements
(define M_statements
  (lambda (exp state return next break continue throw)
    (if (null? exp)
        (next state)
        (M_state (car exp) state return
                 (lambda (new_state)
                   (M_statements (cdr exp) new_state return next break continue throw))
                 break continue throw))))

; assign (=) operation
(define (M_state_assign var expr state return next)
  (if (or (eq? (M_value expr state return) 'error))
      'error
      (next (update-binding var (M_value expr state return) state))))
      
; handle if when we have 2 statements (then and else)
(define (M_state_if_2 condition statement1 statement2 state return next break continue throw)
  (if (eq? (M_bool condition state return) 'error)
      'error
      (if (eq? (M_bool condition state return) 'true)
          (M_state statement1 state return next break continue throw)
          (M_state statement2 state return next break continue throw))))

; handles if when we only have 1 statement
(define (M_state_if_1 condition statement1 state return next break continue throw)
  (if (eq? (M_bool condition state return) 'error)
      'error
      (if (eq? (M_bool condition state return) 'true)
          (M_state statement1 state return next break continue throw)
          (next state)))) ; return the current state if the condition is false

; if operation
(define (M_state_if exp state return next break continue throw)
  (cond
    ((eq? (length exp) 3)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp)))
       (M_state_if_1 condition statement1 state return next break continue throw)))
    ((eq? (length exp) 4)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp))
            (statement2 (cadddr exp)))
       (if (eq? statement2 #f)  
           (M_state_if_1 condition statement1 state return next break continue throw)
           (M_state_if_2 condition statement1 statement2 state return next break continue throw))))
    (else 'error)))

; while operation
(define (M_state_while condition statement state return next throw)
      (loop condition statement state return next throw))

(define (loop condition statement state return next throw)
  (if (eq? (M_bool condition state return) 'true)
      ; Loop
      (M_state statement state return
               (lambda (s1) (loop condition statement s1 return next throw))
               (lambda (s1) (next s1))
               (lambda (s1) (loop condition statement s1 return next throw))
               throw)
      (next state)))

; declare (var) operation
(define (M_state_declare expr state return next)
  (let ((var (cadr expr)))
    (if (assoc var state)
        (error "Variable is already defined")
        (if (null? (rightside expr))
            (let* ((s1 (add-binding var 'null state))) (next s1))
            (let* ((s1 (add-binding var (M_value (car (rightside expr)) state return) state))) (next s1))))))
