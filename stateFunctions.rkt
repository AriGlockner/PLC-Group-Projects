#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")

; verify the state has multiple layers
(define check_break
  (lambda (state)
    (cond
      ((null? state) 'error) ; state has no layers
      ((null? (cdr state)) 'error) ; state is only one layer
      (else state)))) ;; state has multiple layers


; given an arbitrary expression, determine the state of the program after the expression
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
    ;((eq? 'try (keyword exp)) (M_state_try (cadr exp) (caadr (caddr exp)) (caddr (caddr exp)) (cadr (cadddr exp)) 'null state))
    (else 'error)))

(define (M_state exp state return next break continue throw)
  (cond
    ((null? exp) (if (eq? next 'invalid_next)
                     (remove-layer state)
                     (next (remove-layer state))))
    ((null? exp)       (next state))
    ((null? (cdr exp)) (M_state_keyword_helper exp state return next break continue throw))
    ;((not (list? (cadr exp))) (next (M_state_keyword_helper exp state next)))
    ((list? (car exp)) (M_state_keyword_helper exp state return (lambda (s) (M_state (cdr exp) s (lambda (v) v) (lambda (v) v) (lambda (v) v))) break continue))
    (else              (M_state_keyword_helper exp state return next break continue throw))))



; block statements
(define (M_state_block ls state return next break continue throw)
  (M_statementlist (cdr ls)
                   (add-layer state)
                   return
                   (lambda (st) (next (remove-layer st)))
                   (lambda (st) (break (remove-layer st)))
                   (lambda (st) (continue (remove-layer st)))
                   throw
                   ))
 ; (remove-layer (M_state (cdr ls) (add-layer state) next))) ;; dont show me the insides
 ; (M_state (cdr ls) (add-layer state))) ;; show me the insides


(define M_statementlist
  (lambda (stmts state return next break continue throw)
    (if (null? stmts)
        (next state)
        (M_state (car stmts) state return
                 (lambda (nstate) (M_statementlist (cdr stmts) nstate return next break continue throw )) break continue throw)
        )))

; assign (=) operation
(define (M_state_assign var expr state return next)
  (cond
    ((or (eq? (M_value expr state return) 'error)) 'error)
    ((eq? (lookup var state) 'error) (next (add-binding var (M_value expr state return)))) ; if var is not in state
    (else
     (next (update-binding var (M_value expr state return) state))
     )))

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
               throw
               ) 
      (next state)
      ))


; declare (var) operation
(define (M_state_declare expr state return next)
  (let ((var (cadr expr)))
    (if (assoc var state)
        (error "Variable is already defined")
        (if (null? (rightside expr))
            (let* ((s1 (add-binding var 'null state))) (next s1))
            (let* ((s1 (add-binding var (M_value (car (rightside expr)) state return) state))) (next s1))))))





             