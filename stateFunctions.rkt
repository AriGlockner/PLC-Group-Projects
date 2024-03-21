#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")


; given an arbitrary expression, determine the state of the program after the expression
(define (M_state_keyword_helper exp state next)
  (cond
    ((eq? 'var (keyword exp))
       (M_state_declare exp state next))
    ((eq? '= (keyword exp)) (M_state_assign (cadr exp) (caddr exp) state next))
    ((eq? 'if (keyword exp)) (M_state_if exp state next))
    ((eq? 'while (keyword exp)) (M_state_while (cadr exp) (caddr exp) state next))
    ((eq? 'return (keyword exp)) (M_value (cadr exp) state))
    ((eq? 'begin (keyword exp)) (M_state_block exp state))
    ;((eq? 'try (keyword exp)) (M_state_try (cadr exp) (caadr (caddr exp)) (caddr (caddr exp)) (cadr (cadddr exp)) 'null state))
    (else 'error)))

(define (M_state exp state next)
  (cond
    ((null? exp)       (next state))
    ((null? (cdr exp)) (M_state_keyword_helper exp state next))
    ;((not (list? (cadr exp))) (next (M_state_keyword_helper exp state next)))
    ((list? (car exp)) (M_state_keyword_helper exp state (lambda (s) (M_state (cdr exp) s (lambda (v) v)))))
    (else              (M_state_keyword_helper exp state next))))



; block statements
(define (M_state_block ls state)
  (remove-layer (M_state (cdr ls) (add-layer state)))) ;; dont show me the insides
 ; (M_state (cdr ls) (add-layer state))) ;; show me the insides
  

; assign (=) operation
(define (M_state_assign var expr state next)
  (cond
    ((or (eq? (M_value expr state) 'error)) 'error)
    ((eq? (lookup var state) 'error) (next (add-binding var (M_value expr state)))) ; if var is not in state
    (else
     (next (update-binding var (M_value expr state) state))
     )))

; handle if when we have 2 statements (then and else)
(define (M_state_if_2 condition statement1 statement2 state next)
  (if (eq? (M_bool condition state) 'error)
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state statement1 state next)
          (M_state statement2 state next))))

; handles if when we only have 1 statement
(define (M_state_if_1 condition statement1 state next)
  (if (eq? (M_bool condition state) 'error)
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state statement1 state next)
          (next state)))) ; return the current state if the condition is false

; if operation
(define (M_state_if exp state next)
  (cond
    ((eq? (length exp) 3)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp)))
       (M_state_if_1 condition statement1 state next)))
    ((eq? (length exp) 4)
     (let* ((condition (cadr exp))
            (statement1 (caddr exp))
            (statement2 (cadddr exp)))
       (if (eq? statement2 #f)  
           (M_state_if_1 condition statement1 state next)
           (M_state_if_2 condition statement1 statement2 state next))))
    (else 'error)))

; while operation
(define (M_state_while condition statement state next)
  (if (or (eq? (M_bool condition state) 'error)
          (eq? (M_state statement state) 'error))
      'error
      (if (eq? (M_bool condition state) 'true)
          (M_state_while condition statement (M_state statement state))
          state)))

; declare (var) operation
(define (M_state_declare expr state next)
  (let ((var (cadr expr)))
    (if (assoc var state)
        (error "Variable is already defined")
        (if (null? (rightside expr))
            (let* ((s1 (add-binding var 'null state))) (next s1))
            (let* ((s1 (add-binding var (M_value (car (rightside expr)) state) state))) (next s1))))))





             