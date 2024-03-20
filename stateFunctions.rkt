#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")


; given an arbitrary expression, determine the state of the program after the expression
(define M_state
  (lambda (exp state)
    (cond
      ((null? exp) state)
      ((and (list? exp) (list? (keyword exp)))
      ; (display "\nexp")
      ; (display exp)
      ; (display "\ncar")
      ; (display (car exp))
      ; (display "\ncdr")
      ; (display (cdr exp))
      ; (display "\nEnd")
       (M_state (cdr exp) (M_state (car exp) state)))
       
      ((eq? 'var (keyword exp))
       (M_state_declare exp state))
      ((eq? '= (keyword exp)) (M_state_assign (cadr exp) (caddr exp) state))
      ((eq? 'if (keyword exp)) (M_state_if exp state))
      ((eq? 'while (keyword exp)) (M_state_while (cadr exp) (caddr exp) state))
      ((eq? 'return (keyword exp)) (M_value (cadr exp) state))
      ((eq? 'begin (keyword exp)) (M_state_block exp state))
      (else 'error))))

; block statements
(define (M_state_block ls state)
  (remove-layer (M_state (cdr ls) (add-layer state)))) ;; dont show me the insides
 ; (M_state (cdr ls) (add-layer state))) ;; show me the insides
  

; assign (=) operation
(define (M_state_assign var expr state)
  (cond
    ((or (eq? (M_value expr state) 'error)) 'error)
    ((eq? (lookup var state) 'error) (add-binding var (M_value expr state))) ; if var is not in state
    (else
     (update-binding var (M_value expr state) state)
     )))

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