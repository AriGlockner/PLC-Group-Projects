#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")





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


; assign (=) operation

(define (M_state_assign var expr state)
  (cond
    ((or (eq? (M_value expr state) 'error)) 'error)
  ;  ((eq? (lookup var state) 'error) (error "variable doesn't exist yet")) ; variable not even created yet
  ;  ((eq? (lookup var state) 'null) (update-binding var expr state)) ; variable exists but has not been assigned
   ; (else
  ;   (error "attempting to redefine a variable"))))
  ;  (else
  ;   (update-binding var expr state))))



     ((eq? (lookup var state) 'error) (display "adding binding") (display (lookup var state)) (add-binding var expr state))
    (else
     (display "  VAR  ")
     (display var)
     (display "  EXPR  ")
     (display expr)
     (display "  STATE  ")
    (display state)
     (display " LOOKUP ")
     (display (lookup var state))
     (update-binding var expr state)

     )))



(define (M_state_assign-forethan var expr state)
  (cond
    ((or (eq? (M_value expr state) 'error)) 'error)
    ((eq? (lookup var state) 'error) (display "adding binding") (display (lookup var state)) (add-binding var expr state))
    (else
     (display "  VAR  ")
     (display var)
     (display "  EXPR  ")
     (display expr)
     (display "  STATE  ")
     (display state)
     (display " LOOKUP ")
     (display (lookup var state))
     (update-binding var expr state))))




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