#lang racket

(provide (all-defined-out))

(require "utils.rkt")
(require "valueFunctions.rkt")
(require rackunit)

; Your state must now be a list of layers
; Each layer will contain a list of variables and bindings similar to the basic state of part 1
; The initial state consist of a single layer. Each time a new block is entered,
; you must "cons" a new layer to the front of your state (but use abstraction and give the operation a better name than "cons").
; Each time a variable is declared, that variable's binding goes into the top layer.
; Each time a variable is accessed (either to lookup its binding or to change it),
; the search must start in the top layer and work down. When a block is exited, the layer must be popped off of the state,
; deleting any variables that were declared inside the block.  This will ensure that a variable is only active and live inside the block in which it is declared.

; Layers - TODO: move to utils.rkt
(define empty-layer '(() ()))
(define layers '())
(define merge cons)

; Adds a new empty layer to the front of the layers
(define add-layer
  (lambda (layers)
    (merge empty-layer layers)))

; Removes the 1st layer from the front of the list of layers
(define remove-layer
  (lambda (layers)
    (if (null? layers)
        null
        (cdr layers))))


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
