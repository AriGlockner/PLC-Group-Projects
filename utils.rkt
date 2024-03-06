#lang racket

(provide (all-defined-out))

; Some abstraction functions
(define operator car)
(define keyword car)
(define leftoperand cadr)
(define rightside cddr)
(define rightoperand caddr)

; useful for determining if an element is an atom or not
(define (atom? x) (not (pair? x)))

; useful for getting the final state as a single output rather than a list
(define foldl
  (lambda (f acc lst)
    (if (null? lst)
        acc
        (foldl f (f (car lst) acc) (cdr lst)))))

; given a key and the state of the program, return the key's value
(define lookup
  (lambda (key state)
  (cond
    ((null? state) (error "variable used before declared")) ; if state is empty
    ((eq? key (caar state)) (cadar state)) ; return if key found
    (else (lookup key (cdr state))) ; continue to search rest of list if not found
  )))

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