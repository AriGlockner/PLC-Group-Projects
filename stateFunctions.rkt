; given a key and the state of the program, return the key's value
(define lookup
  (lambda (key state)
  (cond
    ((null? state) (error "variable used before declared")) ; if state is empty
    ((eq? key (caar state)) (cadar state)) ; return if key found
    (else (lookup key (cdr state))) ; continue to search rest of list if not found
  )))

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