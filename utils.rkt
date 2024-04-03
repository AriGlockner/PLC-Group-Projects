#lang racket

(provide (all-defined-out))

; Some abstraction functions
(define operator car)
(define keyword car)
(define leftoperand cadr)
(define rightside cddr)
(define rightoperand caddr)

; Some layers functions
(define empty-layer '(() ()))
(define layers '())
(define combine cons)

; useful for determining if an element is an atom or not
(define (atom? x) (not (pair? x)))

; useful for getting the final state as a single output rather than a list
(define foldl
  (lambda (f acc lst)
    (if (null? lst)
        acc
        (foldl f (f (car lst) acc) (cdr lst)))))

; new lookup
(define lookup
  (lambda (key state)
  (cond
    ((null? state) 'error) ; if state is empty
    ((eq? state 'error) 'error)
    ((list? (car state))
       (let ((result (lookup-helper (caar state) (cadar state) key)))
         (if (eq? result 'badday)
             (lookup key (cdr state)) ; continue searching the rest of the state
             result))) ; return the value if found
    (else (error "state is bad")))))

(define lookup-helper
  (lambda (vars keys value)
    (cond
      ((or (null? vars) (null? keys)) 'badday)
      ((eq? (car vars) value)
       (cond
         ((not (box? (car keys))) (car keys))
         ((void? (unbox (car keys))) (error "variable not assigned"))
         (else
          (unbox (car keys)))))
      ; (car keys))
      (else (lookup-helper (cdr vars) (cdr keys) value)))))

; Adds a new empty layer to the front of the layers
(define add-layer
  (lambda (layers)
    (combine empty-layer layers)))

; [DEPRECATED] Removes the 1st layer from the front of the list of layers
(define remove-layer-old
  (lambda (layers)
    (if (null? layers)
        null
        (cdr layers))))

; Removes the 1st layer from the front of the list of layers
(define remove-layer
  (lambda (layers)
    (cond
      ((null? layers) null)
      ((eq? layers 'error) (error "error"))
      (else
       (display layers)
       (cdr layers)))))

; Add Binding to the state
(define add-binding
  (lambda (name value state)
  (if (null? state)
      (list (cons (list name) (list (list (box value)))))
      (cons (cons (cons name (caar state)) (list (cons (box value) (cadar state)))) (cdr state)))))





;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)


; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))



; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))


; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))


(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)



; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))



(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))


; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))



(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append (string-append str (makestr "" vals)) "\n"))))))







(define update-binding
  (lambda (name newvalue state)
    (update name newvalue state)))


; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    ;(display environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (begin (set-box! (car vallist) (scheme->language val)) vallist))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))








; verify the state has multiple layers
(define check_break
  (lambda (state)
    (cond
      ((null? state) 'error) ; state has no layers
      ((null? (cdr state)) 'error) ; state is only one layer
      (else state)))) ;; state has multiple layers

; add 'begin to try
(define add_begin_try
  (lambda (exp)
    (cons 'begin exp)))

; add 'begin to finally
(define add_begin_finally
  (lambda (exp)
    (cond
      ((null? exp) '(begin))
      ((not (eq? (car exp) 'finally)) (error "bad finally"))
      (else (cons 'begin (cadr exp))))))