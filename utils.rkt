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
      ((eq? (car vars) value) (car keys))
      (else (lookup-helper (cdr vars) (cdr keys) value)))))

; Adds a new empty layer to the front of the layers
(define add-layer
  (lambda (layers)
    (combine empty-layer layers)))

; Removes the 1st layer from the front of the list of layers
(define remove-layer
  (lambda (layers)
    (if (null? layers)
        null
        (cdr layers))))

; Add Binding to the state
(define add-binding
  (lambda (name value state)
  (cond
      ((null? state) (list (cons (list name) (list (list value)))))
      (else
       (cons (cons (cons name (caar state)) (list (cons value (cadar state)))) (cdr state)) 
       ))))
       

(define add-to-layer
  (lambda (name value layer)
    (list
     (if (null? (car layer))
         (list name)
         (cons name (car layer)))
     (if (null? (cadr layer))
         (list value)
         (cons value (cadr layer))))))


; Remove Binding from the state
(define remove-binding
  (lambda (name state)
    (cond
      [(null? state) '()]
      [(eq? (caar state) name) (cdr state)]
      [else (combine (car state) (remove-binding name (cdr state)))]
      )))

; update binding
(define update-binding
  (lambda (name newvalue state)
    (cond
      ((null? state) (error "state should not be empty"))
      ((list? (car state))
       (let ((result (update-binding-helper (caar state) (cadar state) name newvalue (lambda (v1 v2 v3) (cons v1 (list v2))))))
         
         
         (if (eq? (update-binding-helper (caar state) (cadar state) name newvalue (lambda (v1 v2 v3) v3)) 'notfound)
             (cons (car state) (update-binding name newvalue (cdr state))) ; continue searching the rest of the state
             (cons result (cdr state))))) ; return the value if found
    (else (error "state is bad")))))


(define update-binding-helper
  (lambda (vars keys value newvalue return)
    (cond
      ((or (null? vars) (null? keys)) (return '() '() 'notfound))
      ((eq? (car vars) value) 
       (update-binding-helper (cdr vars) (cdr keys) value newvalue
                              (lambda (r-vars r-keys status)
                                (return (cons value r-vars) (cons newvalue r-keys) 'found))))
      (else 
       (update-binding-helper (cdr vars) (cdr keys) value newvalue
                              (lambda (r-vars r-keys status)
                                (return (cons (car vars) r-vars) (cons (car keys) r-keys) status)))))))

    

