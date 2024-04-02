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
      (else
       (cdr layers)))))

; Add Binding to the state
(define add-binding
  (lambda (name value state)
  (if (null? state)
      (list (cons (list name) (list (list (box value)))))
      (cons (cons (cons name (caar state)) (list (cons (box value) (cadar state)))) (cdr state)))))






(define update-binding
  (lambda (var newvalue state)
    (call/cc
     (lambda (end)
       (display "\nstate: ")
       (display state)
       (display "\n looking for: ")
       (display var)
       
   ;    (let ((result (update-binding-O var newvalue state state)))
    ;     (display "\nresult:")
    ;     (display result)
    ;     (end result)


         (let ((result (update-binding-helper var newvalue (car state) (car state))))
         (display "\nresult:")
         (display result)
           (cond
            ; ((eq? state '()) (error "state is empty in the update-binding"))
            ;; ((eq? state '()) (end state))
             ((eq? result #f)
             ; (display "ZOOWEEMAMAMA")
            ;  (display (cons (car state) (update-binding var newvalue (cdr state))))
              (end (cons (car state) (update-binding var newvalue (cdr state))))
              )
             (else
              (end (list result)))
             )


         )))))


(define update-binding-helper
  (lambda (var newvalue state end)
    (cond
      ((equal? state '((()()))) (error "state is empty"))
      ((null? state)
       (display "yo gabba")
       (display state)
       #f
       )
   ;   ((eq? '() (cdr state))
   ;    (display "cdr is null!")
   ;    (update-binding-helper var newvalue (car state) end)
   ;    )
      
      
      ((atom? (car state)) (update-binding-helper var newvalue (cdr state) end))
      ((null? (car state)) (update-binding-helper var newvalue (remove-layer state) end))
      ((eq? var (caar state))
     ;  (display  (caadr state))
       (begin (set-box! (caadr state) newvalue) end))
      (else
      ; (display state)
       (update-binding-helper var newvalue (cons (cdar state) (cons (cdadr state) (cddr state))) end)
       )
      )))


; (((z y) (#&20 #&2)) ((x) (#&10)))
; (((m y x) (#&null #&6 #&5)))








; update binding
(define update-binding-O
  (lambda (name newvalue state end)
    (cond
      ((null? state)
       (error "state should not be empty"))
      ((list? (car state))
       (let ((result (update-binding-helper-O (caar state) (cadar state) name newvalue end state (lambda (v1 v2 v3) (cons v1 (list v2))))))
         (if (eq? (update-binding-helper-O (caar state) (cadar state) name newvalue end state (lambda (v1 v2 v3) v3)) 'notfound)
             (cons (car state) (update-binding-O name newvalue (cdr state) end)) ; continue searching the rest of the state
             (cons result (cdr state))))) ; return the value if found
    (else (error "state is bad")))))

(define update-binding-helper-O
  (lambda (vars keys value newvalue end state return)
    (cond
      ((or (null? vars) (null? keys)) (return '() '() 'notfound))
      ((eq? (car vars) value)
     ;  (display "yoyoyoyo")
      ; (display (caadar state))
       (update-binding-helper-O (cdr vars) (cdr keys) value newvalue end state
                              (lambda (r-vars r-keys status)
                                (return (cons value r-vars) (cons (begin (set-box! (caadar state) newvalue) end) r-keys) 'found))))
      (else 
       (update-binding-helper-O (cdr vars) (cdr keys) value newvalue end state
                              (lambda (r-vars r-keys status)
                                (return (cons (car vars) r-vars) (cons (car keys) r-keys) status)))))))









; update binding
(define update-binding-ORIGINAL
  (lambda (name newvalue state)
    (cond
      ((null? state)
       (error "state should not be empty"))
      ((list? (car state))
       (let ((result (update-binding-helper (caar state) (cadar state) name newvalue (lambda (v1 v2 v3) (cons v1 (list v2))))))
         (if (eq? (update-binding-helper (caar state) (cadar state) name newvalue (lambda (v1 v2 v3) v3)) 'notfound)
             (cons (car state) (update-binding name newvalue (cdr state))) ; continue searching the rest of the state
             (cons result (cdr state))))) ; return the value if found
    (else (error "state is bad")))))

(define update-binding-helper-ORIGINAL
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






; update binding
(define update-binding-xx
  (lambda (name newvalue state)
    (cond
      ((null? state)
       (error "state should not be empty"))
      ((list? (car state))
       (let ((result (update-binding-helper (caar state) (cadar state) name newvalue state (lambda (v1 v2 v3) (cons v1 (list v2))))))
         (cond
           ((eq? (update-binding-helper (caar state) (cadar state) name newvalue state (lambda (v1 v2 v3) v3)) 'notfound)
             (cons (car state) (update-binding name newvalue (cdr state)))) ; continue searching the rest of the state
           (else ; return the value if found
           ; (display state)
            (cons result (cdr state)))
           )))
    (else (error "state is bad")))))

(define update-binding-helper-xx
  (lambda (vars keys value newvalue state return)
    (cond
      ((or (null? vars) (null? keys)) (return '() '() 'notfound))
      ((eq? (car vars) value) 
       (update-binding-helper (cdr vars) (cdr keys) value newvalue state
                              (lambda (r-vars r-keys status)
                              ;;  (display "\n yo mama\n")
                               ;;(display (lookup value state))
                                
                             ;;   (display (caadar state))
                               ;; (display (unbox (car keys)))
                                ;;(display newvalue)
                              ;;  (display (set-box! (car keys) newvalue))
                              ;;  (display (unbox (caadar state)))
                              ;; (display (unbox
                                       ;;  (begin (set-box! (caadar state) newvalue) )
                                     ;;          ))

                                
                                
                                (return (cons value r-vars) (cons (begin (set-box! (caadar state) newvalue) #t) r-keys) 'found))))
      (else 
       (update-binding-helper (cdr vars) (cdr keys) value newvalue state
                              (lambda (r-vars r-keys status)
                                (return (cons (car vars) r-vars) (cons (car keys) r-keys) status)))))))






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