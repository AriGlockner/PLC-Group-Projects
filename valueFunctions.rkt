#lang racket

(provide (all-defined-out))

(require "utils.rkt")


; (<op> <int exp> <int exp>) OR (<op> <int exp>)
(define M_int
  (lambda (ls state)
    (M_int_cps ls state (lambda (v) v))))

(define (M_int_cps ls state return)
  (cond
    ((number? ls) (return ls))
    ((atom? ls) (return (lookup ls state)))
    ((and (eq? (operator ls) '-) (null? (rightside ls)))
     (M_int_cps (leftoperand ls) state
            (lambda (r-left) (return (* r-left -1)))))
    ((eq? (operator ls) '+)
     (M_int_cps (leftoperand ls) state
            (lambda (r-left)
              (M_int_cps (rightoperand ls) state
                     (lambda (r-right)
                       (return (+ r-left r-right)))))))
    ((eq? (operator ls) '-)
     (M_int_cps (leftoperand ls) state
            (lambda (r-left)
              (M_int_cps (rightoperand ls) state
                     (lambda (r-right)
                       (return (- r-left r-right)))))))
    ((eq? (operator ls) '*)
     (M_int_cps (leftoperand ls) state
            (lambda (r-left)
              (M_int_cps (rightoperand ls) state
                     (lambda (r-right)
                       (return (* r-left r-right)))))))
    ((eq? (operator ls) '/)
     (M_int_cps (leftoperand ls) state
            (lambda (r-left)
              (M_int_cps (rightoperand ls) state
                     (lambda (r-right)
                       (return (quotient r-left r-right)))))))
    ((eq? (operator ls) '%)
     (M_int_cps (leftoperand ls) state
            (lambda (r-left)
              (M_int_cps (rightoperand ls) state
                     (lambda (r-right)
                       (return (remainder r-left r-right)))))))
    (else (return 'error))))



;(<op> <bool exp> <bool exp>) OR (<op> <bool exp>)
(define M_bool
  (lambda (ls state)
    (M_bool_cps ls state (lambda (v) v))))

(define M_bool_cps
  (lambda (ls state return)
    (cond
      ; base cases
      ((eq? 'true ls) (return 'true))
      ((eq? 'false ls) (return 'false))
      ((atom? ls) (return (lookup ls state)))
      ; if we're dealing with the unary operator and the expression is...
       ((and (eq? (operator ls) '!) (null? (rightside ls)) 
            (M_bool_cps (leftoperand ls) state
                       (lambda (r-left)
                         (if (eq? r-left 'true)
                             (return 'false)
                             (return 'true))))))           
      ; Can't just compare #t and #f, because we need to return 'true and 'false
      ; That means we need to check all cases for each operator that could return 'true or 'false
       ((and (eq? (operator ls) '&&) 
            (M_bool_cps (leftoperand ls) state
                       (lambda (r-left)
                         (M_bool_cps (rightoperand ls) state
                                     (lambda (r-right)
                                       (if (and (eq? r-right 'true) (eq? r-left 'true))
                                           (return 'true)
                                           (return 'false))))))))
       ((and (eq? (operator ls) '||) 
            (M_bool_cps (leftoperand ls) state
                       (lambda (r-left)
                         (M_bool_cps (rightoperand ls) state
                                     (lambda (r-right)
                                       (if (eq? r-right 'true)
                                           (return 'true)
                                           (if (eq? r-left 'true)
                                               (return 'true)
                                               (return 'false)))))))))                                  
      ((and (eq? (operator ls) '==)
            (M_int_cps (rightoperand ls) state
                       (lambda (r-right)
                         (M_int_cps (leftoperand ls) state
                                    (lambda (r-left)
                                      (if (= r-right r-left)
                                          (return 'true)
                                          (return 'false))))))))    
      ((and (eq? (operator ls) '!=)
            (M_int_cps (rightoperand ls) state
                       (lambda (r-right)
                         (M_int_cps (leftoperand ls) state
                                    (lambda (r-left)
                                      (if (= r-right r-left)  
                                          (return 'false)
                                          (return 'true)))))))) 
      ((and (eq? (operator ls) '>)
      (M_int_cps (rightoperand ls) state
                 (lambda (r-right)
                   (M_int_cps (leftoperand ls) state
                              (lambda (r-left)
                                (if (> r-right r-left)
                                    (return 'true)
                                    (return 'false))))))))
      ((and (eq? (operator ls) '<)
      (M_int_cps (rightoperand ls) state
                 (lambda (r-right)
                   (M_int_cps (leftoperand ls) state
                              (lambda (r-left)
                                (if (< r-right r-left)
                                    (return 'true)
                                    (return 'false))))))))
      ((and (eq? (operator ls) '<=)
      (M_int_cps (rightoperand ls) state
                 (lambda (r-right)
                   (M_int_cps (leftoperand ls) state
                              (lambda (r-left)
                                (if (<= r-right r-left)
                                    (return 'true)
                                    (return 'false))))))))
       ((and (eq? (operator ls) '>=)
      (M_int_cps (rightoperand ls) state
                 (lambda (r-right)
                   (M_int_cps (leftoperand ls) state
                              (lambda (r-left)
                                (if (>= r-right r-left)
                                    (return 'true)
                                    (return 'false))))))))
      (else (return 'error)))))



; Given some arbitrary expression, determine the value of the expression
(define (M_value ls state)
  (cond
    ; first two are useful just to catch here, so we don't always have to go to M_bool
    ((eq? ls 'true)                       'true)
    ((eq? ls 'false)                      'false)
    ((number? ls)                         (M_int ls state))
    ; Not a simple expression, try getting a value from M_int or M_bool
    ((not (eq? (M_int ls state) 'error))  (M_int ls state))
    ((not (eq? (M_bool ls state) 'error)) (M_bool ls state))
    ; For handling cases like (M_bool '((false)) state), without this line just gives error
    ((list? ls)                           (M_value (car ls) state))
    (else                                 'error)))