#lang racket

(provide (all-defined-out))
(require "utils.rkt")

; (<op> <int exp> <int exp>) OR (<op> <int exp>)
(define (M_int ls state return)
  (cond
    ((number? ls)           ls)
    ((atom? ls)             (lookup ls state))
    ((and (eq? (operator ls) '-) (null? (rightside ls)))
     (* (M_int (leftoperand ls) state return) -1))
    ((eq? (operator ls) '+)

     (display "\nwe are now adding")
     (display (leftoperand ls))
     (display ls)
     (display state)



     (+ (M_int (leftoperand ls) state return)
                               (M_int (rightoperand ls) state return)))
    ((eq? (operator ls) '-) (- (M_int (leftoperand ls) state return)
                               (M_int (rightoperand ls) state return)))
    ((eq? (operator ls) '*) (* (M_int (leftoperand ls) state return)
                               (M_int (rightoperand ls) state return)))
    ((eq? (operator ls) '/) (quotient (M_int (leftoperand ls) state return)
                                      (M_int (rightoperand ls) state return)))
    ((eq? (operator ls) '%) (remainder (M_int (leftoperand ls) state return)
                                       (M_int (rightoperand ls) state return)))
    (else                   'error)))

;(<op> <bool exp> <bool exp>) OR (<op> <bool exp>)
(define (M_bool ls state return)
  (cond
    ; base cases
    ((eq? 'true ls) 'true)
    ((eq? 'false ls) 'false)
    ((atom? ls) (lookup ls state))
    ; if we're dealing with the unary operator and the expression is...
    ((and (and (eq? (operator ls) '!) (null? (rightside ls)))
          (eq? (M_bool (leftoperand ls) state return) 'true)) 'false) ; ...true, then return false
    ((and (and (eq? (operator ls) '!) (null? (rightside ls)))
          (eq? (M_bool (leftoperand ls) state return) 'false)) 'true) ; ...false, then return true
    ; Can't just compare #t and #f, because we need to return 'true and 'false
    ; That means we need to check all cases for each operator that could return 'true or 'false
    ((and (eq? (operator ls) '&&)
          (and (eq? (M_bool (leftoperand ls) state return) 'true)
               (eq? (M_bool (rightoperand ls) state return) 'true))) 'true)
    ((and (eq? (operator ls) '&&)
          (or (eq? (M_bool (leftoperand ls) state return) 'false)
              (eq? (M_bool (rightoperand ls) state return) 'false))) 'false)
    ((and (eq? (operator ls) '||) (eq? (M_bool (leftoperand ls) state return) 'true)) 'true)
    ((and (eq? (operator ls) '||) (eq? (M_bool (rightoperand ls) state return) 'true)) 'true)
    ((and (eq? (operator ls) '||) (and (eq? (M_bool (rightoperand ls) state return) 'false)
                                       (eq? (M_bool (leftoperand ls) state return) 'false)) 'false))
    ((and (eq? (operator ls) '==) (eq? (M_int (rightoperand ls) state return)
                                       (M_int (leftoperand ls) state return))) 'true)
    ((and (eq? (operator ls) '==) (not (eq? (M_int (rightoperand ls) state return)
                                            (M_int (leftoperand ls) state return)))) 'false)
    ((and (eq? (operator ls) '!=) (not (eq? (M_int (rightoperand ls) state return)
                                            (M_int (leftoperand ls) state return)))) 'true)
    ((and (eq? (operator ls) '!=) (eq? (M_value (rightoperand ls) state return)
                                       (M_value (leftoperand ls) state return))) 'false)
    ((and (eq? (operator ls) '>) (> (M_int (leftoperand ls) state return)
                                    (M_int (rightoperand ls) state return))) 'true)
    ((and (eq? (operator ls) '>) (not (> (M_int (leftoperand ls) state return)
                                         (M_int (rightoperand ls) state return)))) 'false)
    ((and (eq? (operator ls) '<) (< (M_int (leftoperand ls) state return)
                                    (M_int (rightoperand ls) state return))) 'true)
    ((and (eq? (operator ls) '<) (not (< (M_int (leftoperand ls) state return)
                                         (M_int (rightoperand ls) state return)))) 'false)
    ((and (eq? (operator ls) '<=) (<= (M_int (leftoperand ls) state return)
                                      (M_int (rightoperand ls) state return))) 'true)
    ((and (eq? (operator ls) '<=) (not (<= (M_int (leftoperand ls) state return)
                                           (M_int (rightoperand ls) state return)))) 'false)
    ((and (eq? (operator ls) '>=) (>= (M_int (leftoperand ls) state return)
                                      (M_int (rightoperand ls) state return))) 'true)
    ((and (eq? (operator ls) '>=) (not (>= (M_int (leftoperand ls) state return)
                                           (M_int (rightoperand ls) state return)))) 'false)
    (else 'error)))

; Given some arbitrary expression, determine the value of the expression
(define (M_value ls state return)
  (cond
    ; first two are useful just to catch here, so we don't always have to go to M_bool
    ((eq? ls 'true) 'true)
    ((eq? ls 'false) 'false)
    ((number? ls) (M_int ls state return))
    ; Not a simple expression, try getting a value from M_int or M_bool
    ((not (eq? (M_int ls state return) 'error)) (M_int ls state return))
    ((not (eq? (M_bool ls state return) 'error)) (M_bool ls state return))
    ; For handling cases like (M_bool '((false)) state), without this line just gives error
    ((list? ls) (M_value (car ls) state return))
    (else 'error)))
