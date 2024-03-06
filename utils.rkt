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