#lang racket

(require "interpreter2.rkt")
(require rackunit)

; part 3 canvas integration tests
;(check-equal? (interpret "tests/p3_t1.bad") 10)
;(check-equal? (interpret "tests/p3_t2.bad") 14)
;(check-equal? (interpret "tests/p3_t3.bad") 45)
;(check-equal? (interpret "tests/p3_t4.bad") 55)
;(check-equal? (interpret "tests/p3_t5.bad") 1)
;(check-equal? (interpret "tests/p3_t6.bad") 115) 
;(check-equal? (interpret "tests/p3_t7.bad") 'true)
;(check-equal? (interpret "tests/p3_t8.bad") 20) 
;(check-equal? (interpret "tests/p3_t9.bad") 24)
;(check-equal? (interpret "tests/p3_t10.bad") 2) ; Fail
;(check-equal? (interpret "tests/p3_t11.bad") 35) ; Fail
;(check-exn
;   exn:fail? (lambda () (interpret "tests/p3_t12.bad")))
;(check-equal? (interpret "tests/p3_t13.bad") 90) 
;(check-equal? (interpret "tests/p3_t14.bad") 69) ; Fail
;(check-equal? (interpret "tests/p3_t15.bad") 87) ; Fail
;(check-equal? (interpret "tests/p3_t16.bad") 64) ; Fail
;(check-exn
;   exn:fail? (lambda () (interpret "tests/p3_t17.bad")))
;(check-equal? (interpret "tests/p3_t18.bad") 125) 
;(check-equal? (interpret "tests/p3_t19.bad") 100) ; Fail
;(check-equal? (interpret "tests/p3_t20.bad") 2000400) ; Fail

;(check-equal? (interpret "tests/p3_t21.bad") 2) ; Fail
;(check-equal? (interpret "tests/p3_t22.bad") 1) ; Fail
;(check-equal? (interpret "tests/p3_t23.bad") 1) ; Fail
;(check-equal? (interpret "tests/p3_t24.bad") 3) ; Fail
;(check-equal? (interpret "tests/p3_t25.bad") 3)
;(check-equal? (interpret "tests/p3_t26.bad") 1)





(interpret "tests/ethan.bad")


;((() ()) ((x) (#&10)) ((main y) ((() ((var x 10) (function blah () ((= x 16))) (var b 9) (funcall blah) (return x)) #<procedure:...ts/interpreter2.rkt:327:2>) #&3)))


;(((x) (#&16)) ((main y) ((() ((var x 10) (function blah () ((= x 16))) (var b 9) (funcall blah) (return x)) #<procedure:...ts/interpreter2.rkt:327:2>) #&3)))