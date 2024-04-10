; If you are using scheme instead of racket, comment these two lines, uncomment the (load "simpleParser.scm") and comment the (require "simpleParser.rkt")
#lang racket
(require "functionParser.rkt")
(require rackunit)
; (load "simpleParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

(provide (all-defined-out))

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  Sets default continuations for return, break, continue, throw, and "next statement"
(define (interpret file)
  (scheme->language
   (interpret-statement-list (parser file) (initenvironment) (lambda (v) v)
                             (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                             (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env))))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define (interpret-statement-list statement-list environment return break continue throw next)
  (if (null? statement-list)
      (next environment)
      (interpret-statement (operator statement-list) environment return break continue throw (lambda (env) (interpret-statement-list (remainingframes statement-list) env return break continue throw next)))))


; interpret a statement in the environment with continuations for return, break, continue, throw, and "next statement"
(define (interpret-statement statement environment return break continue throw next)
  (cond
    ((eq? 'return (statement-type statement)) (interpret-return statement environment return throw))
    ((eq? 'var (statement-type statement)) (interpret-declare statement environment next throw))
    ((eq? '= (statement-type statement)) (interpret-assign statement environment next throw))
    ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw next))
    ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw next))
    ((eq? 'continue (statement-type statement)) (continue environment))
    ((eq? 'break (statement-type statement)) (break environment))
    ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw next))
    ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw))
    ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw next))
    ((eq? 'function (statement-type statement)) (interpret-function statement environment next))
    ((eq? 'funcall (statement-type statement)) (interpret-funcall-state statement environment return break continue throw next))
    (else (myerror "Unknown statement:" (statement-type statement)))))

; Calls a function in a value
(define (interpret-funcall-value funcall environment throw)
  ; Get the function parameters
  (let* ((func_name (get-function-name funcall))
         (actual_params (get-actual-params funcall))
         (closure (lookup-function-closure func_name environment))
         (form_params (get-form-params-from-closure closure))
         (fn_body (get-fn-body-from-closure closure))
         (env-creator (get-env-creator-from-closure closure)))
    
    ; Interpret the function
    (interpret-statement-list
     fn_body ; statement list
     (env-creator environment actual_params) ; environment
     (lambda (v) v) ; return 
     (lambda (env) (myerror "Break used outside of loop")) ; break
     (lambda (env) (myerror "Continue used outside of loop")) ; continue
     throw ; throw
     (lambda (env) env) ; next
     )))

; Calls a function in a state
(define (interpret-funcall-state funcall environment return break continue throw next)
  ;(display "\nINTERPRETING FUNCALL STATE\n")
  ; Get the function parameters
  (let* ((func_name (get-function-name funcall))
         (actual_params (get-actual-params funcall))
         (closure (lookup-function-closure func_name environment))
         (form_params (get-form-params-from-closure closure))
         (fn_body (get-fn-body-from-closure closure))
         (env-creator (get-env-creator-from-closure closure)))
    
    ; Interpret the function
    (next
     (begin
       (interpret-statement-list
        fn_body
        (env-creator environment actual_params)
        (lambda (ret) (next environment))
        (lambda (env) (myerror "Break used outside of loop"))
        (lambda (env) (myerror "Continue used outside of loop"))
        throw
        next)
       environment))))

; Adds a new function to the environment. Global functions are declared with the global variables. Nested functions are declared with the local variables
(define interpret-function
  (lambda (statement environment next)
    (if (eq? 'main (get-function-name statement))
        ; Run main function
        (interpret-funcall-value '(funcall main)
                                 (insert-function (get-function-name statement) (get-formal-params statement) (get-function-body statement) environment)
                                 (lambda (v env) (myerror "Uncaught exception thrown")))
        ; Add function to the environment
        (next (insert-function (get-function-name statement) (get-formal-params statement) (get-function-body statement) environment)))))

; Calls the return continuation with the given expression value
(define (interpret-return statement environment return throw)
 ; (display "\nenv in interpret-return: ")
  ;(display environment)
  (return (eval-expression (get-expr statement) environment throw)))

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define (interpret-declare statement environment next throw)
  (if (exists-declare-value? statement)
      (next (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment throw) environment))
      (next (insert (get-declare-var statement) 'novalue environment))))

; Updates the environment to add a new binding for a variable
(define (interpret-assign statement environment next throw)
  (next (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment throw) environment)))

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define (interpret-if statement environment return break continue throw next)
  (cond
    ((eval-expression (get-condition statement) environment throw) (interpret-statement (get-then statement) environment return break continue throw next))
    ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw next))
    (else (next environment))))

; Interprets a while loop.  We must create break and continue continuations for this loop
(define (interpret-while statement environment return throw next)
  (letrec ((loop (lambda (condition body environment)
                   (if (eval-expression condition environment throw)
                       (interpret-statement body environment return (lambda (env) (next env)) (lambda (env) (loop condition body env)) throw (lambda (env) (loop condition body env)))
                       (next environment)))))
    (loop (get-condition statement) (get-body statement) environment)))

; Interprets a block.  The break, continue, throw and "next statement" continuations must be adjusted to pop the environment
(define (interpret-block statement environment return break continue throw next)
  (interpret-statement-list (remainingframes statement)
                                       (push-frame environment)
                                       return
                                       (lambda (env) (break (pop-frame env)))
                                       (lambda (env) (continue (pop-frame env)))
                                       (lambda (v env) (throw v (pop-frame env)))
                                       (lambda (env) (next (pop-frame env)))))

; We use a continuation to throw the proper value.  Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define (interpret-throw statement environment throw)
  (throw (eval-expression (get-expr statement) environment throw) environment))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define (create-throw-catch-continuation catch-statement environment return break continue throw next finally-block)
  (cond
    ((null? catch-statement) (lambda (ex env) (interpret-block finally-block env return break continue throw (lambda (env2) (throw ex env2))))) 
    ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
    (else (lambda (ex env)
                (interpret-statement-list 
                     (get-body catch-statement) 
                     (insert (catch-var catch-statement) ex (push-frame env))
                     return 
                     (lambda (env2) (break (pop-frame env2))) 
                     (lambda (env2) (continue (pop-frame env2))) 
                     (lambda (v env2) (throw v (pop-frame env2))) 
                     (lambda (env2) (interpret-block finally-block (pop-frame env2) return break continue throw next)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define (interpret-try statement environment return break continue throw next)
  (let* ((finally-block (make-finally-block (get-finally statement)))
         (try-block (make-try-block (get-try statement)))
         (new-return (lambda (v) (interpret-block finally-block environment return break continue throw (lambda (env2) (return v)))))
         (new-break (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (break env2)))))
         (new-continue (lambda (env) (interpret-block finally-block env return break continue throw (lambda (env2) (continue env2)))))
         (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw next finally-block)))
    (interpret-block try-block environment new-return new-break new-continue new-throw (lambda (env) (interpret-block finally-block env return break continue throw next)))))


; get function closure
(define lookup-function-closure
  (lambda (function enviroment)
    (cond
      ; if empty we couldn't find it
      ((equal? enviroment '((()()))) 
       (error "function not found"))
      ; if its not in that first frame remove first frame and try again
      ((not (exists-in-list? function (first-frame-variables enviroment))) 
       (lookup-function-closure function (pop-frame enviroment)))
      ; if not the first variable in the first frame then remove first and then try again
      ((not (eq? function (car (first-frame-variables enviroment))))
       (lookup-function-closure function (cons (cons (cdr (first-frame-variables enviroment)) (list (cdr (first-frame-values enviroment)))) (pop-frame enviroment))))
      ; sometimes it's not the only thing left in the list, so take the car
      ((and (eq? function (car (first-frame-variables enviroment)))
            (list? (caar (first-frame-values enviroment))))
       (car (first-frame-values enviroment)))
      ; if its the first variable in the first frame then return the value
      ((eq? function (car (first-frame-variables enviroment)))
       (first-frame-values enviroment))
      ; else something went wrong
      (else
       (error "lookup failed")
       ))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define (make-try-block try-statement) (cons 'begin try-statement))

(define (make-finally-block finally-statement)
  (cond
    ((null? finally-statement) '(begin))
    ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
    (else (cons 'begin (operand1 finally-statement)))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define (eval-expression expr enviroment throw)
  (eval-expression-cps expr enviroment throw (lambda (v) v)))

(define eval-expression-cps
  (lambda (expr environment throw return)
  ;  (display "\nenv in eval-expression-cps: ")
   ; (display environment)
    (cond
      ((number? expr) (return expr))
      ((eq? expr 'true) (return #t))
      ((eq? expr 'false) (return #f))
      ((not (list? expr)) (return (lookup expr environment)))
      ((eq? (car expr) 'funcall) (return (interpret-funcall-value expr environment throw)))
      (else (return (eval-operator expr environment throw))))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define (eval-operator expr enviroment throw) (eval-operator-cps expr enviroment throw (lambda (v) v)))

(define (eval-operator-cps expr environment throw return)
  (cond
    ((eq? '! (operator expr))
     (eval-expression-cps (operand1 expr) environment throw
                      (lambda (r-op1) (return (not r-op1)))))
    ((and (eq? '- (operator expr)) (= 2 (length expr)))
     (eval-expression-cps (operand1 expr) environment throw
                      (lambda (r-op1) (return (- r-op1)))))
    (else (eval-expression-cps (operand1 expr) environment throw
                      (lambda (r-op1) (return (eval-binary-op2 expr r-op1 environment throw)))))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define (eval-binary-op2 expr op1value enviroment throw) (eval-binary-op2-cps expr op1value enviroment throw (lambda (v) v)))

(define (eval-binary-op2-cps expr op1value environment throw return)
  (cond
    ((eq? '+ (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (+ op1value r-op2)))))
    ((eq? '- (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (- op1value r-op2)))))
    ((eq? '* (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (* op1value r-op2)))))      
    ((eq? '/ (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (quotient op1value r-op2)))))
    ((eq? '% (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (remainder op1value r-op2)))))
    ((eq? '== (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (isequal op1value r-op2)))))
    ((eq? '!= (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (not(isequal op1value r-op2))))))
    ((eq? '< (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (< op1value r-op2)))))
    ((eq? '> (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (> op1value r-op2)))))
    ((eq? '<= (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (<= op1value r-op2)))))
    ((eq? '>= (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (>= op1value r-op2)))))
    ((eq? '|| (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (or op1value r-op2)))))
    ((eq? '&& (operator expr))
     (eval-expression-cps (operand2 expr) environment throw
                      (lambda (r-op2) (return (and op1value r-op2)))))
    (else (myerror "Unknown operator:" (operator expr)))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define (isequal val1 val2)
  (if (and (number? val1) (number? val2))
      (= val1 val2)
      (eq? val1 val2)))

;-----------------
; HELPER FUNCTIONS
;-----------------

(define (atom? x) (not (pair? x)))

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

;(define remove1 cdr)

(define (exists-operand2? statement) (not (null? (cddr statement))))
(define (exists-operand3? statement) (not (null? (cdddr statement))))

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
(define get-function-name operand1)

(define get-formal-params operand2)
(define get-function-body operand3)

(define get-actual-params cddr)

(define (catch-var catch-statement) (operator (operand1 catch-statement)))

;------------------------
; Closure Functions
;------------------------

; Create Closure Function
(define (create_closure_function formal_param_list)
  (lambda (current_env actual_param_list)
    (function-environment current_env actual_param_list formal_param_list)))

; Makes the closure
(define (make_closure formal_params body state) (list formal_params body (create_closure_function formal_params)))

; takes in the function closure, just returns the list of formal parameters
(define (get-form-params-from-closure function_closure) (operator function_closure))

; takes in the closure and returns just the body
(define (get-fn-body-from-closure function_closure)
  (operand1 function_closure))

; takes in the closure and returns the function that creates a new environment
(define (get-env-creator-from-closure function_closure)
  (operand2 function_closure))

;----------------------------
; Environment/State Functions
;----------------------------

; create a new empty environment
(define (initenvironment) (list (emptyframe)))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define (emptyframe) '(() ()))

; Creates a new environment for a function from the global variables and the parameters
(define (newenvironment global params) (list (operator params) (operator global)))

; Gets the global variables out of an environment
(define (get-globals env) (get-globals-cps env (lambda (v) v)))

; Helper to make this tail recursive
(define (get-globals-cps env return)
  (cond
    ((null? env) (return '((() ()))))
    ((null? (remainingframes env)) (return env))
    (else (return (get-globals-cps (remainingframes env) (lambda (v) v))))))

; Binds the parameters to the values (or value of the expressions) that they are passed in with.
(define (bind-actual-formal env actual-param-list formal-param-list)
  (operator (bind-actual-formal-helper env actual-param-list formal-param-list '((() ())) (lambda (v) v) (lambda (v) v))))

; Preforms the bindings for the bind-actual-formal function
(define (bind-actual-formal-helper env actual-param-list formal-param-list binding return throw)
  (if (null? actual-param-list)
      (if (null? formal-param-list)
          (return binding)
          (error "The formal and actual parameters must match"))
      (return (bind-actual-formal-helper env (remainingframes actual-param-list) (remainingframes formal-param-list)
                                             (insert (operator formal-param-list) (eval-expression (operator actual-param-list) env throw) binding) return throw))))

; Create the environment for the function
(define (function-environment current-env actual-param-list formal-param-list)
  (cons (bind-actual-formal current-env actual-param-list formal-param-list)
  (get-globals current-env)))

; creates a binding of the 2 lists
(define (bind-parameters env actual formal return)
  (cond
    ; Either the actual or formal parameters is empty
    ((null? actual)
     (if (null? formal)
         (return env)
         (error "The formal and actual parameters must match")))
    ((null? formal) (error "The formal and actual parameters must match"))
    ; Otherwise
    (else env)))

; add a frame onto the top of the environment
(define (push-frame environment) (cons (emptyframe) environment))

; remove a frame from the environment
(define (pop-frame environment) (remainingframes environment))

; some abstractions
(define topframe operator)
(define remainingframes cdr)

; does a variable exist in the environment?
(define (exists? var environment)
  (cond
    ((null? environment) #f)
    ((exists-in-list? var (variables (topframe environment))) #t)
    (else (exists? var (remainingframes environment)))))

; does a variable exist in a list?
(define (exists-in-list? var l)  
  (cond
    ((null? l) #f)
    ((eq? var (operator l)) #t)
    (else (exists-in-list? var (remainingframes l)))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define (lookup var environment) (lookup-variable var environment))

; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define (lookup-variable var environment)
 ; (display "\nenv in lookup-variable: ")
  ;(display environment)
  (let ((value (lookup-in-env var environment)))
    (if (eq? 'novalue value)
        (myerror "error: variable without an assigned value:" var)
        value)))

;(define (disp msg)
 ; (display msg)
  ;(display "\n"))

; Return the value bound to a variable in the environment
(define (lookup-in-env var environment)
  ; Debugging stuff
;  (disp "Looking up:")
;  (disp var)
;  (disp environment)
;  (display "Option chosen: ")
;  (cond
;    ((null? environment) (disp "null"))
;    ((exists-in-list? var (variables (topframe environment))) (disp "exists"))
;    (else (disp "not in current frame")))
;  (display "\n")
;  (display "\nenv in lookup-in-env: ")
 ; (display environment)

  ; Actual code
  (cond
    ((null? environment) (myerror "error: undefined variable" var))
;    ((atom? environment) environment)
    ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (operator environment)))
    (else (lookup-in-env var (cdr environment)))))

; Return the value bound to a variable in the frame
(define (lookup-in-frame var frame)  
  (if (exists-in-list? var (variables frame))
      (language->scheme (unbox (get-value (indexof var (variables frame)) (store frame))))
      (myerror "error: undefined variable" var)))

; Get the location of a name in a list of names
(define (indexof var l)
  (cond
    ((null? l) 0)  ; should not happen
    ((eq? var (operator l)) 0)
    (else (+ 1 (indexof var (remainingframes l))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable (or a function) already exists in this frame.
(define (insert var val environment)
  (if (exists-in-list? var (variables (operator environment)))
      (myerror "error: variable is being re-declared:" var)
      (cons (add-to-frame var val (operator environment)) (remainingframes environment))))

; Adds a new function/(params) (body) pair into the environment. Gives an error if the function (or a variable) already exists in this frame.
(define (insert-function name formal-params func-body environment)
  (if (exists-in-list? name (variables (operator environment)))
      (myerror "error: variable is being re-declared:" name)
      (cons (add-func-to-frame name (make_closure formal-params func-body environment) (operator environment)) (remainingframes environment))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define (update var val environment)
  (if (exists? var environment)
      (update-existing var val environment)
      (myerror "error: variable used but not defined:" var)))

; Add a new variable/value pair to the frame.
(define (add-to-frame var val frame)
  (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame))))

; Add a new name,function_closure pair to the frame.
(define add-func-to-frame
  (lambda (name closure frame)
    (list (cons name (variables frame)) (cons closure (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define (update-existing var val environment)
  (if (exists-in-list? var (variables (operator environment)))
      (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
      (cons (topframe environment) (update-existing var val (remainingframes environment)))))

; Changes the binding of a variable in the frame to a new value.
(define (update-in-frame var val frame)
  (list (variables frame) (update-in-frame-store var val (variables frame) (store frame))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define (update-in-frame-store var val varlist vallist)
  (if (eq? var (operator varlist))
      (begin (set-box! (operator vallist) (scheme->language val)) vallist)
      (cons (operator vallist) (update-in-frame-store var val (remainingframes varlist) (remainingframes vallist)))))

; Returns the list of variables from a frame
(define (variables frame) (operator frame))

; Returns the store from a frame
(define (store frame) (operand1 frame))

; returns list of variables from the first frame
(define (first-frame-variables env) (caar env))

; returns list of values from the first frame
(define (first-frame-values env) (cadar env))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.
(define (language->scheme v)
  (cond 
    ((eq? v 'false) #f)
    ((eq? v 'true) #t)
    (else v)))

(define (scheme->language v)
  (cond
    ((eq? v #f) 'false)
    ((eq? v #t) 'true)
    (else v)))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define (myerror str . vals)
  (error str))
  
;  (letrec ((makestr (lambda (str vals)
 ;                     (if (null? vals)
  ;                        str
   ;                       (makestr (string-append str (string-append " " (symbol->string (operator vals)))) (remainingframes vals))))))
    ;(error-break (display (string-append (string-append str (makestr "" vals)) "\n")))))


(display "Start Debugging:\n")

; Test environments
;(check-equal? (newenvironment (insert 'a 10 '((() ()))) (insert 'a 1 (insert 'b 5 '((() ()))))) '(((a b) (#&1 #&5)) ((a) (#&10))))

; Check getting the global variables
;(check-equal? (get-globals '((() ()))) '((() ())))
;(check-equal? (get-globals (insert 'a 1 (insert 'b 5 '((() ()))))) '(((a b) (#&1 #&5))))
;(check-equal? (get-globals (newenvironment (insert 'a 10 '((() ()))) (insert 'a 1 (insert 'b 5 '((() ())))))) '(((a) (#&10))))

; Check binding variables to values
;(define global_var (newenvironment (insert 'a 10 '((() ()))) (insert 'a 1 (insert 'b 5 '((() ()))))))
;(define parameter_definitions '(x y z))
;(define parameter_bindings '(a 10 (+ a b)))
;(check-equal? (bind-actual-formal global_var parameter_bindings parameter_definitions) '((z y x) (#&6 #&10 #&1)))

; (function-environment current-env defined-params passed-in-params)
;(check-equal? (function-environment global_var parameter_bindings parameter_definitions) '(((z y x) (#&6 #&10 #&1)) ((a) (#&10))))


; function definition
; Without parameters
;(define add-function (interpret-function '(function add () (return 1)) (initenvironment) (lambda (v) v)))
;(check-equal? (caaar add-function) 'add)
;(check-equal? (car (caadar add-function)) '())
;(check-equal? (car (cdr (caadar add-function))) '(return 1))

; With parameters
;(define add-function2 (interpret-function '(function add (a b) (return (+ a b))) (initenvironment) (lambda (v) v)))
;(check-equal? (caaar add-function2) 'add)
;(check-equal? (car (caadar add-function2)) '(a b))
;(check-equal? (car (cdr (caadar add-function2))) '(return (+ a b)))

;
; (lambda (funcall environment throw)
;(interpret-funcall-value '(funcall add) add-function (lambda (v) v))

;(((add) ((() (return 1) #<procedure:...ts/interpreter2.rkt:311:2>))))

;(lookup-function-closure 'f '(((main myfunc x)
 ;  ((() ((funcall myfunc 6 x)) #<procedure:...ts/interpreter2.rkt:311:2>)
 ;   ((a b) ((= x (+ a b))) #<procedure:...ts/interpreter2.rkt:311:2>)
;    #&10)))


;(interpret "scratch.bad")

;(lookup 'r '(((z r) (1 2)) ((main myfunc x)
 ;  ((() ((funcall myfunc 6 x)) procedure)
 ;   ((a b) ((= x (+ a b))) procedure)
 ;   #&10))))

;(lookup-function-closure 'myfunc '(((z r) (1 2)) ((main myfunc x)
;   ((() ((funcall myfunc 6 x)) procedure)
;    ((a b) ((= x (+ a b))) procedure)
;    #&10)) ((q l) (3 4))
;       ))

; test lookup-function-closure
;(check-equal? (lookup-function-closure 'myfunc '(((z r) (1 2)) ((main myfunc x)
 ;  ((() ((funcall myfunc 6 x)) procedure)
  ;  ((a b) ((= x (+ a b))) procedure)
   ; #&10)) ((q l) (3 4))
     ;  )) '((a b) ((= x (+ a b))) procedure))


; create-closure -> formal parameters function
;(check-equal? (get-form-params-from-closure '((a b) ((= x (+ a b))) procedure)) '(a b))
; create-closure -> function body function
;(check-equal? (get-fn-body-from-closure '((a b) ((= x (+ a b))) procedure)) '((= x (+ a b))))
; create-closure -> env-creator-function
;(check-equal? (get-env-creator-from-closure '((a b) ((= x (+ a b))) procedure)) 'procedure)


; test
;(define e '(((main setX x) ((() ((funcall setX 2) (return x)) 'p1) ((a) ((= x a)) 'p2) #&2)))) ; leave commented
;(define e '(((main setX x) ((() ((funcall setX 2) (return x)) 'p1) ((a) ((= x a)) 'p2) #&2))))
;(disp e) ; displays the environment
;(disp (topframe e)) ; displays the frame
;(disp (variables (topframe e))) ; displays the variables names that are in the frame
;(disp (lookup-in-frame 'x (topframe e))) ; Lookup the value of the variable
;(disp "\n")
;(check-equal? (lookup-in-env 'x e) 2)

;(disp "\nStarting the interpret:\n")
;(check-equal? (interpret "tests/foo.bad") 2)
;(check-equal? (interpret "tests/bar.bad") 5)
;(check-equal? (interpret "tests/p3_t11.bad") 35)
;(interpret "tests/p3_t11.bad")
;(interpret "tests/foo.bad")
;(interpret "tests/bar.bad")
