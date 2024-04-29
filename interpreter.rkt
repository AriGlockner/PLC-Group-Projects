; Ethan Hansen, Gabriel Wolf, Ari Glockner
; CSDS 345 Programming Language Concepts
; Interpreter Project 3
; Mar 2024

#lang racket
(require "classParser.rkt")
(require rackunit)
; (load "simpleParser.scm")

; An interpreter for the simple language using tail recursion for the M_state functions and does not handle side effects.

; The functions that start interpret-...  all return the current environment.  These are the M_state functions.
; The functions that start eval-...  all return a value.  These are the M_value and M_boolean functions.

(provide (all-defined-out))

; Interprets the file. After compilation, runs the class's main body from entryclass
(define (interpret file entryclass)
  (let* ((entryatom (string->symbol entryclass))
         (global-env (get-all-classes file entryatom))
         (entry-class-closure (find-class-closure entryatom global-env))
         (main-fn-closure (find-function-in-class 'main entry-class-closure))
         (main-env ((get-env-creator-from-closure main-fn-closure) global-env '()))
         (fn_body (operand1 main-fn-closure)))
    (scheme->language (execute-main fn_body main-env default-lambda
                                    (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                    (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env)))))

; Compiles all classes
(define (get-all-classes file entryclass)
  (scheme->language
   (interpret-statement-list (parser file) (initenvironment) default-lambda
                             (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                             (lambda (v env) (myerror "Uncaught exception thrown")) (lambda (env) env))))

; Executes the specified main function
(define (execute-main fn_body env return break continue throw next)
  (next (interpret-statement-list fn_body env return break continue throw next)))

; Finds a function within the class closure
(define (find-function-in-class function-name class-closure)
  ; Call the helper function with the fields are removed
  (lookup-function-closure function-name (remove2 (unbox class-closure))))

; interprets a list of statements.  The state/environment from each statement is used for the next ones.
(define (interpret-statement-list statement-list environment return break continue throw next)
  (if (null? statement-list)
      (next environment)
      (interpret-statement (operator statement-list) environment return break continue throw
                           (lambda (env) (interpret-statement-list (remainingframes statement-list) env return break continue throw next)))))

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
    ((eq? 'class (statement-type statement)) (interpret-class statement environment return break continue throw next))
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
    (interpret-statement-list fn_body (env-creator environment actual_params) default-lambda (lambda (env) (myerror "Break used outside of loop"))
                              (lambda (env) (myerror "Continue used outside of loop")) throw (lambda (env) env))))

; Calls a function in a state
(define (interpret-funcall-state funcall environment return break continue throw next)
  ; Get the function parameters
  (let* ((func_name (get-function-name funcall))
         (actual_params (get-actual-params funcall))
         (closure (lookup-function-closure func_name environment))
         (form_params (get-form-params-from-closure closure))
         (fn_body (get-fn-body-from-closure closure))
         (env-creator (get-env-creator-from-closure closure)))
    
    ; Interpret the function
    (next (begin (interpret-statement-list fn_body (env-creator environment actual_params)
                                           return (lambda (env) (myerror "Break used outside of loop"))
                                           (lambda (env) (myerror "Continue used outside of loop")) throw next) environment))))

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
(define (interpret-throw statement environment return break continue throw next)
  (throw (eval-expression (get-expr statement) environment throw) environment))

; interpret something like "class A {body}" and add the class closure to the global state
(define (interpret-class statement environment return break continue throw next)
  (next (add-class-closure statement environment)))

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
(define (lookup-function-closure function enviroment)
  (cond
    ; if empty we couldn't find it
    ((equal? enviroment '((()()))) (error "function not found"))
    ; if its not in that first frame remove first frame and try again
    ((not (exists-in-list? function (first-frame-variables enviroment)))
     (lookup-function-closure function (pop-frame enviroment)))
    ; if not the first variable in the first frame then remove first and then try again
    ((not (eq? function (operator (first-frame-variables enviroment))))
     (lookup-function-closure function (combine (combine (remove1 (first-frame-variables enviroment)) (list (remove1 (first-frame-values enviroment)))) (pop-frame enviroment))))
    ; sometimes it's not the only thing left in the list, so take the car
    ((and (eq? function (operator (first-frame-variables enviroment))) (list? (operator2 (first-frame-values enviroment))))
     (operator (first-frame-values enviroment)))
    ; if its the first variable in the first frame then return the value
    ((eq? function (operator (first-frame-variables enviroment)))
     (first-frame-values enviroment))
    ; else something went wrong
    (else (error "lookup failed"))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define (make-try-block try-statement) (combine 'begin try-statement))

(define (make-finally-block finally-statement)
  (cond
    ((null? finally-statement) '(begin))
    ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
    (else (combine 'begin (operand1 finally-statement)))))

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define (eval-expression expr enviroment throw)
  (eval-expression-cps expr enviroment throw default-lambda))

(define eval-expression-cps
  (lambda (expr environment throw return)
    (cond
      ((number? expr) (return expr))
      ((eq? expr 'true) (return #t))
      ((eq? expr 'false) (return #f))
      ((not (list? expr)) (return (lookup expr environment)))
      ((eq? (operator expr) 'funcall) (return (interpret-funcall-value expr environment throw)))
      (else (return (eval-operator expr environment throw))))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define (eval-operator expr enviroment throw) (eval-operator-cps expr enviroment throw default-lambda))

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
(define (eval-binary-op2 expr op1value enviroment throw) (eval-binary-op2-cps expr op1value enviroment throw default-lambda))

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

(define combine cons)

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operator2 caar)

(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define remove1 cdr)
(define remove2 cddr)
(define remove3 cdddr)

(define (exists-operand2? statement) (not (null? (remove2 statement))))
(define (exists-operand3? statement) (not (null? (remove3 statement))))

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

(define get-actual-params remove2)

(define (catch-var catch-statement) (operator (operand1 catch-statement)))

; Lambdas
(define default-lambda (lambda (v) v))

;------------------------
; Class Closure Stuff
;-----------------------

(define (add-class-closure statement environment)
  (let ((class_name (get-class-name statement)))
    (insert class_name (make-class-closure class_name statement environment) environment)))

(define (make-class-closure class_name statement environment)
  (let* ((body (get-class-body statement))
         (super_class (find-super-or-null (get-super-class-name statement) environment))
         (field_names_and_init (get-field-info body))
         (method_info (get-methods-info body class_name (get-globals environment))))
    (list super_class field_names_and_init method_info)))

; Gets the fields in a class
(define (get-field-info body) (get-field-info-cps body '(() ()) default-lambda))

; Helper function that gets the field info
(define (get-field-info-cps body state return)
  (cond
    ((null? body) (return state))
    ((eq? 'var (operator body)) (get-field-info-cps (remove1 body) (add-to-frame (cadar body) (caddar body) state) default-lambda))
    (else (return (get-field-info-cps (remove1 body) state default-lambda)))))

; (env) --> list of class names
(define (get-class-name-list env)
  (if (pair? env)
      (operator2 env)
      (error "env not a pair")))

; (env) --> list of class closures
(define (get-class-closure-list env)
  (if (pair? env)
      (cadar env)
      (error "env not a pair")))

; (name, env) --> find super or null --> find-class-closure
(define (find-super-or-null name env)
  (cond
    ((eq? name '()) 'null)
    ((eq? find-class-closure (operand1 name)))
    (else (find-class-closure name env))))

; (name of class, env) --> class closure
(define (find-class-closure name env)
  (if (empty? env)
      (error "empty env")
      (find-class-closure-cps name (get-class-name-list env) (get-class-closure-list env))))

; helper for find-class-closure
(define (find-class-closure-cps name class-names class-closures)
  (cond
    ((or (eq? class-names '()) (eq? class-names '())) (error "class does not exist in state"))
    ((eq? name (operator class-names)) (operator class-closures))
    (else (find-class-closure-cps name (remove1 class-names) (remove1 class-closures)))))

; (closure, var) --> var index
(define (get-var-index closure v)
  (if (eq? closure '())
      (error "closure is empty")
      (reverseindexof v (operand1 closure))))

; (statement) --> class name
(define (get-class-name statement)
  (if (null? statement)
      (error "class is empty")
      (operand1 statement)))

; (statement) --> super_class
(define (get-super-class-name statement)
  (cond
    ((empty? statement) (error "class is empty"))
    ((null? (operand2 statement)) '())
    (else (operand1 (operand2 statement)))))

; (statement) --> class_body
(define (get-class-body statement)
  (if (empty? statement)
      (error "class is empty")
      (operand3 statement)))

; (body) --> (list of static functions)
(define (get-static-functions-list body)
  (cond
    ((eq? body '()) '())
    ((eq? 'static-function (operator2 body))
     (if (eq? (cadar body) 'main)
         'main
         (get-static-functions-list (pop-frame body))))
    (else (get-static-functions-list (pop-frame body)))))

; (body) --> (list of functions)
(define (get-functions-list body)
 (cond
   ((eq? body '()) '())
   ((eq? 'function (operator2 body)) (combine (cadar body) (get-functions-list (pop-frame body))))
   (else (get-functions-list (pop-frame body)))))

; (body class-name global-env) --> ((methods names) (method closures))
(define (get-methods-info body class-name global-env)
  (cond
    ((eq? body '()) '(()()))
    ((or (eq? 'function (operator2 body)) (eq? 'static-function (operator2 body)))
     (let*
         ((name (cadar body))
          (formal-params (operator (cddar body)))
          (function-body (operand1 (cddar body)))
          (method-closure (create-method-closure class-name formal-params function-body global-env))
          (rest (get-methods-info (remove1 body) class-name global-env)))
       (list (combine name (operator rest)) (combine method-closure (operand1 rest)))))
    (else (get-methods-info (pop-frame body) class-name global-env))))
         
; another thing ethan said
(define (create-method-closure compile_type formal_params fn_body global_env)
  (list formal_params fn_body (make-method-env-creator formal_params global_env)
        (lambda () (find-class-closure (compile_type global_env)))))
    
; do the thing ethan says
(define (make-method-env-creator formal_param_list global_env)
  (lambda (current_env actual_param_list)
    (methods-env global_env current_env actual_param_list formal_param_list)))

(define (methods-env global_env current-env actual-param-list formal-param-list)
  (let ((env (combine (bind-actual-formal current-env actual-param-list formal-param-list) (list global_env)))) env))

;------------------------
; Closure Functions
;------------------------

; Create Closure Function
(define (create_closure_function formal_param_list static_env)
  (lambda (current_env actual_param_list)
    (function-environment static_env current_env actual_param_list formal_param_list)))

; Makes the closure
(define (make_closure formal_params body static-env)
  (list formal_params body (create_closure_function formal_params static-env)))

; takes in the function closure, just returns the list of formal parameters
(define (get-form-params-from-closure function_closure) (operator function_closure))

; takes in the closure and returns just the body
(define (get-fn-body-from-closure function_closure) (operand1 function_closure))

; takes in the closure and returns the function that creates a new environment
(define (get-env-creator-from-closure function_closure) (operand2 function_closure))

;---------------------------------------
; Create an Object (instantiate a class)
;---------------------------------------

; Creates a new instance of the class specified in the name parameter
(define (create-object name env) (create-instance-closure (find-class-closure name env) default-lambda))

; Copies the values from the class closure over to become the new object
(define (create-instance-closure class-closure return)
  (if (null? class-closure)
      (return '())
      (return (create-instance-closure (remove1 class-closure) (lambda (v) (combine (operator class-closure) v))))))

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
(define (get-globals env) (get-globals-cps env default-lambda))

; Helper to make this tail recursive
(define (get-globals-cps env return)
  (cond
    ((null? env) (return '((() ()))))
    ((null? (remainingframes env)) (return env))
    (else (return (get-globals-cps (remainingframes env) default-lambda)))))

; Binds the parameters to the values (or value of the expressions) that they are passed in with.
(define (bind-actual-formal env actual-param-list formal-param-list)
  (operator (bind-actual-formal-helper env actual-param-list formal-param-list '((() ())) default-lambda default-lambda)))

; Preforms the bindings for the bind-actual-formal function
(define (bind-actual-formal-helper env actual-param-list formal-param-list binding return throw)
  (if (null? actual-param-list)
      (if (null? formal-param-list)
          (return binding)
          (error "The formal and actual parameters must match"))
      (return (bind-actual-formal-helper env (remainingframes actual-param-list) (remainingframes formal-param-list)
                                             (insert (operator formal-param-list) (eval-expression (operator actual-param-list) env throw) binding) return throw))))

; Create the environment for the function
(define (function-environment static-env current-env actual-param-list formal-param-list)
  (let ([env (combine (bind-actual-formal current-env actual-param-list formal-param-list)
                   (append (kill-global-static static-env) (get-globals current-env)))])
    env))


; kill global static
(define (kill-global-static static) (remove-last static))

; creates a binding of the 2 lists
(define (bind-parameters env actual formal return)
  (cond
    ; Either the actual or formal parameters is empty
    ((null? actual) (if (null? formal)
                        (return env)
                        (error "The formal and actual parameters must match")))
    ((null? formal) (error "The formal and actual parameters must match"))
    ; Otherwise
    (else env)))

; add a frame onto the top of the environment
(define (push-frame environment) (combine (emptyframe) environment))

; remove a frame from the environment
(define (pop-frame environment) (remainingframes environment))

; some abstractions
(define topframe operator)
(define remainingframes remove1)

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
  (let ((value (lookup-in-env var environment)))
    (if (eq? 'novalue value)
        (myerror "error: variable without an assigned value:" var)
        value)))

; Return the value bound to a variable in the environment
(define (lookup-in-env var environment)
  (cond
    ((null? environment) (myerror "error: undefined variable" var))
    ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
    (else (lookup-in-env var (remove1 environment)))))

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
(define (get-value n l)
  (if (zero? n)
      (operator l)
      (get-value (- n 1) (remove1 l))))

; check if env is empty
(define (empty? env)
  (cond
    ((eq? env '((()()))) #t)
    ((null? env) #t)
    (else #f)))

; reverse index of
(define (reverseindexof var l)
  (define (reverseindexof-helper var l index)
    (cond
      ((null? l) -1)  ; not found
      ((eq? var (operator l)) index)  ; found, return index
      (else (reverseindexof-helper var (remove1 l) (- index 1)))))  ; continue searching with decremented index
  (reverseindexof-helper var l (- (length l) 1)))  ; start with the length of the list as the initial index

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable (or a function) already exists in this frame.
(define (insert var val environment)
  (if (exists-in-list? var (variables (operator environment)))
      (myerror "error: variable is being re-declared:" var)
      (combine (add-to-frame var val (operator environment)) (remainingframes environment))))

; Adds a new function/(params) (body) pair into the environment. Gives an error if the function (or a variable) already exists in this frame.
(define (insert-function name formal-params func-body environment)
  (if (exists-in-list? name (variables (operator environment)))
      (myerror "error: variable is being re-declared:" name)
      (combine (add-func-to-frame name (make_closure formal-params func-body environment) (operator environment)) (remainingframes environment))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define (update var val environment)
  (if (exists? var environment)
      (update-existing var val environment)
      (myerror "error: variable used but not defined:" var)))
        

; Add a new variable/value pair to the frame.
(define (add-to-frame var val frame)
  (list (combine var (variables frame)) (combine (box (scheme->language val)) (store frame))))

; Add a new name,function_closure pair to the frame.
(define (add-func-to-frame name closure frame)
  (list (combine name (variables frame)) (combine closure (store frame))))

; Changes the binding of a variable in the environment to a new value
(define (update-existing var val environment)
  (if (exists-in-list? var (variables (operator environment)))
      (combine (update-in-frame var val (topframe environment)) (remainingframes environment))
      (combine (topframe environment) (update-existing var val (remainingframes environment)))))

; Changes the binding of a variable in the frame to a new value.
(define (update-in-frame var val frame)
  (list (variables frame) (update-in-frame-store var val (variables frame) (store frame))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define (update-in-frame-store var val varlist vallist)
  (if (eq? var (operator varlist))
      (begin (set-box! (operator vallist) (scheme->language val)) vallist)
      (combine (operator vallist) (update-in-frame-store var val (remainingframes varlist) (remainingframes vallist)))))

; Returns the list of variables from a frame
(define (variables frame) (operator frame))

; Returns the store from a frame
(define (store frame) (operand1 frame))

; returns list of variables from the first frame
(define (first-frame-variables env) (operator2 env))

; returns list of values from the first frame
(define (first-frame-values env) (cadar env))

; remove last elment of list
(define (remove-last lst)
  (if (null? (remove1 lst)) '()
      (combine (operator lst) (remove-last (remove1 lst)))))


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
(define error-break default-lambda)
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define (myerror str . vals) (error str))

; create the syntax debug
; (+ 1 (debug x))
; prints the value of x while still using x in the addition
(define-syntax (debug syn)
  (define slist (syntax->list syn))
  (datum->syntax syn `(let ((y ,(cadr slist))) (begin (println y) y))))

; println to print with a newline
(define-syntax (println syn)
  (define slist (syntax->list syn))
  (datum->syntax syn `(begin (print ,(cadr slist)) (newline))))
