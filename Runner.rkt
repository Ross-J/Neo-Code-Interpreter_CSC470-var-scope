#lang Racket

(require "Variable_Environment.rkt")
(require "Tools.rkt")
(provide (all-defined-out))


; Parsed Neo-Code Runner
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code))
      ((equal? (car parsed-code) 'var-exp)
       (resolve_env env (cadr parsed-code)))
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code) env))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code env))

      ;(print-exp (var-exp a)) should result in -> **screen** 1
      ((equal? (car parsed-code) 'print-exp)
       (run-print-exp (cadr parsed-code) env))
      
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (push_scope_to_env (cadr (cadr (cadr parsed-code)))
                                (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                                env
                                )
             )
      )            
    )
  ) 
)


; Runner for parsed boolean expressions
(define run-bool-parsed-code
  (lambda(parsed-code env)
    (let ((op (elementAt parsed-code 0))
           (num1 (run-neo-parsed-code (elementAt parsed-code 1) env))
           (num2 (run-neo-parsed-code (elementAt parsed-code 2) env)))
           (cond
             ((equal? op '>) (> num1 num2))
             ((equal? op '<) (< num1 num2))
             ((equal? op '>=) (>= num1 num2))
             ((equal? op '<=) (<= num1 num2))
             ((equal? op '==) (= num1 num2))
             ((equal? op '!=) (not (= num1 num2)))
             ((equal? op '&&) (and num1 num2))
             ((equal? op '||) (or num1 num2))
             (else (not num1))
        )
      )
    )
  )


; Runner for parsed math expressions
(define run-math-exp
  (lambda (op num1 num2)
    (cond
      ((equal? op '+) (+ num1 num2))
      ((equal? op '-) (- num1 num2))
      ((equal? op '*) (* num1 num2))
      ((equal? op '/) (/ num1 num2)) 
      ((equal? op '//) (quotient num1 num2))
      ((equal? op '%) (modulo num1 num2))
      (else #false)
    )
  )
)


;Runner for parsed print expressions
(define run-print-exp
  (lambda (parsed-code env)
    (display (string-append "**screen** " (number->string
                                           (run-neo-parsed-code parsed-code env))))
  )
)
   

; Runner for parsed let expressions
(define run-let-exp
  (lambda (parsed-code env)
    (let*
        ((new_env (cascade-update-env (elementAt parsed-code 1) env))
          (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env)
    )
  )
)


(define cascade-update-env
  (lambda (parsed-scope env)
    (if (null? parsed-scope) env
        (let* (
               (local-scope (if (equal? (car (car env)) 'global)
                                '()
                                (car env)))
               (global-scope-env (pop_env_to_global_scope env))
               (first-name-value-pair (car parsed-scope))
               (new-local-scope (cons (list (car first-name-value-pair)
                                            (run-neo-parsed-code (cadr first-name-value-pair) env))
                                            local-scope))
               (new-env (cons new-local-scope global-scope-env))
               )
          (cascade-update-env (cdr parsed-scope) new-env)
        )       
    )
  )
)
