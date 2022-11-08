#lang Racket

(require "Tools.rkt")
(require "Variable_Environment.rkt")
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
      
      ((equal? (car parsed-code) 'bool-exp) (run-bool-parsed-code (cdr parsed-code env)))
      ((equal? (car parsed-code) 'math-exp) ;(run-math-parsed-code (cadr parsed-code env)))
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
       (run-let-exp parsed-code))
        
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (extend_scope
              (cadr (cadr (cadr parsed-code)))
              (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
              env)
             )
            )
      )
    ) 
  )


; Runner for parsed boolean expressions
(define run-bool-parsed-code
  (lambda (parsed-code env)
    (let ((op (cadr parsed-code))
          (num1 (run-neo-parsed-code (caddr parsed-code) env))
          (num2 (run-neo-parsed-code (cadddr parsed-code) env)))
      (cond
        ((equal? op '>) (> num1 num2))
        ((equal? op '<) (< num1 num2))
        ((equal? op '>=) (>= num1 num2))
        ((equal? op '<=) (<= num1 num2))
        ((equal? op '==) (= num1 num2))
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


(define run-math-parsed-code
  (lambda (parsed-code env)
    (let ((op (cadr parsed-code))
          (num1 (run-neo-parsed-code (caddr parsed-code) env))
          (num2 (run-neo-parsed-code (cadddr parsed-code) env)))
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
)


; Runner for parsed let expressions
(define run-let-exp
  (lambda (parsed-code env)
    (let* ((list-of-names (getVarNames (elementAt parsed-code 1)))
           (list-of-values (getValues (elementAt parsed-code 1)))
           (new_env (extend_scope list-of-names list-of-values env))
           (body (elementAt parsed-code 2)))
      (run-neo-parsed-code body new_env)
    )
  )
)

