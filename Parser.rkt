#lang Racket

(require "Tools.rkt")
(provide (all-defined-out))


; Neo-Code to Scheme Parser
(define neo-parser
  (lambda (neo-code)
    (cond
      ((null? neo-code) '())
      ((number? neo-code) (list 'num-exp neo-code))
      ((symbol? neo-code) (list 'var-exp neo-code))
       ((equal? (car neo-code) 'bool) (neo-bool-code-parser neo-code))
      ((equal? (car neo-code) 'math) (neo-math-code-parser neo-code))
      ((equal? (car neo-code) 'ask) (neo-ask-code-parser neo-code))
      ((equal? (car neo-code) 'function) (neo-function-code-parser neo-code))
      ((equal? (car neo-code) 'call) (neo-call-code-parser neo-code))
      ((equal? (car neo-code) 'local-vars) (neo-let-code-parser neo-code))
      
      ((equal? (car neo-code) 'print)
       (list 'print-exp (neo-parser (cadr neo-code))))

      ;(assign x 8) -> (assign-exp x (num-exp 8))
      ((equal? (car neo-code) 'assign)
       (list 'assign-exp (cadr neo-code) (neo-parser (caddr neo-code))))

      ;(block (assign x 8) (print x)) -> (block-exp (assign-exp x (num-exp 8) (print-exp (var-exp 8))))
      ((equal? (car neo-code) 'block)
       (cons 'block-exp (neo-parser (cdr neo-code))))

      (else (map neo-parser neo-code))
    )
  )
)

; Parser for boolean expressions
(define neo-bool-code-parser
  (lambda (neo-code)
     (if (equal? (length neo-code) 3)
            (list 'bool-exp (elementAt neo-code 1) (neo-parser (caddr neo-code)) '())
        (cons 'bool-exp (cons (cadr neo-code) (map neo-parser (cddr neo-code)))))     
  )
)

; Parser for math expressions
(define neo-math-code-parser
  (lambda (neo-code)
    (list 'math-exp (cadr neo-code)
             (neo-parser (caddr neo-code))
             (neo-parser (cadddr neo-code)))
  )
)

; Parser for function expressions
(define neo-function-code-parser
  (lambda (neo-code)
    (list 'func-exp
             (list 'params (cadr neo-code))
             (list 'body-exp (neo-parser (caddr neo-code))))
  )
)

; Parser for ask expressions
(define neo-ask-code-parser
  (lambda (neo-code)
    (cons 'ask-exp
             (map neo-parser (cdr neo-code)))
  )
)

; Parser for call expressions
(define neo-call-code-parser
  (lambda(neo-code)
    (list 'app-exp
             (neo-parser (cadr neo-code))
             (neo-parser (caddr neo-code)))
  )
)

; Parser for let expressions
(define neo-let-code-parser
  (lambda (neo-code)
    (list 'let-exp
          (map (lambda (pair) (list (car pair) (neo-parser (elementAt pair 1))))
               (elementAt neo-code 1))
           (neo-parser (elementAt neo-code 2)))
  )
)