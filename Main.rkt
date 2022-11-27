#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Environment.rkt")


;(define scope '((a 1) (b 2) (c 5)))
(define env '((global (a 1) (b 2) (c 5))))

 

;(define sample-code '(print a)) 
;(define sample-code '(assign x 8))
;(define sample-code '(block (assign x 8) (print x)))


(define sample-code '(block (print a) (assign x 8)
                            (assign y (math * x 2)) (print y)
                            (assign z (math + b y)) (print z)))

#|(define sample-code '(block (assign i 0) (while (bool < i 10)
                           (block (assign a (math + i 1))
                                  (assign i a) (print i)))))|#


(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)

;(define scope '((y 10) (x 8)))
;(display (update_scope 'x 16 scope))
