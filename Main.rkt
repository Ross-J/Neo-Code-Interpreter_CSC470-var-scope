#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Environment.rkt")


(define scope '((a 1) (b 2) (c 5)))
(define env '((global (a 1) (b 2) (c 5))))

  


;(define sample-code '(call (function (a) (call (function (r) a) (a))) (5)))
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))

;(define sample-code '(print a)) 
;(define sample-code '(assign x 8))
;(define sample-code '(block (assign x 8) (print x)))

(define sample-code '(block (print a) (assign x 8) (assign y (math * x 2)) (print y) (assign z (math + b y)) (print z)))


(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)
