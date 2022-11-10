#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Environment.rkt")


(define scope '((a 1) (b 2) (c 5)))
(define env '((global (a 1) (b 2) (c 5))))

  

;(define sample-code '(call (function (a) a) (5)))
;(define sample-code '(call (function(x) (math + x a)) (5)))
(define sample-code '(call (function (a) (call (function (r) a) (a))) (5)))


;(define sample-code '(local-vars ((p c)) (math / a p)))
;(define sample-code '(call (function (r) (local-vars ((p 100)) (math / r p)) ) (a)))
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))



(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(run-neo-parsed-code parsed-neo-code env)
