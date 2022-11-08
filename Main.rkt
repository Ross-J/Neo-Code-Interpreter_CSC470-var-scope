#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Environment.rkt")



;(define scope '((a 1) (b 2) (c 5)))
;environment is global scope and also any other scope added afterward
(define env '((global (a 0) (x 7) (z 'x))))

;uses global environment - x is parameter, a = 1
;currently, global environment is extended to include x = 5
(define sample-code1 '(call (function(x) (math + x a)) (5)))

;here we see the weakness of the current var-scope implementation
;this is a problem we need to solve
(define sample-code2 '(call (function (a) (call (function (r) a) (a))) (5)))

(define sample-code3 '(call (function(x) (local-vars ((a 3) (b 7) (c 3)) (math + a b))) (5)))

(define sample-code4 '(call (function (a) (local-vars ((x 5) (y 6) (z 9)) ((call (function (b)(math + a (math * b x)))) (2)))) (3)))


;(display (neo-parser sample-code1))
(define parsed-neo-code (neo-parser sample-code2))
(run-neo-parsed-code parsed-neo-code env)

