#lang Racket

(require "Parser.rkt")
(require "Runner.rkt")
(require "Tools.rkt")
(require "Variable_Environment.rkt")


(define scope '((a 1) (b 2) (c 5)))
(define env '((global (a 1) (b 2) (c 5))))

  


;(define sample-code '(call (function (a) (call (function (r) a) (a))) (5)))
;(define sample-code '(local-vars ((a 7) (b a) (x b)) (math + x a)))

;(display (string-append "**screen** " (number->string 1)))
(define sample-code '(print a)) ;should print out -> **screen** 1



(displayln (neo-parser sample-code))
(define parsed-neo-code (neo-parser sample-code))
(displayln (run-neo-parsed-code parsed-neo-code env))
