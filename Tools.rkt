#lang Racket

(provide (all-defined-out))

; Tool to return element at a given index
(define elementAt
  (lambda (lst index)
    (cond
      ((not (list? lst)) "this is not a list")
      ((null? lst) "this is an empty list or index out of bound")
      ((equal? index 0) (car lst))
      (else (elementAt (cdr lst) (- index 1)))
    )
  )
)

; Tool to get variable names from variable scope
(define getVarNames
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (cdr (car lst))) (getVarNames (cdr lst)))
    )
  )
)

;Tool to get variable values from variable scope
(define getValues
  (lambda (lst)
    (if (null? lst) '()
        (cons (car (cdr (car lst))) (getValues (cdr lst)))
    )
  )
)