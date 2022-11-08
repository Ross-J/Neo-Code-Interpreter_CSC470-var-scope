#lang Racket

(provide (all-defined-out))

; Resolve a value from the variable environment
(define resolve_scope
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
      )
    )
  )
 

;note - environment is a list of scopes
;global variable scope = (global (a 1) (b 2) (c 5))
(define resolve_env
  (lambda resolve_env
    (lambda (environment varname)
      (cond
        ((null? environment) #false)
        ((null? (car environment)) (resolve_env (cdr environment) varname))
        ;if the global scope is found first, no other scopes need to be checked
        ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
        ;otherwise, local scope is found and must be used
        (else (let ((resolved_result (resolve_scope (car environment) varname)))
                (if (equal? resolved_result #false)
                    (resolve_env (cdr environment) varname)
                    resolved_result)
                )
              )
        )
      )
    )
  )






; Extend the local variable scope
(define extend_scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extend_scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
      )
    )
  )


#|
(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    ()
  )
)
|#