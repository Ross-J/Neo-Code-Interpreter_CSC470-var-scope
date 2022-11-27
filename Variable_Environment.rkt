#lang Racket


(provide (all-defined-out))


; Resolve a value from a scope in the variable environment
(define resolve_scope
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) varname))
    )
  )
)


; Resolve a scope from the variable environment
(define resolve_env
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((null? (car environment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname))
      (else (let ((resolved_result (resolve_scope (car environment) varname)))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
      )
    )
  )
)


(define extend-scope
  (lambda (list-of-varname list-of-value scope)
    (cond
      ((null? list-of-varname) scope)
      ((null? list-of-value) scope)
      (else (extend-scope (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             scope)))
    )
  )
)


(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env))) 
      (cons new_scope pop_off_env) 
    )
  )
)


(define pop_env_to_global_scope
  (lambda (env)
    (cond
      ((null? env) #false)
      ((equal? (length env) 1)
       (if (equal? (car (car env)) 'global) env
           #false))
      (else (pop_env_to_global_scope (cdr env)))
    )
  )
)


;goal - if var x is already defined in var scope, assign x should
   ;not create a new variable, but update existing one
;case 1 -> scope is empty
;case 2 -> first name/value pair needs to be updated
;case 3 -> middle pair needs to be updated
;case 4 -> no matching pairs can be found

(define update_scope
  (lambda (varname value scope)
    (letrec (
           (check-varname-in-pair (lambda (pair) (equal? (car pair) varname)))
           (check-varname-in-scope
            (lambda (curr_scope)
              (cond
                ((null? curr_scope) #false)
                (else
                 (if (check-varname-in-pair (car curr_scope)) #true
                     (check-varname-in-scope (cdr curr_scope))
                     )
                 )
                )
              )
            )
           )
      (cond
        ((null? scope) (cons (list varname value) scope))
        ((equal? varname (caar scope)) (cons (list varname value) (cdr scope)))
        (else
         (if (check-varname-in-scope scope) 
             (cons (car scope) (update_scope varname value (cdr scope)))
             (cons (list varname value) scope)
             )
         )
        )
      )
    )
  )



(define extend_local_scope
  (lambda (list-of-varname list-of-value env)
    (cond
      ((null? env) #false)
      ((equal? (caar env) 'global) (push_scope_to_env list-of-varname list-of-value env))
      (else (cons (extend-scope list-of-varname list-of-value (car env))
                  (pop_env_to_global_scope env)))
   )
  )
)




