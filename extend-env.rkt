#lang eopl

;; Gramática
;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;; Var = Sym

;empty-env : () → Env
(define empty-env
  (lambda () (list 'empty-env)))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))


;; Gramática extendida (con extend-env*)
;; Env := (empty-env)
;;     := (extend-env Var SchemeVal Env)
;;     := (extend-env* (Var SchemeVal)* Env)
;; Var := Symbol

;extend-env*
;extiende cada variable asociada con cada valor en el ambiente dado.
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (list 'extend-env*
              (extend-env*-aux vars vals)
              env))))

;extend-env*-aux; list x lista -> List of Pairs
; funcion auxiliar de extend-env* que asocia una lista de variables con sus valores
(define extend-env*-aux
  (lambda (vars vals)
    (if (null? vars)
        '()
        (cons (cons (car vars) (car vals)) (extend-env*-aux (cdr vars) (cdr vals))))))
;Pruebas
(extend-env*-aux '(x y z) '(1 2 3))
;((x . 1) (y . 2) (z . 3))

; Ambientes de prueba
(define env1 (extend-env 'r 17 (empty-env)))
(define env2 (extend-env 'b 27 env1))
(define env3 (extend-env* '(x y z) '(7 8 9) env1))
(define e
  (extend-env 'y 8
              (extend-env* '(x z w) '(1 4 5)
                           (extend-env 'a 7
                                       (empty-env)))))

;depth-env: env -> Int
;Retorna la profundidad de un ambiente
(define depth-env
  (lambda (env)
    (cond
     [(eqv? (car env) 'empty-env) 0]
     [(eqv? (car env) 'extend-env) (+ 1 (depth-env (cadddr env)))]
     [(eqv? (car env) 'extend-env*) (+ 1 (depth-env (caddr env)))])))
;Pruebas
(depth-env e)
; 3

;get-var-values-env: env -> List
;Retorna las variables con sus valores correspondientes de un ambiente dado
(define get-var-values-env
  (lambda (env)
    (cond
      [(eqv? (car env) 'empty-env) '()]
      [(eqv? (car env) 'extend-env) (list (cons (cadr env) (caddr env)))]
      [(eqv? (car env) 'extend-env*) (list (cadr env))])))

(define check-env
  (lambda (e n)
        (if (eqv? (- (depth-env e) n) 0)
            (get-var-values-env e)
            (cond
              [(eqv? (car e) 'empty-env) '()]
              [(eqv? (car e) 'extend-env) (check-env (cadddr e) n)]
              [(eqv? (car e) 'extend-env*) (check-env (caddr e) n)]))))


