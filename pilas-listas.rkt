#lang eopl
;;Estudiantes:
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co

;; GRAMATICA PARA STACK (pila)
;;
;; <stack>    ::= (<empty-stack>)
;;            ::= (<symbol> <stack>)
;;-----------------------------------------------------------------------------------

;; empty-stack :  -> Stack
;; Proposito:
;; Procedimiento que retorna la representacion vacía de una pila
(define empty-stack
  (lambda () '()
    )
  )
;;-----------------------------------------------------------------------------------

;; push : Symbol x Stack -> Stack
;; Proposito:
;; Procedimiento que inserta un elemento en la pila
(define push
  (lambda (n stck)
    (cons n stck)))
;;-----------------------------------------------------------------------------------

;; pop : Stack -> Stack
;; Proposito:
;; Procedimiento que retira el elemento superior de la pila
(define pop
  (lambda (stck)
    (if (empty-stack? stck)
        (empty-stack)
        (cdr stck)
        )
    )
  )
;;-----------------------------------------------------------------------------------

;; top : Stack -> Symbol
;; Proposito:
;; Procedimiento que retorna el elemento superior de la pila
(define top
  (lambda (stck)
    (if (empty-stack? stck)
        (empty-stack)
        (car stck)
        )
    )
  )
;;-----------------------------------------------------------------------------------

;; empty-stack? : Stack -> Boolean
;; Proposito:
;; Procedimiento que verifica si una pila esta vacía
(define empty-stack?
  (lambda (stck)
    (eqv? (empty-stack) stck)
    )
  )
;;-----------------------------------------------------------------------------------