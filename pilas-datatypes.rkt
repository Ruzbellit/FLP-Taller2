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

;;Creacion del tipo de dato Stack con la funcion datatype
(define-datatype stack stack?
  (empty-stack)
  (stack-element (sym symbol?)
                 (rest stack?)
                 )
  )
;;-----------------------------------------------------------------------------------

;; push : Symbol x Stack -> Stack
;; Proposito:
;; Procedimiento que inserta un elemento en la pila
(define push
  (lambda (n stck)
    (cases stack stck
      (empty-stack () (stack-element n (empty-stack)))
      (stack-element (sym rest) (stack-element n rest)))
    )
  )
;;-----------------------------------------------------------------------------------

;; pop : Stack -> Stack
;; Proposito:
;; Procedimiento que retira el elemento superior de la pila
(define pop
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (stack-element (sym rest) rest)
      )
    )
  )
;;-----------------------------------------------------------------------------------

;; top : Stack -> Symbol
;; Proposito:
;; Procedimiento que retorna el elemento superior de la pila
(define top
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (stack-element (sym rest) sym)
      )
    )
  )
;;-----------------------------------------------------------------------------------

;; empty-stack? : Stack -> Boolean
;; Proposito:
;; Procedimiento que verifica si una pila esta vacía
(define empty-stack?
  (lambda (stck)
    (cases stack stck
      (empty-stack () #t)
      (stack-element (sym rest) #f)
      )
    )
  )

;;-----------------------------------------------------------------------------------