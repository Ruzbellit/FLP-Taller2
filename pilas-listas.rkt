;;Ruzbellit Rossy Romero Ramirez - 1925456
;;Christian Villanueva Paez - 1924546

#lang eopl

;;******************************************************************************************
;;
;; Gramatica para stack
;;
;; <stack>    ::= (<empty-stack>)
;;            ::= (<symbol> <stack>)
;******************************************************************************************

(define empty-stack
  (lambda ()
    (list 'empty-stack)
    )
  )

(define stack
  (lambda (sym stack)
    (list 'stack sym stack)))

(define push
  (lambda (n stck)
    (stack n stck)))

(define pop
  (lambda (stck)
    (if (empty-stack? stck)
        (empty-stack)
        (caddr stck)
        )
    )
  )

(define top
  (lambda (stck)
    (if (empty-stack? stck)
        (empty-stack)
        (cadr stck)
        )
    )
  )

(define empty-stack?
  (lambda (stck)
    (eqv? (car (empty-stack)) (car stck))
    )
  )