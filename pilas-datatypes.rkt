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

(define-datatype stack stack?
  (empty-stack)
  (stack-element (sym symbol?)
                 (rest stack?)
                 )
  )

(define push
  (lambda (n stck)
    (cases stack stck
      (empty-stack () '(n))
      (stack-element (sym rest) (stack-element n stck)))
    )
  )

(define pop
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (stack-element (sym rest) rest)
      )
    )
  )

(define top
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (stack-element (sym rest) sym)
      )
    )
  )

(define my-stack
  (stack-element 'x
                 (stack-element 'y
                                (empty-stack)
                                )
                 )
  )