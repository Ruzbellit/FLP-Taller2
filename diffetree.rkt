;;Ruzbellit Rossy Romero Ramirez - 1925456
;;Christian Villanueva Paez - 1924546

#lang eopl


;;******************************************************************************************

;; Gramatica para diff-tree
;;
;; <Diff-tree>    ::= (one)
;:                ::= (diff <Diff-tree> <Diff-tree>)
;******************************************************************************************

(define one
  (lambda ()
    (list 'one))
  )


(define Diff-tree
  (lambda (diff-tree-l diff-tree-r)
    (list 'diff diff-tree-l diff-tree-r)
     )
  )

(define zero
  (lambda () '()
    )
  )

(define iszero?
  (lambda (n)
    (eqv? n 0))
  )

(define successor
  (lambda (diff-tree)
    '()
     )
  )

(define predecessor
  (lambda (n)1))

(define diff-tree-plus
  (lambda (tree1 tree2)
    5
    )
  )