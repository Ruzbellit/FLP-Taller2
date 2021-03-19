#lang eopl
(define diff
  (lambda (tree)
    (if (eqv? (car tree) 'one)
        1
        (- (cadr tree) (caddr tree))
        )
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
  (lambda (n)1)
  )

(define predecessor
  (lambda (n)1))

(define diff-tree-plus
  (lambda (tree1 tree2)
    
    )
  )