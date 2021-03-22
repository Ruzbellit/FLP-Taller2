#lang eopl
(define empty-bintree
  (lambda () '()
    )
  )

(define current-element
  (lambda (tree)
    (car tree)
    )
  )

(define move-to-left-son
  (lambda (tree)
    (cadr tree))
  )

(define move-to-right-son
  (lambda (tree)
    (caddr tree)))

(define number->bintree
  (lambda (number)
    '(number empty-bintree emptybintree)
    )
  )


(define empty-bintree?
  (lambda (tree)
    (eqv? empty-bintree tree)
    )
  )

(define at-leaf?
  (lambda (tree)
    (and (empty-bintree? (move-to-left-son)) (empty-bintree? (move-to-right-son)))
    )
  )

(define bintree-with-at-least-one-child?
  (lambda (tree)
    (or (not (empty-bintree? (move-to-left-son))) (not (empty-bintree? (move-to-right-son))))
    )
  )

(define insert-to-left
  (lambda (num tree)
    (cond
      ))
  )

(define insert-to-right
  (lambda (num tree)
    (cond
      ))
  )

(define bintree-order-validation
  (lambda (tree)1
    )
  )

(define insert-element-into-bintree
  (lambda (tree num)1
    )
  )