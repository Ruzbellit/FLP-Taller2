#lang eopl
(define-datatype bintree bintree?
  (empty-bintree)
  (node (int number?)
        (left bintree?)
        (right bintree?)
        )
  )

(define current-element
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () empty-bintree)
      (node (int left right) int)
      )
    )
  )

(define move-to-left-son
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () empty-bintree)
      (node (int left right) left)
      )
    )
  )

(define move-to-right-son
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () empty-bintree)
      (node (int left right) right)
      )
    )
  )

(define number->bintree
  (lambda (num)
    (node num (empty-bintree) (empty-bintree))
    )
  )

(define empty-bintree?
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () #t)
      (else #f))
    )
  )

(define at-leaf?
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () #f)
      (node (int left right) (and (empty-bintree? left) (empty-bintree? right)))
      )
    )
  )

(define bintree-with-at-least-one-child
(lambda (tree)
    (cases bintree tree
      (empty-bintree () #f)
      (node (int left right) (or (empty-bintree? left) (empty-bintree? right)))
      )
    )
  )

(define insert-to-left
  (lambda (num tree)
    (node (current-element tree) (number->bintree num) (empty-bintree))
    )
  )

(define insert-to-right
  (lambda (num tree)
    (node (current-element tree) (empty-bintree) (number->bintree num))
    )
  )

(define arbol (node 5
                    (node 4 (empty-bintree) (empty-bintree))
                    (node 5 (empty-bintree) (empty-bintree))
                    )
  )