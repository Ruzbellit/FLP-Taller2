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
  (lambda (n)
    (node n (empty-bintree) (empty-bintree))
    )
  )

(define insert-to-left
  (lambda (num tree)
    (node (current-element tree) (number->bintree n) ())))

(define insert-to-right
  (lambda (num tree)))

(define bintree-order-validation
  (lambda (tree)))

(define arbol (node 5
                    (node 4 (empty-bintree) (empty-bintree))
                    (node 5 (empty-bintree) (empty-bintree))
                    )
  )