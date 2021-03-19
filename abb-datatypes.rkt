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
      (node (int left rigt) int)
      )
    )
  )

(define arbol (node 5
                    (node 4 (empty-bintree) (empty-bintree))
                    (node 5 (empty-bintree) (empty-bintree))
                    )
  )