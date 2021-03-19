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