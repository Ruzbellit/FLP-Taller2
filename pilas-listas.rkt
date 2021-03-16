#lang eopl
(define empty-stack '())

(define push
  (lambda (n stack)
    (cons n stack)))

(define pop
  (lambda (stack)
    (if (empty-stack? stack)
        '()
        (cdr stack)
        )
    )
  )

(define top
  (lambda (stack)
    (if (empty-stack? stack)
        '()
        (car stack)
        )
    )
  )

(define empty-stack?
  (lambda (stack)
    (eqv? empty-stack stack)
    )
  )

(define a '(1 2 3 4))