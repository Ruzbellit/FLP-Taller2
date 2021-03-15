#lang eopl
(define-datatype stack stack?
  (empty-stack)
  (non-empty-stack (sym symbol?)))

(define push
  (lambda (n stck)
    (cases stack stck
      (empty-stack () '(n))
      (non-empty-stack (sym) (cons n stck)))
    )
  )

(define pop
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (non-empty-stack (sym) (cdr stck))
      )
    )
  )

(define top
  (lambda (stck)
    (cases stack stck
      (empty-stack () '())
      (non-empty-stack (sym symbol?) (car stck))
      )
    )
  )