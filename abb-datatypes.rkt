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

(define bintree-with-at-least-one-child?
(lambda (tree)
    (cases bintree tree
      (empty-bintree () #f)
      (node (int left right) (or (not (empty-bintree? left)) (not (empty-bintree? right))))
      )
    )
  )

(define insert-to-left
  (lambda (num tree)
    (node (current-element tree) (number->bintree num) (move-to-right-son tree))
    )
  )

(define insert-to-right
  (lambda (num tree)
    (node (current-element tree) (move-to-left-son tree) (number->bintree num))
    )
  )

(define bintree-order-validation
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () #t)
      (node (int left right) (and (if (at-leaf? left)
                                     (and (> (current-element tree) (current-element (move-to-left-son tree)))
                                          (bintree-order-validation left)
                                          )
                                     #t
                                     )
                                 (if (at-leaf? right)
                                     (and (< (current-element tree) (current-element (move-to-right-son tree)))
                                          (bintree-order-validation right)
                                          )
                                     #t)
                                 )
            )
      )
    )
  )



(define insert-element-into-bintree
  (lambda (num tree)
    (cases bintree tree
      (empty-bintree () tree)
      (node (int lef right) 2)
      )
    )
  )

(define parse
  (lambda (list-bintree)
    (cond
      [(null? list-bintree) (empty-bintree)]
      [else (node (car list-bintree) (parse (cadr list-bintree)) (parse (caddr list-bintree)))]
      )
    )
  )

(define unparse
  (lambda (tree)
    (cases bintree tree
      (empty-bintree () '())
      (node (int left right) (cons int (cons (unparse left) (cons (unparse right) '()))))
      )
    )
  )

(define lista
    '(5 (3 (1 () ()) (4 () ())) ())
  )

(define arbol (node 5
                    (node 2
                          (node 5
                                (empty-bintree)
                                (empty-bintree))
                          (empty-bintree))
                    (node 6 (empty-bintree) (empty-bintree))
                    )
  )

;Pruebas
(empty-bintree)
;()

(current-element (parse '(7 () ())))
;7

(move-to-left-son (parse'(12 (1 () ()) (31 () ()))))
;(1 () ())

(move-to-right-son (parse '(12 (1 () ()) (31 () ()))))
;(31 () ())

(number->bintree 93)
;(93 () ())

(empty-bintree? (move-to-left-son (parse '(13 () ()))))
;#t

(at-leaf? (parse '(7 () ())))
;#t

(bintree-with-at-least-one-child? (parse '(18 () (38 () ()))))
;#t

(insert-to-left 9 (parse '(18 () ())))
;(18 (9 () ()) ())

(insert-to-right 27 (insert-to-left 9 (parse '(18 () ()))))
;(18 (9 () ()) (27 () ()))

(define Arbol_Ejemplo
  (parse '(8 (3 (1 () ())
                (6 (4 () ())
                   (7 () ())))
             (10 ()
                 (14 (13 () ())
                     ())))
         )
  )
(bintree-order-validation Arbol_Ejemplo)
;#t

;(insert-element-into-bintree Arbol_Ejemplo 2)