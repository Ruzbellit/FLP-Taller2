#lang eopl
;;Estudiantes:
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co

;; GRAMATICA
;; Bintree  ::= (empty-bintree)
;;          ::= (node <Number> <Bintree> <Bintree>)
;;-----------------------------------------------------------------------------------


;;
(define empty-bintree
  (lambda () '()
    )
  )
;;-----------------------------------------------------------------------------------

;; current-element : Bintree -> Number
;; Proposito:
;; Procedimiento que retorna el valor del nodo raiz del Bintree
(define current-element
  (lambda (tree)
    (car tree)
    )
  )
;;-----------------------------------------------------------------------------------

;; move-to-left-son : Bintree -> List
;; Proposito:
;; Procedimiento que retorna el nodo izquierdo del Bintree
(define move-to-left-son
  (lambda (tree)
    (cadr tree))
  )
;;-----------------------------------------------------------------------------------

;; move-to-right-son : Bintree -> Bintree
;; Proposito:
;; Procedimiento que retorna el nodo derecha del Bintree
(define move-to-right-son
  (lambda (tree)
    (caddr tree)))
;;-----------------------------------------------------------------------------------

;; number->bintree : Number -> Bintree
;; Proposito:
;; Procedimiento que convierte un numero en una Bintree con nodos hijos vacios
(define number->bintree
  (lambda (number)
    (list number (empty-bintree) (empty-bintree))
    )
  )
;;-----------------------------------------------------------------------------------

;; empty-bintree? : Bintree -> Boolean
;; Proposito:
;; Procedimiento que verifica si un Bintree esta vacio
(define empty-bintree?
  (lambda (tree)
    (eqv? (empty-bintree) tree)
    )
  )
;;-----------------------------------------------------------------------------------

;; at-leaf? : Bintree -> Boolean
;; Proposito:
;; Procedimiento que verifica si el Bintree tiene nodos hijos vacios
(define at-leaf?
  (lambda (tree)
    (and (empty-bintree? (move-to-left-son tree)) (empty-bintree? (move-to-right-son tree)))
    )
  )
;;-----------------------------------------------------------------------------------

;; bintree-with-at-least-one-child? : Bintree -> Boolean
;; Proposito:
;; Procedimiento que verifica si el Bintree tiene almenos un nodo hijo que no esta vacio
(define bintree-with-at-least-one-child?
  (lambda (tree)
    (or (not (empty-bintree? (move-to-left-son tree))) (not (empty-bintree? (move-to-right-son tree))))
    )
  )
;;-----------------------------------------------------------------------------------

;; insert-to-left : Number x Bintree -> Bintree
;; Proposito:
;; Procedimiento que inserta un numero en el nodo izquierdo del Bintree
(define insert-to-left
  (lambda (num tree)
    (list (current-element tree) (number->bintree num) (move-to-right-son tree))
  )
  )
;;-----------------------------------------------------------------------------------

;; insert-to-right : Number x Bintree -> Bintree
;; Proposito:
;; Procedimiento que inserta un numero en el nodo derecho del Bintree
(define insert-to-right
  (lambda (num tree)
    (list (current-element tree) (move-to-left-son tree) (number->bintree num))
    )
  )
;;-----------------------------------------------------------------------------------

;; bintree-order-validation : Bintree -> Boolean
;; Proposito:
;; Procedimiento que verifica si el Bintree cumple con la propiedad de orden
;; (lado izquierdo los menores, lado derecho los mayores)
(define bintree-order-validation
  (lambda (tree)
    (if(empty-bintree? tree)
       #t
       (and (if (empty-bintree? (move-to-left-son tree))
                #t
                (and (> (current-element tree) (current-element (move-to-left-son tree)))
                     (bintree-order-validation (move-to-left-son tree))))
            (if (empty-bintree? (move-to-right-son tree))
                #t
                (and (and (< (current-element tree) (current-element (move-to-right-son tree)))
                          (bintree-order-validation (move-to-right-son tree))))
                )
            )
       )
    )
  )
;;-----------------------------------------------------------------------------------

;; Auxiliar
;; search-element-in-bintree : Number x Bintree -> Boolean
;; Proposito:
;; Procedimiento que verifica si un numero se encuentra en el Bintree
(define search-element-in-bintree
  (lambda (num tree)
    (if (empty-bintree? tree)
        #f
        (if (eqv? num (current-element tree))
            #t
            (or (search-element-in-bintree num (move-to-left-son tree))
                (search-element-in-bintree num (move-to-right-son tree)))
            )
        )
    )
  )

;; insert-element-into-bintree : Bintree x Number -> Bintree
;; Proposito:
;; Procedimiento que inserta un numero en el Bintree, teniendo en cuenta la propiedad de orden
;; (lado izquierdo los menores, lado derecho los mayores)
(define insert-element-into-bintree
  (lambda (tree num)
    (if (search-element-in-bintree num tree)
        tree
        (list (current-element tree)
              (if (empty-bintree? (move-to-left-son tree))
                  (if (< num (current-element tree))
                      (number->bintree num)
                      (move-to-left-son tree)
                      )
                  (if(< num (current-element tree))
                     (insert-element-into-bintree (move-to-left-son tree) num)
                     (move-to-left-son tree)
                     )
                  )
              (if (empty-bintree? (move-to-right-son tree))
                  (if (> num (current-element tree))
                      (number->bintree num)
                      (move-to-right-son tree)
                      )
                  (if(> num (current-element tree))
                     (insert-element-into-bintree (move-to-right-son tree) num)
                     (move-to-right-son tree)
                     )
                  )
            )
        )
    )
  )
;;-----------------------------------------------------------------------------------

;;Pruebas

;;empty-bintree:
(empty-bintree)
;;()

;;current-element:
(current-element '(7 () ()))
;;7

;;move-to-left-son:
(move-to-left-son '(12 (1 () ()) (31 () ())))
;;(1 () ())

;;move-to-right-son:
(move-to-right-son '(12 (1 () ()) (31 () ())))
;;(31 () ())

;;number→bintree:
(number->bintree 93)
;;(93 () ())

;;empty-bintree?:
(empty-bintree? (move-to-left-son '(13 () ())))
;;#t

;;at-leaf ?:
(at-leaf? '(7 () ()))
;;#t

;;bintree-with-at-least-one-child?:
(bintree-with-at-least-one-child? '(18 () (38 () ())))
;;#t

;;insert-to-left:
(insert-to-left 9 '(18 () ()))
;;(18 (9 () ()) ())

;;insert-to-right:
(insert-to-right 27 (insert-to-left 9 '(18 () ())))
;;(18 (9 () ()) (27 () ()))

(define Arbol_Ejemplo
'(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))

;;bintree-order-validation:
(bintree-order-validation Arbol_Ejemplo)
;;#t

;;insert-element-into-bintree:
(insert-element-into-bintree Arbol_Ejemplo 2)
;;(8 (3 (1 () (2 () ())) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))
