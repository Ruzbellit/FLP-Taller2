#lang eopl
;;Estudiantes:
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co

;; GRAMATICA PARA DIFF-TREE
;;
;; <Diff-tree>    ::= (one)
;:                ::= (diff <Diff-tree> <Diff-tree>)
;;-----------------------------------------------------------------------------------

;; Diff-tree->Number : Diff-tree -> Number
;; Proposito:
;; Procedimiento que transforma una representacion de Diff-tree en un numero
(define Diff-tree->Number
  (lambda (diff-tree)
    (if (eqv? (car diff-tree) 'one)
        1
        (- (Diff-tree->Number (cadr diff-tree))
           (Diff-tree->Number (caddr diff-tree))))))
;;-----------------------------------------------------------------------------------

;; zero :  -> Diff-tree
;; Proposito:
;;Procedimiento que retorna una representacion del numero 0 para Diff-tree
(define zero
  (lambda ()
    '(diff (one) (one))
    )
  )
;;-----------------------------------------------------------------------------------

;; is-zero? : Diff-tree -> Boolean
;; Proposito:
;; Procedimiento que verifica si una representacion del Diff-tree
;; es igual al numero 0
(define is-zero?
  (lambda (diff-tree)
    (eqv? (Diff-tree->Number (zero))
          (Diff-tree->Number diff-tree)
          )
    )
  )
;;-----------------------------------------------------------------------------------

;; successor : Diff-tree -> Diff-tree
;; Proposito:
;; Procedimiento que retorna el numero posterior de la representacion de Diff-tree
(define successor
  (lambda (diff-tree)
    (list 'diff diff-tree '(diff (diff (one) (one)) (one)))
     )
  )
;;-----------------------------------------------------------------------------------

;; predecessor : Diff-tree -> Diff-tree
;; Proposito:
;; Procedimiento que retorna el numero anterior de la representacion de Diff-tree
(define predecessor
  (lambda (diff-tree)
    (list 'diff diff-tree '(one)))
  )
;;-----------------------------------------------------------------------------------

;; diff-tree-plus : Diff-tree x Diff-tree -> Diff-tree
;; Proposito:
;; Procedimiento que suma las representaciones de Diff-tree
(define diff-tree-plus
  (lambda (dif-tree1 dif-tree2)
     (list 'diff dif-tree1 (list 'diff '(diff (one) (one)) dif-tree2))
    )
  )
;;-----------------------------------------------------------------------------------