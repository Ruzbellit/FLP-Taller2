#lang eopl
;;Estudiantes:
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co

;;GRAMATICA PARA PREFIX-LIST
;;Prefix-list ::= (Prefix-exp)
;;                 pref-exp (pref)
;;Prefix-exp ::= Int
;;               const-exp (num)
;;           ::= - Prefix-exp Prefix-exp
;;               diff-exp (operand1 operand2)
;;-----------------------------------------------------------------------------------

;;Creacion del datatype
(define-datatype prefix-exp prefix-exp?
  (const-exp (num integer?))
  (diff-exp (operand1 prefix-exp?)
            (operand2 prefix-exp?)))

(define-datatype prefix-list prefix-list?
  (pref-exp (pref prefix-exp?)))

;; parse : List -> Estructura sintaxis abstracta
;; Proposito:
;; Procedimiento que convierte de sintaxis concreta a sintaxis abstracta
(define parse
  (lambda (dato)
    (cond
      [(integer? (car dato))
        (cons (const-exp (car dato)) (cdr dato))]
      [(eqv? (car dato) '-)
       (cons
        (diff-exp
         (car (parse (cdr dato)))
         (car (parse (cdr (parse (cdr dato))))))
        (cdr (parse (cdr (parse (cdr dato))))))])))

;; unparse : Estructura sintaxis abstracta -> List
;; Proposito:
;; Procedimiento que convierte de sintaxis abstracta a sintaxis concreta
(define unparse
  (lambda (exp)
    (cases prefix-exp exp
      (const-exp (num) num)
      (diff-exp (operand1 operand2)
                  (list '- (unparse operand1) (unparse operand2))))
      ))


;;Pruebas:
;(parse '(- - 3 2 - 4 - 12 7 ))
;(unparse (diff-exp
; (diff-exp
; (const-exp 3)
; (const-exp 2))
; (diff-exp
; (const-exp 4)
; (diff-exp
; (const-exp 12)
; (const-exp 7)))))
