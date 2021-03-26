#lang eopl
;;Estudiantes:
;
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co
 
;******************************************************************************************
;; Gramática para first language:
;;
;;  <programa>      ::= <expresion>
;;                      <un-program (exp)>
;;  <expresion>     ::= <numero>
;;                      <num-lit (n)>
;;                  ::= (<expresion> <operacion> <expresion>)
;;                      <exp-lit (exp1 op exp2)>
;;                  ::= <identificador>
;;                      <variable (id)>
;;                  ::= var (identificador = <expresion>)* in <expresion>
;;                      <declaracion (ids exps cuerpo)>
;;  <operacion>     ::= + | - | * | /
;;                      primitiva
;******************************************************************************************

;******************************************************************************************

;Especificación Léxica
(define lexica
  '((espacio-en-blanco
     (whitespace) skip)
    (comentario
     ("%" (arbno (not #\newline))) skip)
    (identificador
     (letter (arbno (or letter digit "?"))) symbol)
    (numero
     (digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)
(define gramatica
  '((programa (expresion) un-program)
    (expresion (numero) num-lit)
    (expresion ("(" expresion operacion expresion ")") exp-lit)
    (expresion (identificador) variable)
    (expresion
     ("var" (arbno "(" identificador "=" expresion ")") "in" expresion)
     declaracion)
    (operacion ("+") suma-op)
    (operacion ("-") resta-op)
    (operacion ("*") mult-op)
    (operacion ("/") div-op)))

;Tipos de datos para la sintaxis abstracta de la gramática
;Construidos manualmente:
(define-datatype programa programa?
  (un-program
   (exp expresion?)))

(define-datatype expresion expresion?
  (num-lit
   (n number?))
  (exp-lit
   (exp1 expresion?)
   (op operacion?)
   (exp2 expresion?))
  (variable
   (id symbol?))
  (declaracion
   (ids (list-of symbol?))
   (exps (list-of expresion?))
   (cuerpo expresion?)))

(define-datatype operacion operacion?
  (suma-op)
  (resta-op)
  (mult-op)
  (div-op))

;Datatypes Construidos automáticamente:
;(sllgen:make-define-datatypes lexica gramatica)
;
;(define show-the-datatypes
;  (lambda () (sllgen:list-define-datatypes lexica gramatica)))


;******************************************************************************************
;Implementacion de la función unparse:

;; unparse : List -> List
;; Proposito:
;; Es la funcion principal unparse, hace unparse de una expresion valida en la gramatica
(define unparse
  (lambda (exp)
    (cond
      [(programa? exp)
       (cases programa exp
         (un-program (exp) (unparse exp)))]
      [(expresion? exp)
       (cases expresion exp
         (num-lit (n) n)
         (exp-lit (exp1 op exp2) (list (unparse exp1) (unparse op) (unparse exp2)))
         (variable (id) id)
         (declaracion
          (ids exps cuerpo)
          (append '(var) (unparse-declaraciones ids exps) '(in) (list (unparse cuerpo)))))]
      [(operacion? exp)
       (cases operacion exp
         (suma-op () '+)
         (resta-op () '-)
         (mult-op () '*)
         (div-op () '/))])))

;; unparse-declaraciones : List x List -> List
;; Proposito:
;; Esta es una funcion auxiliar de unparse, ayuda a hacer el unparse de una 'declaracion',
;; relaciona cada id con la expresion correspondiente
(define unparse-declaraciones
  (lambda (ids exp)
    (if (null? ids)
        '()
        (cons
         (list (car ids) '= (unparse (car exp)))
         (unparse-declaraciones (cdr ids) (cdr exp))))))

;Pruebas
(unparse-declaraciones '(a b c) (list (num-lit 9) (num-lit 11) (num-lit 17)))
; ((a = 9) (b = 11) (c = 17))
(unparse-declaraciones '(x y) (list (exp-lit (num-lit 3) (suma-op) (num-lit 2)) (num-lit 6) ))
;((x = (3 + 2)) (y = 6))

;Funcion scan&parse generada por la librería sllgen
( define scan&parse
   ( sllgen:make-string-parser
     lexica
     gramatica ) )

;; Ejemplos de expresiones en sintaxis abstracta
;exp1 es un num-lit
(define exp1 (num-lit 27))
;exp2 es un exp-lit
(define exp2 (exp-lit (num-lit 27) (resta-op) (num-lit 10)))
;exp3 es una variable
(define exp3 (variable 'R))
;exp4 es una declaracion
(define exp4 (declaracion
              '(R U Z)
              (list (num-lit 1) (num-lit 2) (num-lit 3))
              (exp-lit (variable 'R) (suma-op) (variable 'Z))))
;exp5 es un programa
(define exp5
  (un-program
   (declaracion
    '(R U Z)
    (list (num-lit 1) (num-lit 2) (num-lit 3))
    (exp-lit (variable 'R) (suma-op) (variable 'Z)))))

;Pruebas unparse
(unparse exp1)
(unparse exp2)
(unparse exp3)
(unparse exp4)
(unparse exp5)

;Pruebas parse
(scan&parse "var (R = 1) (U = 2) (Z = 3) in (R + Z)")
