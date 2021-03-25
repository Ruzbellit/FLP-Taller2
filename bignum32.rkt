#lang eopl
;;Estudiantes:
;;Ruzbellit Rossy Romero Ramirez (1925456)
;;ruzbellit.romero@correounivalle.edu.co
;;Christian Villanueva Paez (1924546)
;;christian.villanueva@correounivalle.edu.co


;; suma : BigNum32 BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el resultado de una suma 
(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))
;; Pruebas
;(suma '(0 1) '(1))   //'(1 1)
;(suma '(31) '(2))    //'(1 1)
;(suma '(31) '(1))    //'(0 1)
;(suma '(31) '(31))   //'(30 1)
;------------------------------------------------------------------------------

;; resta : BigNum32 BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el resultado de una resta 
(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))
;; Pruebas
;(resta '(0 2) '(1))   //'(31 1)
;(resta '(0 1) '(1))   //'(31)
;(resta '(1 1) '(0))   //'(1 1)
;------------------------------------------------------------------------------

;; multiplicacion : BigNum32 BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el resultado de una multiplicacion 
(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))))
;; Pruebas
;(multiplicacion '(31) '(2))     // '(30 1)  
;(multiplicacion '(0 1) '(1))    // '(0 1)
;(multiplicacion '(1 2) '(0))    // '(0)
;(multiplicacion '(31) '(31))    // '(1 30)
;------------------------------------------------------------------------------

;; potencia : BigNum32 BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el resultado de una potencia
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))
;; Pruebas
;(potencia '(31) '(2))     // '(1 30)  
;(potencia '(1 2) '(1))    // '(1 2)
;(potencia '(31 1) '(0))   // '(1)
;------------------------------------------------------------------------------

;; factorial : BigNum32 BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el resultado de una factorial
(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))
;; Pruebas
;(factorial '(2))     // '(2)  
;(factorial '(0))     // '(1)
;(factorial '(5))     // '(24 3) 
;------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;;Representacion Bignum con base 32
(define zero (lambda () '(0)))

(define is-zero? (lambda (n) (equal? n '(0))))

;; successor : BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el sucesor de un numero
(define successor (lambda (n)
                    (if (null? n)
                        '(1)
                        (if (< (car n) 31)
                            (cons (+ (car n) 1) (cdr n))
                            (cons 0 (successor (cdr n)))))))

;; predecessor : BigNum32 -> BigNum32
;; Proposito:
;; Procedimiento que retorna el predecessor de un numero 
(define predecessor (lambda (n)
                    (cond
                      [(null? n) '(0)]
                      [(is-zero? n) '()]
                      [(equal? n '(0 1)) '(31)]
                      [(> (car n) 0) (cons (- (car n) 1) (cdr n))]
                      [(eqv? (car n) 0) (cons 31 (predecessor (cdr n)))])))

;; Ejemplos de prueba successor
(successor '(0))
;; '(1)
(successor '(31))
;; '(0 1)
(successor '(0 1))
;; '(1 1)
(successor '(30 1))
;; '(31 1)
(successor '(31 1))
;;  '(0 2)
(successor '(31 31))
;; '(0 0 1)
;; Ejemplos de prueba predecessor
(predecessor '(1))
;; '(0)
(predecessor '(31))
;; '(30)
(predecessor '(0 1))
;; '(31)
(predecessor '(0 2))
;;  '(31 1)
(predecessor '(0 0 1))
;; '(31 31)


