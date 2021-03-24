#lang eopl

(define suma
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (suma (predecessor x) y)))))

(define resta
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (resta  x (predecessor y))))))

(define multiplicacion
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (suma (multiplicacion (predecessor x) y) y))
    ))
    
(define potencia
  (lambda (x y)
    (if (is-zero? y)
        (successor y)
        (multiplicacion (potencia x (predecessor y)) x))))

(define factorial
  (lambda (n)
    (if (is-zero? n)
        (successor n)
        (multiplicacion n (factorial (predecessor n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

(define zero (lambda () '(0)))

(define is-zero? (lambda (n) (equal? n '(0))))

(define successor (lambda (n)
                    (if (null? n)
                        '(1)
                        (if (< (car n) 31)
                            (cons (+ (car n) 1) (cdr n))
                            (cons 0 (successor (cdr n)))))))

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
;; '(1)
(predecessor '(31))
;; '(0 1)
(predecessor '(0 1))
;; '(31)
(predecessor '(0 2))
;;  '(31 1)
(predecessor '(0 0 1))
;; '(31 31)


