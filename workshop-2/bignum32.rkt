#lang eopl
(require rackunit)

; Ejercicio 1.

; zero: -> empty
; Procedimiento constructor de la representacion del cero, en este caso
; una lista vacia.

(define zero
  (lambda ()
    empty
    )
  )

; is-zero? list -> boolean
; Procedimiento que verifica si una lista ingresada es la representacion del cero en Bignum.

(define is-zero?
  (lambda (n)
    (null? n)
    )
  )

; successor-N: list, number -> list
; Procedimiento que retorna la representacion Bignum del sucesor de un numero en base N.

(define successor-N
  (lambda (n N)
    (cond
      [(is-zero? n) (cons 1 (zero))]
      [(is-zero? (car n)) (cons 1 (cdr n))]
      [(equal? (- N 1) (car n)) (cons (zero) (successor-N (cdr n) N))]
      [else (cons (+ 1 (car n)) (cdr n))]
      )
    )
  )

; successor: list -> list
; Procedimiento que retorna la representacion Bignum del sucesor de un numero en base 32.

(define successor
  (lambda (n)
    (successor-N n 32)
    )
  )

; predecessor-N: list, number -> list
; Procedimiento que retorna la representacion Bignum del predecesor de un numero en base N.

(define predecessor-N
  (lambda (n N)
    (cond
      [(is-zero? (car n)) (cons (- N 1) (predecessor-N (cdr n) N))]
      [(equal? '(1) n) (zero)]
      [(equal? 1 (car n)) (cons (zero) (cdr n) )]
      [else (cons (- (car n) 1) (cdr n))]
      )
    )
  )

; predecessor: list -> list
; Procedimiento que retorna la representacion Bignum del predecesor de un numero en base 32.

(define predecessor
  (lambda (n)
    (predecessor-N n 32)
    )
  )

; to-bignum: number, number -> list
; Procedimiento que recibe un numero n y una base N y retorna la representacion Bignum de n en base N.

(define to-bignum
  (lambda (n N)
    (cond
      [(null? n) (zero)]
      [(zero? n) empty]
      [else (successor-N (to-bignum (- n 1) N) N)]
      )
    )
  )

; to-number: list, number -> number
; Procedimiento que recibe una lista de la representacion Bignum de un numero en base N y
; retorna el valor en base 10 de n.

(define to-number
  (lambda (n N)
    (cond
      [(is-zero? n) 0]
      [(is-zero? (car n)) (*  (to-number (cdr n) N) N)]
      [else (+ (car n) (* (to-number (cdr n) N) N))]
      )
    )
  )


; ========================= Implementacion ============================================


; suma-N: list, list, number -> list
; Procedimiento que realiza la suma de dos valores x e y representados en Bignum base N.

(define suma-N
  (lambda (x y N)
    (if (is-zero? x) y
        (successor-N (suma-N (predecessor-N x N) y N) N)
        )
    )
  )
; suma: list, list -> list
; Procedimiento que realiza la suma de dos valores x e y representados en Bignum base 32.

(define suma
  (lambda (x y)
    (suma-N x y 32)
    )
  )
; Pruebas:

(suma '(1) '(2)) ; 1 + 2 = 3 -> '(3).
(suma '(1 1) '(3)) ; 33 + 3 = 36 -> '(4 1).
(suma '(4 1) '(8 2)) ; 36 + 72 = 108 -> '(12 3).
(suma '(4 5) '(() 1)) ; 164 + 32 = 196 -> '(4 6)


(check-equal? (suma '(1) '(2)) '(3))
(check-equal? (suma '(1 1) '(3)) '(4 1))
(check-equal? (suma '(4 1) '(8 2)) '(12 3))
(check-equal? (suma '(4 5) '(() 1)) '(4 6))


; resta-N: list, list, number -> list
; Procedimiento que realiza la resta de dos valores x e y representados en Bignum base N.

(define resta-N
  (lambda (x y N)
    (cond
      [(is-zero? x) y]
      [(is-zero? y) x]
      [else (resta-N (predecessor-N x N) (predecessor-N y N) N)]
      )
    )
  )


; resta: list, list -> list
; Procedimiento que realiza la resta de dos valores x e y representados en Bignum base 32.

(define resta
  (lambda (x y)
    (resta-N x y 32)
    )
  )

; Pruebas:

(resta '(2) '(1)) ; 2 - 1 = 1 -> '(1)
(resta '(1 1) '(1)) ; 33 - 1 = 32 ->'(() 1)
(resta '(8 1) '(() 1)) ; 40 - 32 = 8 -> '(8)

(check-equal? (resta '(2) '(1)) '(1))
(check-equal? (resta '(1 1) '(1)) '(() 1))
(check-equal? (resta '(8 1) '(() 1)) '(8))


; multiplicacion-N: list, list, number -> list
; Procedimiento que realiza la multiplicacion de dos valores x e y representados en Bignum base N.

(define multiplicacion-N
  (lambda (x y N)
    (cond
      [(or (is-zero? x) (is-zero? y)) (zero)]
      [(suma-N (multiplicacion-N (predecessor-N x N) y N) y N)]
      )
    )
  )

; multiplicacion: list, list -> list
; Procedimiento que realiza la multiplicacion de dos valores x e y representados en Bignum base 32.

(define multiplicacion
  (lambda (x y)
    (multiplicacion-N x y 32)
    )
  )

; Pruebas:

(multiplicacion '() '(2)) ; 0*2 = 0 -> '()
(multiplicacion '(2) '(2)) ; 2*2 = 4 -> '(4)
(multiplicacion '(() 1) '(2)) ; 32*2 = 64 -> '(() 2)
(multiplicacion '(2 1) '(1)) ; 34*1 = 34 -> '(2 1)

(check-equal? (multiplicacion '() '(2)) '())
(check-equal? (multiplicacion '(2) '(2)) '(4))
(check-equal? (multiplicacion '(() 1) '(2)) '(() 2))
(check-equal? (multiplicacion '(2 1) '(1)) '(2 1))



; division-N: list, list, number -> list
; Procedimiento que realiza la division entre x e y representados en Bignum base N.

(define division-N
  (lambda (x y N)
    (cond
      [(is-zero? y) (eopl:error "divison entre cero no definida")]
      [(is-zero? x) x]
      [(> (to-number y N) (to-number x N)) (eopl:error "no es posible en los enteros")]
      [(equal? x y) (successor-N (zero) N)]
      [else (suma-N (successor-N (zero) N) (division-N (resta-N x y N) y N) N)]
      )
    )
  )

; division: list, list -> list
; Procedimiento que realiza la division de entre valores x e y representados en Bignum base 32.

(define division
  (lambda (x y)
    (division-N x y 32)
    )
  )

; Pruebas:

(division '(4) '(2)) ; 4/2 = 2 -> '(2)
(division '(() 1) '(8)) ; 32/8 = 4 ->'(4)
(division '() '(2 1)) ; 0/34 = 0 -> '()
(division '(() 2) '(2)) ; 64/2 = 32 -> '(() 1)
(division '(1 2) '(5)) ; 65/5 = 13 -> '(13)

(check-equal? (division '(4) '(2)) '(2))
(check-equal? (division '(() 1) '(8)) '(4))
(check-equal? (division '() '(2 1)) '())
(check-equal? (division '(() 2) '(2)) '(() 1))
(check-equal? (division '(1 2) '(5)) '(13))


; potencia-N: list, list, number -> list
; Procedimiento que multiplica exp veces el numero n ambos en representacion Bignum base N.

(define potencia-N
  (lambda (n exp N)
    (cond
      [(and (is-zero? n) (is-zero? exp)) (eopl:error "no esta definido")]
      [(is-zero? exp) (successor-N (zero) N)]
      [else (multiplicacion-N n (potencia-N n (predecessor-N exp N) N) N)]
      )
    )
  )

; potencia: list, list -> list
; Procedimiento que multiplica exp veces el numero n ambos en representacion Bignum base 32.

(define potencia
  (lambda (n exp)
    (potencia-N n exp 32)
    )
  )

; Pruebas:

(potencia '(2) '(2)) ; 2^2 = 4 -> '(4)
(potencia '(3) '(4)) ; 3^4 = 81 -> '(17 2)
(potencia '(() 1) '(2)) ; 32^2 = 1024 -> '(() () 1)

(check-equal? (potencia '(2) '(2)) '(4))
(check-equal? (potencia '(3) '(4)) '(17 2))
(check-equal? (potencia '(() 1) '(2)) '(() () 1))


; factorial-N: list, number -> list
; Procedimiento que retorna el factorial del numero n en representacion Bignum base N.

(define factorial-N
  (lambda (n N)
    (cond
      [(is-zero? n) (successor-N (zero) N)]
      [else (multiplicacion-N n (factorial-N (predecessor-N n N) N) N)]
      )
    )
  )

; factorial: list, number -> list
; Procedimiento que retorna el factorial del numero n en representacion Bignum base 32.

(define factorial
  (lambda (n)
    (factorial-N n 32)
    )
  )

; Pruebas:

(factorial '()) ; 0! = 1 -> '(1)
(factorial '(3)) ; 3! = 6 -> '(6)
(factorial '(8)) ; 8! = 40320 -> '(() 12 7 1)

(check-equal? (factorial '()) '(1))
(check-equal? (factorial '(3)) '(6))
(check-equal? (factorial '(8)) '(() 12 7 1))
