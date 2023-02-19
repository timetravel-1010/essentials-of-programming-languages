#lang eopl

;------------------------------------------------------
;Exercises
;------------------------------------------------------

; Exercise 1
;; sublists :
;; Purpose:
;; Procedure that counts the number of sublists in a list.
;;
(define sublists
  (lambda (l)
    (cond
      [(null? l) 0]
      [(list? (car l))
       (+ 1 (+ (sublists(car l)) (sublists(cdr l))))]
      [else (sublists(cdr l))])))
;; Tests:
(sublists '(4 5 8 (1 2) 8 (7 8 9 (2 4))))
(sublists '(((a b) c) (1 (2 (3 4)) (d e f (g))) 5 ((6 (7) 8) 9)))


;; ejercicio 2
;; filtro
;; Proposito:
;; Procedimiento que recibe como argumentos una lista (lst) y un predicado (pred)
;; y retorna una nueva lista con los elementos de lst que cumplen con el predicado.

(define filtro
  (lambda (pred lst)
    (cond
      [(null? lst) empty]
      [(pred (car lst)) (cons (car lst) (filtro pred (cdr lst)))]
      [else (filtro pred (cdr lst))]
      )))

;; Pruebas
(filtro number? '((1 2 a (b)) 3 4 c 5 d))
; ’(3 4 5)
(filtro symbol? '((1 2 a (b)) 3 4 c 5 d))
; ’(c d)
(filtro list? '((1 2 a (b)) 3 4 c 5 d))
; ’((1 2 a (b)))
(filtro boolean? '(#t 21 af '(aswd 31 #f #t) #f #t))
; (#t #f #t)


#|
ejercicio 3.
inversion-listas:
Proposito:
Procedimiento que invierte las 2 sublistas de una lista.
|#

(define inversion-listas
  (lambda (l)
    (if (null? l) empty
        (cons (reverse (car l)) (inversion-listas (cdr l)))
        )
    )
  )


;;Pruebas:
(inversion-listas '((2 1) (4 3) (b a) (d c)))
(inversion-listas '((es Racket) (genial muy) (21 20)))
(inversion-listas '((como Hola) (:v estais)))
(inversion-listas '((y Divide) (:D venceras)))



;ejercicio 4
#|
aux-situar-en-lista
procedimiento auxiliar que inserta un una lista en una pocicion de otra lista
<lista>: '()
       : <valor1 valor2>
       :<lista> <lista>
|#

(define aux-situar-en-lista
  (lambda (lst pos elem acc)
    (cond
      [(eq? lst '()) '()]
      [(= acc pos) (cons elem (cdr lst))]
      [else (cons (car lst) (aux-situar-en-lista (cdr lst) pos elem (+ acc 1)))])))

#|
situar-en-lista procedimiento que llama al procedimiento aux-situar-en-lista con una valor constante en elem
|#

(define situar-en-lista
  (lambda (lst pos elem)
    (aux-situar-en-lista lst pos elem 0)))
;; pruebas
(situar-en-lista '(1 2 3 4 5 6 7 8 9) 0 'Comienzo)
(situar-en-lista '(1 2 3 4 5 6 7 8 9) 4 'Mitad)
(situar-en-lista '(1 2 3 4 5 6 7 8 9) 8 'Final)


;; ejercicio 5
;; ordenar
;; Proposito:
;; Procedimiento que recibe dos argumentos, un predicado (pred) y una lista de numeros (lst-num).
;; el procedimiento ordena la lista lst-num según el predicado pred, que puede tomar el valor 
;; de mayor que o menor que.

(define ordenar
  (lambda (pred lst-num)
    (let
        (;zona de declaraciones
         (selected-num (mayor-menor pred lst-num))
         )
      ;zona de expresiones
      (if (null? lst-num) empty
          (append (cons selected-num empty)
                  (ordenar pred (eliminar-elem lst-num selected-num)))
          )
      )))

;; Pruebas

;(ordenar < '(58 41 67 54 32 10))
; '(10 32 41 54 58 67)
;(ordenar > '(58 41 67 54 32 10))
; '(67 58 54 41 32 10)



;; mayor-menor
;; Proposito:
;; Procedimiento que recibe como argumento un pred (que puede ser > o <) y una lista (lst).
;; En caso de ingresar > se retorna el mayor elemento de lst y con < el menor.
(define mayor-menor
  (lambda (pred lst)
    (cond
      [(eqv? (length lst) 1) (car lst)]
      [(null? lst) empty]
      [else (
             (lambda (x y pred) (if (pred x y) x y)) (car lst) (mayor-menor pred (cdr lst)) pred)]
      )))

;; Pruebas

(mayor-menor > '(1 22 3 4))
; 22
(mayor-menor < '(2 3 1 -1 3))


;; eliminar-elem
;; Proposito:
;; Procedimiento que recibe como argumento una lista (lst) y un elemento (elem)
;; el procedimiento retorna una nueva lista sin la primer ocurrencia de elem en lst.

(define eliminar-elem
  (lambda (lst elem)
    (if (null? lst) empty
        (if (eqv? (car lst) elem) (cdr lst)
            (cons (car lst) (eliminar-elem (cdr lst) elem)))
        )))

;; Pruebas

(eliminar-elem '(1 2 3 4) '1)
; (2 3 4)
(eliminar-elem '(#t "hello-underworld" #f) #t)
; ("hello-underworld" #f)
(eliminar-elem '(3 2 #f r st #f) #f)
; (3 2 r st #f)


#|
ejercicio 6.
indice-lista:
Proposito:
Procedimiento que retorna la posicion del primer elemento
de una lista l, que cumple con un predicado pred. En caso
de no encontrarse retorna #f. 
|#

(define indice-lista
  (lambda (pred l)
    (letrec
        (
         (aux
          (lambda (pred l i)
            (if (null? l) #f
                (if (pred (car l)) i
                    (aux pred (cdr l) (+ i 1)))
                )
            )
            ) 
          )
      (aux pred l 0)
        )
    )
  )

;;Pruebas:
(indice-lista (lambda (x) (eqv? x 'd)) '(a b c d e f g))
(indice-lista (lambda (x) (> x 3)) '(0 1 2 3 4 5 6))
(indice-lista (lambda (x) (< x 10)) '(0 1 2 3 4 5 6))


;; ejercicio 7
;; contar-ocurrencias: symbol, S-list -> int
;; Proposito:
;; Procedimiento que recibe como argumentos un elemento simbolo (elem) y
;; una S-list teniendo en cuenta la siguiente gramatica.
;; <S-list> ::= ({<S-exp>}*)
;; <S-exp> ::= <S-list>
;; <S-exp> ::= <Symbol>
'((f x) y (((x z) x)))
'(<S-list> <Symbol> <S-list>)
'(<S-exp> <S-exp> <S-exp>)
;; el procedimiento retorna el numero de ocurrencias de elem en S-list.

(define contar-ocurrencias
  (lambda (elem s-list)
    (cond
      [(null? s-list) 0]
      [(symbol? s-list)
       (if (eqv? elem s-list) 1 0)]
      [else
       (+ (contar-ocurrencias elem (car s-list)) (contar-ocurrencias elem (cdr s-list)))]
        )))

;; Pruebas

(contar-ocurrencias 'x '((f x) y (((x z) x))))
; 3
(contar-ocurrencias 'x '((f x) y (((x z) () x))))
; 3
(contar-ocurrencias 'w '((f x) y (((x z) x))))
; 0


;; ejercicio 8
;; (intercambio :
;; Proposito:
;; Funcion que recibe dos elemntos y una lista y los intercambia en la lista
;;
(define intercambio
  (lambda (elem1 elem2 s-list)
    (cond
      [(null? s-list) '()]
      [(eqv? elem1 (car s-list)) (cons elem2 (intercambio elem1 elem2 (cdr s-list)))]
      [(eqv? elem2 (car s-list)) (cons elem1 (intercambio elem1 elem2 (cdr s-list)))]
      [(list? (car s-list))
       (cons (intercambio elem1 elem2 (car s-list)) (intercambio elem1 elem2 (cdr s-list)))]
      [else (cons (car s-list) (intercambio elem1 elem2 (cdr s-list)))])))
;pruebas
(intercambio 'a 'd '(a b c d))
(intercambio 'a 'd '(a d () c d))
(intercambio 'x 'y '((x) y (z (x))))




#|
ejercicio 9.
producto:
Proposito:
Procedimiento que recibe 2 listas y retorna en una lista de listas
el producto cartesiano entre las 2 listas. 
|#

(define producto
  (lambda (l1 l2)
    (letrec
        (
         (aux
          (lambda (l1 l2)
            (if (null? l2) empty
                (cons (list (car l1) (car l2)) (aux l1 (cdr l2)))
                )
            )
          )
         )
      (if (null? l1) empty
          (append (aux l1 l2) (producto (cdr l1) l2)))
      )
    )
  )

;;Pruebas:
(producto '(a b c) '(x y))
(producto '(a b c d) '(x y z))
(producto '(Hola) '(mundo todos gente))

;;ejercicio 10
;;Funcion que aplica un una funcion binaria F a todos los elementos que se encuentran
;;entre a y b y cumplen el predicado fileter al final el resultado se devuelve contando
;;con el acc
;;
(define filter-acum
  (lambda (a b F acc filter)
    (cond
      [(> a b) acc]
      [(filter a) (filter-acum (+ a 1) b F (F acc a) filter)]
      [else (filter-acum (+ a 1) b F acc filter)])
    ))
;pruebas
(filter-acum 1 10 + 0 odd?)
(filter-acum 1 10 + 0 even?)



;; ejercicio 11
;; list-append: list, list -> list
;; Proposito:
;; Procedimiento que recibe dos listas y retorna una nueva lista con
;; los elementos de lst2 agregados a los de lst.

(define list-append
  (lambda (lst lst2)
    (if (null? lst) lst2
        (cons (car lst) (list-append (cdr lst) lst2))
        )))

;; Pruebas

(list-append '(1 2 3) '(4 5))
; (1 2 3 4 5)
(list-append '(5 a b 3) '(g 4))
; (5 a b 3 g 4)
(list-append '(#t (lambda (y) y) 12 empty) '(#f 133 abc))
; (#t (lambda (y) y) 12 empty #f 133 abc)


#|
ejercicio 12.
operate:
Proposito:
Procedimiento que recibe 2 listas, donde lrators es una lista de
funciones binarias de tamano n y lrands es una lista de numeros de
tamano n + 1. La funcion retorna el resultado de aplicar sucesivamente
las operaciones en lrators a los valores en lrands  
|#

(define operate
  (lambda (lrators lrands)
    (letrec
        (
         (aux
           (lambda (l1 l2)
             (if (null? (cdr l1)) 
                 ((car l1) (cadr l2) (car l2)) 
                 ((car l1) (aux (cdr l1) (cdr l2)) (car l2))  
                 )
             )
          )
         )
      (aux (reverse lrators) (reverse lrands))
      )
    )
  )
;;Pruebas
(operate (list + * + - *) '(1 2 8 4 11 6))
(operate (list *) '(4 5))
(operate (list / / / / /) '(5000 5 5 4 5 2))
(operate (list * * * *) '(5 4 3 2 1))



;;ejercicio 13
;;funcion que ejecuta la operacion binaria F con cada pocicion n-esima de las listas L1 y L2
;;y devuelve una lista de igual tamanio con las operaciones realizadoas
;;
(define zip
  (lambda (F L1 L2)
    (if (null?  L1) '()
        (cons (F (car L1) (car L2)) (zip F (cdr L1) (cdr L2))))))
;;puebas
;; (zip + '(1 4) '(6 2))
;; (zip * '(11 5 6) '(10 9 8))



#|
ejercicio 14.
|#


#|
is-in-node?:
Proposito:
Procedimiento que recibe 2 parametros, un numero num y un nodo nd, busca
al numero num en el nodo nd, en caso de encontrarlo retorta #t en caso
contrario retorna #f
|#

(define is-in-node?
  (lambda (num nd)
    (letrec
        (
         (aux
          (lambda (num nd)
            (if (null? nd) 0
                (if (eqv? num (car nd)) 1
                    (+ (aux num (cadr nd)) (aux num (caddr nd)))
                    )
                )
            )
          )
         )
      (eqv? 1 (aux num nd))
      )
    )
  )


;;Pruebas
(is-in-node? 20 '(26 (20 (17 () ()) ()) (31 () ())))
(is-in-node? 12 '(10 () (14 (13 () ()) ())))



#|
path:
Proposito:
Procedimiento que recibe 2 parametros un numero num y un arbol binario
bst y retorna una lista con la ruta a tomar (iniciando desde el nodo raiz del arbol),
indicada por cadenas left y right, hasta llegar al numero num recibido. Si el numero
num es encontrado en el nodo raiz, el procedimiento debe retornar una lista vacia.
|#

(define path
  (lambda (num bst)
    (if (null? bst) empty
        (if (is-in-node? num (cadr bst))
            (cons 'left (path num (cadr bst)))  
            (if (is-in-node? num (caddr bst))
                (cons 'right (path num (caddr bst)))
                empty
                )
            )
        )
    )
  )

;;Pruebas
(path 17 '(14 (7 () (12 () ()))
                (26 (20 (17 () ())
                        ())
                    (31 () ()))))

(path 31 '(14 (7 () (12 () ()))
                (26 (20 (17 () ())
                        ())
                    (31 () ()))))

(path 4 '(8 (3
             (1 () ()) (6 (4 () ()) (7 () ())))
            (10 () (14 (13 () ()) ()))))
            
            
           

;; ejercicio 15
;; compose: 
;; Proposito:
;; Procedimiento que recibe dos argumentos proc1 y proc2 que son
;; prodecimientos de un argumento, y un valor val.
;; Retorna la composición de ambos procedimientos aplicados sobre val.

(define compose
  (lambda (proc1 proc2)
    (lambda (x)
      (proc1 (proc2 x))
      )))

;; Pruebas
((compose car cdr) '(a b c d))
; b
((compose number? car) '(a b c d))
; #f
((compose symbol? (compose car cdr)) '(a b c d))
; #t
((compose boolean? even?) 5)
; #t
((compose list? cdr) '(+ 2 6))
; #t
((compose number? (compose car cdr)) '(+ 2 6))
; #t



;;ejercicio 16
;;funcion que devuelve el procedimiento para encontrar un elemento en una lista

(define carCdr
  (lambda (elem lst errvalue)
    (cond
      [(null? lst) errvalue]
      [(list? (car lst))
       (let (
             (val (carCdr elem (car lst) errvalue))
             )
         (if (zero? (contar-ocurrencias errvalue val)) (append (list 'compose val 'car))
             (append (list 'compose (carCdr elem (cdr lst) errvalue) 'cdr))
             )
         )]
      [(eqv? (car lst) elem) 'car]
      [else
       (let (
             (val (carCdr elem (cdr lst) errvalue))
             )
         (if (zero? (contar-ocurrencias errvalue val)) (append (list 'compose val 'cdr))
             (append (list 'compose (carCdr elem (cdr lst) errvalue) 'cdr))
             )
         )]
      )))

(carCdr 'a '(a b c) 'fail)
; car

(carCdr 'c '(a b c) 'fail)
; (compose (compose car cdr) cdr)

(carCdr 'dog '(cat lion (fish dog ()) pig) 'fail)
; (compose (compose (compose (compose car cdr) car) cdr) cdr)

(carCdr 'dog '((fish dog ()) dog) 'fail)
; (compose (compose car cdr) car)

(carCdr 'dog '((dog) fish) 'fail)
; (compose car car)

(carCdr 'dog '(whale (cat) ((mouse ((dog) snake) fish) cow) (dog fish)) 'fail)
#|
(compose
 (compose
  (compose
   (compose
    (compose (compose (compose car car) car) cdr)
    car)
   car)
  cdr)
 cdr)
|#

(carCdr 'a '() 'fail)
; fail



