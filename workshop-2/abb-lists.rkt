#lang eopl
(require rackunit)

; Kevin Satizabal Pascuas - 1910230
; Jose Manuel Madrid Torres - 1943827

; Ejercicio 5.

; Gramatica:

; Bintree ::= ()
; Bintree ::= (int Bintree Bintree)

; ======================= Implementación usando listas ========================

; empty-bintree: -> empty
; Procedimiento que crea un arbol binario vacio.

(define empty-bintree
  (lambda ()
    empty)
  )


; empty-bintree?: list -> boolean.
; Procedimiento que recibe un arbol binario y retorna #t en caso de estar
; vacio, #f en caso contrario.

(define empty-bintree?
  (lambda (tree)
    (null? tree)
    )
  )


; at-leaf?: list -> boolean.
; Procedimiento que verifica si la posicion actual es un arbol hoja.

(define at-leaf?
  (lambda (tree)
    (cond
      [(empty-bintree? tree) #t]
      [(and (empty-bintree? (move-to-right-son tree))
            (empty-bintree? (move-to-left-son tree))) #t]
      [else #f]
      )
    )
  )


; bintree-with-at-least-one-child?: list -> boolean.
; Procedimiento que verifica si el nodo actual es un arbol binario con al menos un hijo.

(define bintree-with-at-least-one-child?
  (lambda (tree)
    (cond
      [(empty-bintree? tree) #f]
      [(or (not (empty-bintree? (move-to-right-son tree)))
            (not (empty-bintree? (move-to-left-son tree)))) #t]
      [else #f]
      )
    )
  )


; current-element: list -> number.
; Procedimiento que recibe un arbol binario y retorna el valor de la raiz.

(define current-element
  (lambda (tree)
    (if (empty-bintree? tree) (empty-bintree)
        (car tree)
        )
  )
 )


; move-to-left-son: list -> list.
; Procedimiento que recibe un arbol binario y retorna el sub-arbol izquierdo (hijo izquierdo).

(define move-to-left-son
  (lambda (tree)
    (if (empty-bintree? tree) (empty-bintree)
        (cadr tree)  
      )
    )
  )

; move-to-right-son: list -> list
; Procedimiento que recibe un arbol binario y retorna el sub-arbol derecho (hijo derecho).

(define move-to-right-son
  (lambda (tree)
    (if (empty-bintree? tree) (empty-bintree)
        (caddr tree)
        )
    )
  )

; number->bintree: number -> list.
; Procedimiento que recibe un numero y retorna un arbol binario sin hijos
; con el numero como raiz.

(define number->bintree
  (lambda (n)
    [cons n
          (cons (empty-bintree) (cons (empty-bintree) (empty-bintree)))]
    )
  )

; insert-to-left: number, list -> list.
; Procedimiento que recibe un numero y un arbol binario e inserta el numero convertido en
; arbol binario a la izquierda del arbol recibido como argumento.

(define insert-to-left
  (lambda (n tree)
    (cond
      [(empty-bintree? tree) (number->bintree n)]
      [else
       [cons (car tree)
          (cons (number->bintree n) (cons (caddr tree)
                                          (empty-bintree)))]]
      )
    )
  )

; insert-to-right: number, list -> list.
; Procedimiento que recibe un numero y un arbol binario e inserta el numero convertido en
; arbol binario a la derecha del arbol recibido como argumento.

(define insert-to-right
  (lambda (n tree)
    (cond
      [(empty-bintree? tree) (number->bintree n)]
      [else
       [cons (car tree)
          (cons (cadr tree) (cons  (number->bintree n)
                                   (empty-bintree)))]])
    )
  )

; bin-tree-order-validation: list -> boolean
; Procedimiento que recibe un arbol binario de busqueda y retorna #t en caso de que cumpla
; con la propiedad de orden, #f en caso contrario.

(define bin-tree-order-validation
  (lambda (tree)
    (letrec
        ( ; zona de declaraciones
         (first (current-element tree))
         (left-bintree (move-to-left-son tree))
         (right-bintree (move-to-right-son tree))
         (left (current-element left-bintree))
         (right (current-element right-bintree))
         (func
          (lambda (tree first)
            (cond
              [(empty-bintree? tree) #t]
              [else
               (and
                (if (equal? left (empty-bintree)) #t
                    (< left first)
                    )
                (if (equal? right (empty-bintree)) #t
                    (> right first)
                    )
                (bin-tree-order-validation left-bintree)
                (bin-tree-order-validation right-bintree)
                )
               ]
              )
            )
          )
         )
      (func tree first)
      )
    )
  )

; insert-element-into-bintree: list, number -> list.
; Procedimiento que recibe un arbol binario de busqueda y un numero, retorna un nuevo arbol
; con el numero insertado cumpliendo la propiedad de orden. En caso de que el numero ya se
; encuentre en el arbol retorna el mismo arbol.

(define insert-element-into-bintree
  (lambda (tree n)
    (cond
      [(null? tree) (number->bintree n)]
      [(equal? (current-element tree) n) tree]
      [(letrec
           (;zona de declaraciones
            (l-tree (move-to-left-son tree))
            (r-tree (move-to-right-son tree))
            (current-node (current-element tree))
            )
         ;zona de expresiones
         (cond
           [(> current-node n) (list current-node
                                     (insert-element-into-bintree l-tree n)
                                     r-tree)]
           [else (list current-node
                       l-tree
                       (insert-element-into-bintree r-tree n)
                       )]
           )
         )]
      )
    )
  )
 

(define ejemplo
  '(8
    (3 
     (1 () ())
     (6
      (4 () ())
      (7 () ()))) 
    (10
     ()
     (14
      (13 () ())
      ()))
    )
  )

(define ejemplo-2
  '(8
    (3 
     (1 () (2 () ()))
     (6
      (4 () ())
      (7 () ()))) 
    (10
     ()
     (14
      (13 () ())
      ()))
    )
  )

(define arbole
  '(5
    (4
     (3 () ())
     ())
     (6
     ()
     (9 () ())))
  )

(define arbole-2
  '(5
    (4
     (3 (1 () ()) ())
     ())
     (6
     ()
     (9 () ())))
  )

; Pruebas:

(check-equal? (empty-bintree? '()) #t)
(check-equal? (empty-bintree? '(1 () ())) #f)

(check-equal? (at-leaf? '(2 () ())) #t)
(check-equal? (at-leaf? '(3 (1 () ()) 4 () ())) #f)

(check-equal? (bintree-with-at-least-one-child? '()) #f)
(check-equal? (bintree-with-at-least-one-child? '(2 () (3 () ()))) #t)
(check-equal? (bintree-with-at-least-one-child? '(4 () ())) #f)

(check-equal? (current-element '(2 () ())) 2)
(check-equal? (current-element '(3 (1 () ()) ())) 3)
(check-equal? (current-element '()) '())

(check-equal? (move-to-left-son '(2 (1 () ()) (3 () ()))) '(1 () ()))
(check-equal? (move-to-left-son '(2 () ())) '())
(check-equal? (move-to-left-son '(5 (4 (1 () ()) ()) (6 () ()))) '(4 (1 () ()) ()))

(check-equal? (move-to-right-son '(2 (1 () ()) (3 () ()))) '(3 () ()))
(check-equal? (move-to-right-son '(2 () ())) '())
(check-equal? (move-to-right-son '(5 (4 (1 () ()) ()) (6 () ()))) '(6 () ()))

(check-equal? (number->bintree 4) '(4 () ()))
(check-equal? (number->bintree 2) '(2 () ()))

(check-equal? (insert-to-left 3 '(5 () ())) '(5 (3 () ()) ()))
(check-equal? (insert-to-left 2 '(4 (1 () ()) (5 () ()))) '(4 (2 () ()) (5 () ())))

(check-equal? (insert-to-right 7 '(5 () ())) '(5 () (7 () ())))
(check-equal? (insert-to-right 6 '(4 (1 () ()) (5 () ()))) '(4 (1 () ()) (6 () ())))

(check-equal? (bin-tree-order-validation ejemplo) #t)
(check-equal? (bin-tree-order-validation '(2 (3 () ()) ())) #f)
(check-equal? (bin-tree-order-validation arbole) #t)

(check-equal? (insert-element-into-bintree ejemplo 2) ejemplo-2)
(check-equal? (insert-element-into-bintree arbole 1) arbole-2)

