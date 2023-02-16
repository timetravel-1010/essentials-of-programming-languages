#lang eopl
(require rackunit)

; Ejercicio 5.

; Gramatica:

; Bintree ::= ()
; Bintree ::= (int Bintree Bintree)

; ======================= Implementación usando Datatypes ========================

; Definicion del tipo de dato binary-tree.

(define-datatype binary-tree binary-tree?
  [empty-binary-tree]
  [leaf-node (datum number?)]
  [interior-node (node number?)
                 (left binary-tree?)
                 (right binary-tree?)]
  )

; empty-bintree: -> struct.
; Procedimiento constructor de un arbol binario vacio.

(define empty-bintree
  (lambda ()
    (empty-binary-tree)
    )
  )

; current-element: struct -> list | number.
; Procedimiento que retorna el nodo raiz del arbol ingresado.

(define current-element
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () '()]
      [leaf-node (datum) datum]
      [interior-node (node left right) node]
      )
    )
  )

; move-to-left-son: struct -> struct.
; Procedimiento que recibe un arbol binario y retorna el hijo izquierdo o un arbol binario vacio
; en caso de no tenerlo.

(define move-to-left-son
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () (empty-binary-tree)]
      [leaf-node (datum) (empty-binary-tree)]
      [interior-node (node left right) left]
      )
    )
  )

; move-to-right-son: struct -> struct.
; Procedimiento que recibe un arbol binario y retorna el sub-arbol derecho (hijo derecho).

(define move-to-right-son
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () (empty-binary-tree)]
      [leaf-node (datum) (empty-binary-tree)]
      [interior-node (node left right) right]
      )
    )
  )

; number->bintree: number -> struct.
; Procedimiento que recibe un numero y retorna un arbol binario sin hijos
; con el numero como raiz.

(define number->bintree
  (lambda (n)
    (leaf-node n)
    )
  )

; empty-bintree?: struct -> boolean.
; Procedimiento que recibe un arbol binario y retorna #t en caso de estar
; vacio, #f en caso contrario.

(define empty-bintree?
  (lambda (tree)
    (equal? tree (empty-binary-tree))
    )
  )

; at-leaf?: struct -> boolean.
; Procedimiento que verifica si la posicion actual es un arbol hoja.

(define at-leaf?
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () #t]
      [leaf-node (node) #t]
      [interior-node (node left right) #f]
      )
    )
  )

; bintree-with-at-least-one-child?: struct -> boolean.
; Procedimiento que verifica si el nodo actual es un arbol binario con al menos un hijo.

(define bintree-with-at-least-one-child?
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () #f]
      [leaf-node (node) #f]
      [interior-node (node left right) (if (or (not (equal? left (empty-binary-tree)))
                                               (not (equal? right (empty-binary-tree))))
                                           #t #f)]
        )
    )
  )

; insert-to-left: number, struct -> struct.
; Procedimiento que recibe un numero y un arbol binario e inserta el numero convertido en
; arbol binario a la izquierda del arbol recibido como argumento.

(define insert-to-left
  (lambda (n tree)
    (cases binary-tree tree
      [empty-binary-tree () (number->bintree n)]
      [leaf-node (node) (interior-node node (leaf-node n) (empty-binary-tree))]
      [interior-node (node left right) (interior-node node (leaf-node n) right)]
      )
    )
  )

; insert-to-right: number, struct -> struct.
; Procedimiento que recibe un numero y un arbol binario e inserta el numero convertido en
; arbol binario a la derecha del arbol recibido como argumento.

(define insert-to-right
  (lambda (n tree)
    (cases binary-tree tree
      [empty-binary-tree () (number->bintree n)]
      [leaf-node (node) (interior-node node (empty-binary-tree) (leaf-node n))]
      [interior-node (node left right) (interior-node node left (leaf-node n))]
      )
    )
  )

; bin-tree-order-validation: struct -> boolean.
; Procedimiento que recibe un arbol binario de busqueda y retorna #t en caso de que cumpla
; con la propiedad de orden, #f en caso contrario.

(define bintree-order-validation
  (lambda (tree)
    (cases binary-tree tree
      [empty-binary-tree () #t]
      [leaf-node (node) #t]
      [interior-node (node left right)
                     (letrec
                         (; zona de declaraciones
                          (l-left (current-element left)) ; valor del nodo izquierdo.
                          (l-right (current-element right)) ; valor del nodo derecho.
                          )
                       ; zona de expresiones
                       (and
                        (if (empty-bintree? left) #t
                            (< l-left node))
                        (if (empty-bintree? right) #t
                            (< node l-right))
                        (bintree-order-validation left)
                        (bintree-order-validation right)
                        )
                       )]
      )
    )
  )

; insert-element-into-bintree: struct, number -> struct.
; Procedimiento que recibe un arbol binario de busqueda y un numero, retorna un nuevo arbol
; con el numero insertado cumpliendo la propiedad de orden. En caso de que el numero ya se
; encuentre en el arbol retorna el mismo arbol.

(define insert-element-into-bintree
  (lambda (tree n)
    (cases binary-tree tree
      [empty-binary-tree () (number->bintree n)]
      [leaf-node (node) (if (> node n) (interior-node node (leaf-node n) (empty-binary-tree))
                            (interior-node node (empty-binary-tree) (leaf-node n)))]
      [interior-node (node left right)
                     (letrec
                         (; zona de declaraciones
                          )
                       ; zona de expresiones
                       (if (equal? node n) tree
                           (cond
                             [(> node n) (interior-node node
                                                        (insert-element-into-bintree left n)
                                                        right)]
                             [else (interior-node node
                                                  left
                                                  (insert-element-into-bintree right n)
                                                  )]
                             )
                           )
                       )]
      )
    )
  )


; parse: list -> struct.
; Procedimiento que recibe una lista y retorna el arbol de sintaxis abstracta si la lista ingresada
; cumple con los requisitos de construccion de un arbol binario.

(define parse
  (lambda (l)
    (cond
      [(null? l) (empty-binary-tree)]
      [(null? (cadr l)) (if (null? (caddr l)) (leaf-node (car l))
                            (interior-node (car l)
                                           (empty-binary-tree)
                                           (parse (caddr l))))]
      [(null? (caddr l)) (interior-node (car l) (parse (cadr l)) (empty-binary-tree))]
      [else
       (interior-node (car l)
                      (parse (cadr l))
                      (parse (caddr l))
                      )
       ]
      )
    )
  )

; unparse: struct -> list.
; Procedimiento que recibe un arbol de sintaxis abstracta y retorna la representacion
; del arbol binario en listas.

(define unparse
  (lambda (bin)
    (cases binary-tree bin
      [empty-binary-tree () '()]
      [leaf-node (node) (cons node (cons empty (cons empty empty)))]
      [interior-node (node left right) (cons node (cons (unparse left) (cons (unparse right) empty)))]
      )
    )
  )

(define ejemplo
  (interior-node 8
                 (interior-node 3
                                (leaf-node 1)
                                (interior-node 6
                                               (leaf-node 4)
                                               (leaf-node 7)))
                 (interior-node 10
                                (empty-binary-tree)
                                (interior-node 14
                                               (leaf-node 13)
                                               (empty-binary-tree)))
                 )
  )

(define ejemplo-2
  (interior-node 8
                 (interior-node 3
                                (interior-node 1
                                               (empty-binary-tree)
                                               (leaf-node 2))
                                (interior-node 6
                                               (leaf-node 4)
                                               (leaf-node 7)))
                 (interior-node 10
                                (empty-binary-tree)
                                (interior-node 14
                                               (leaf-node 13)
                                               (empty-binary-tree)))
                 )
  )

(define ejemplo-listas
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

(define arbole
  (interior-node 5
                 (interior-node 4
                                (leaf-node 3)
                                (empty-binary-tree))
                 (interior-node 6
                                (empty-binary-tree)
                                (leaf-node 9))
                 )
  )

(define arbole-2
  (interior-node 5
                 (interior-node 4
                                (interior-node 3
                                               (leaf-node 1)
                                               (empty-binary-tree))
                                (empty-binary-tree))
                 (interior-node 6
                                (empty-binary-tree)
                                (leaf-node 9))
                 )
  )

(define arbole-listas
  '(5
    (4
     (3 () ())
     ())
    (6
     ()
     (9 () ())))
  )

; Pruebas:


(check-equal? (empty-bintree? (empty-binary-tree)) #t)
(check-equal? (empty-bintree? (leaf-node 1)) #f)

(check-equal? (at-leaf? (empty-binary-tree)) #t)
(check-equal? (at-leaf? (interior-node 3 (leaf-node 1) (leaf-node 4))) #f)

(check-equal? (bintree-with-at-least-one-child? (empty-binary-tree)) #f)
(check-equal? (bintree-with-at-least-one-child? (interior-node 2
                                                               (empty-binary-tree)
                                                               (leaf-node 3))) #t)
(check-equal? (bintree-with-at-least-one-child? (leaf-node 4)) #f)

(check-equal? (current-element (leaf-node 2)) 2)
(check-equal? (current-element (interior-node 3 (leaf-node 1) (empty-binary-tree))) 3)
(check-equal? (current-element (empty-binary-tree)) '())

(check-equal? (move-to-left-son (interior-node 2 (leaf-node 1) (leaf-node 3))) (leaf-node 1))

(check-equal? (move-to-left-son (leaf-node 2)) (empty-binary-tree))
(check-equal? (move-to-left-son (interior-node 5
                                               (interior-node 4
                                                              (leaf-node 1)
                                                              (empty-binary-tree))
                                               (leaf-node 6)))
              (interior-node 4 (leaf-node 1) (empty-binary-tree)))

(check-equal? (move-to-right-son (interior-node 2 (leaf-node 1) (leaf-node 3))) (leaf-node 3))
(check-equal? (move-to-right-son (leaf-node 2)) (empty-binary-tree))

(check-equal? (number->bintree 4) (leaf-node 4))
(check-equal? (number->bintree 2) (leaf-node 2))

(check-equal? (insert-to-left 3 (leaf-node 5)) (interior-node 5 (leaf-node 3) (empty-binary-tree)))
(check-equal? (insert-to-left 2
                              (interior-node 4
                                             (leaf-node 1)
                                             (leaf-node 5)))
              (interior-node 4 (leaf-node 2) (leaf-node 5)))

(check-equal? (insert-to-right 7 (leaf-node 5)) (interior-node 5 (empty-binary-tree) (leaf-node 7)))
(check-equal? (insert-to-right 6 (interior-node 4
                                                (leaf-node 1)
                                                (leaf-node 5)))
              (interior-node 4 (leaf-node 1) (leaf-node 6)))

(check-equal? (bintree-order-validation ejemplo) #t)
(check-equal? (bintree-order-validation (interior-node 2 (leaf-node 3) (empty-binary-tree))) #f)
(check-equal? (bintree-order-validation arbole) #t)

(check-equal? (insert-element-into-bintree ejemplo 2) ejemplo-2)
(check-equal? (insert-element-into-bintree arbole 1) arbole-2)

(check-equal? (parse ejemplo-listas) ejemplo)
(check-equal? (parse arbole-listas) arbole)

(check-equal? (unparse ejemplo) ejemplo-listas)
(check-equal? (unparse arbole) arbole-listas)
