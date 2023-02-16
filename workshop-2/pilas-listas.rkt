#lang eopl

; Ejercicio 3.

<stack> ::= (empty-stack)
<stack> ::= (extend-stack elem <stack>)

;;Constructores

#|
empty-stack:
Proposito:
Procedimiento que representa una pila vacia
|#
(define empty-stack
 (lambda ()
  '(empty-stack)))

#|
extend-stack:
Proposito:
Procedimiento que se encarga de extender una pila
|#
(define extend-stack
  (lambda (elem env)
    (list 'extend elem env)))


;;Area del programador

;;Ambiente e
(define e
  (extend-stack 'd 
        (extend-stack 8
              (extend-stack 'x
                    (extend-stack 14
                          (empty-stack))))))
;;Ambiente p
(define p
  '(empty-stack))


#|
push:
Proposito:
Procedimiento que inserta un elemento a la pila
|#
(define push
  (lambda (sym pila)
    (letrec
        (
         (aux
          (lambda (pila)
            (if (empty-stack? pila) (empty-stack)
                (list 'extend (first-stack->elem pila) (aux (rest-stack->env pila)))))) 
         )
      (list 'extend sym (list 'extend (first-stack->elem pila) (aux (rest-stack->env pila))))
      )
    )
  )


#|
pop:
Proposito:
Procedimiento que retira el elemento superior de una pila
|#
(define pop
  (lambda (pila)
    (letrec
        (
         (aux
          (lambda (pila)
            (if (empty-stack? pila) (empty-stack)
                (list 'extend (first-stack->elem pila) (aux (rest-stack->env pila)))))) 
         )
      (aux (caddr pila))
      )
    )
  )

#|
top:
Proposito:
Procedimiento que retorna el elemento superior de una pila sin retirarlo.
|#
(define top
  (lambda (pila)
    (first-stack->elem pila)))


;;Observadores

#|
empty-stack?:
Proposito:
Procedimiento que se encarga de preguntar si ana pila esta vacia.
|#
(define empty-stack?
  (lambda (pila)
    (equal? pila '(empty-stack))))


;;Extractores

#|
first-stack->elem:
Proposito:
Procedimiento que retorna el primer elemento de una ambiente.
|#
(define first-stack->elem
  (lambda (pila)
    (cadr pila)))

#|
first-stack->elem:
Proposito:
Procedimiento que retorna el resto de un ambiente
|#
(define rest-stack->env
  (lambda (pila)
    (caddr pila)))



