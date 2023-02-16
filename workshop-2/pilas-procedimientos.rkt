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
    (lambda (signal)
      (cond
        [(= signal 0) elem]
        [(= signal 1) env]
        [else (eopl:error "Error en extend")]))))
      

;;Area del programador

;;Ambiente e
(define e
  (extend-stack 'd 
        (extend-stack 8
              (extend-stack 'x
                    (extend-stack 14
                          (empty-stack))))))


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
                (cons 'extend (cons (first-stack->elem pila)
                                    (cons (aux (rest-stack->env pila)) empty))))))   
         )
      (cons 'extend (cons sym (cons (cons 'extend
                              (cons (first-stack->elem pila)
                                            (cons (aux (rest-stack->env pila)) empty))) empty)))
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
                (cons 'extend (cons (first-stack->elem pila) (cons (aux (rest-stack->env pila)) empty))))))  
         )
      (aux (rest-stack->env pila))
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
    (pila 0)))

#|
first-stack->elem:
Proposito:
Procedimiento que retorna el resto de un ambiente
|#
(define rest-stack->env
  (lambda (pila)
    (pila 1)))

