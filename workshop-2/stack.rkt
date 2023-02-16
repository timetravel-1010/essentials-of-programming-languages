#lang eopl

; Ejercicio 3.

<stack> ::= (empty-stack)
<stack> ::= (extend-stack elem <stack>)

;Definicion del tipo de dato stack.
(define-datatype stack stack?
  (empty-stack)
  (extend-stack (elem scheme-value?)
            (pila stack?)))


;;Procedimiento que retorna #true para cualquier 
(define scheme-value?
  (lambda (x)
    #true))


#|
push:
Proposito:
Procedimiento que inserta un elemento a la pila
|#
(define push
  (lambda (elem pila)
    (extend-stack elem pila)))


#|
pop:
Proposito:
Procedimiento que retira el elemento superior de una pila
|#
(define pop
  (lambda (pila)
    (cases stack pila
      (empty-stack () (eopl:error "La pila esta vacia"))
      (extend-stack (elem pila)
                    pila))))


#|
top:
Proposito:
Procedimiento que retorna el elemento superior de una pila sin retirarlo.
|#
(define top
  (lambda (pila)
    (cases stack pila
      (empty-stack () (eopl:error "La pila esta vacia"))
      (extend-stack (elem pila)
                    elem))))
;;Ambiente e
(define e
  (extend-stack 'd 
        (extend-stack '10
              (extend-stack 'z
                    (extend-stack '13
                          (empty-stack))))))
;;Ambiente a
(define a
  (empty-stack))
