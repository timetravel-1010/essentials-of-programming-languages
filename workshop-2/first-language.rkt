#lang eopl


; Ejercicio 7.

;; <programa>   ::= <expresion>
;;              un-program (exp)
;;
;; <expresion>  ::= <numero>
;;              num-lit (n)
;;
;;              ::= (<expresion> <operacion> <expresion>)
;;              exp-lit (exp1 op exp2)
;;
;;              ::= <identificador>
;;              variable (id)
;;
;;              ::= var (identificador = <expresion>)* in <expresion>
;;              declaracion (ids exps cuerpo)
;;
;; <operacion>  := + - * /
;;              primitiva


; Definicion de la especificacion lexica.

(define lexical-s
  '(
    (espacio (whitespace) skip)
    (comentario ("&" (arbno (not #\newline))) skip)
    (identificador ("$" letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number) 
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
    )
  )

; Definicion de la especificacion gramatical.

(define grammatical-s
  '(
    (programa (expresion) un-program)
    (expresion (numero) numero-lit)
    (expresion ("(" expresion operacion expresion ")") exp-lit)
    (expresion (identificador) variable)
    (expresion ("var" (arbno identificador "=" expresion) "in" expresion) declaracion)
    (operacion ("+") add-op)
    (operacion ("-") sub-op)
    (operacion ("*") mul-op)
    (operacion ("/") div-op)
    )
  )

; Se definen los datatypes.
(sllgen:make-define-datatypes lexical-s grammatical-s)


; unparse: struct -> list.
; Procedimiento que recibe un arbol de sintaxis abstracta y retorna la representacion en sintaxis
; concreta usando listas.

(define unparse
  (lambda (prog)
    (cases programa prog
      [un-program (expresion) (unparse-expresion expresion)]
      )
    )
  )

; unparse-expresion: struct -> list.
; Procedimiento que recibe un arbol de sintaxis abstracta y retorna la representacion en sintaxis
; concreta usando listas.
 
(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      [numero-lit (numero) numero]
      [exp-lit (exp1 operacion exp2) (list (unparse-expresion exp1) (operadores operacion) (unparse-expresion exp2))]
      [variable (id) id]
      [declaracion (ids values listexp) (append '(var) (intercalar-variables ids (numlit->number values)) '(in) (list (unparse-expresion listexp)))]
      )
    )
  )

; operadores: struct -> symbol.
; Procedimiento que retorna un simbolo correspondiente a cada operacion.

(define operadores
  (lambda (op)
    (cases operacion op
      [add-op () '+]
      [sub-op () '-]
      [mul-op () '*]
      [div-op () '/]
      )
    )
  )

; numlit->number: list -> list
; Procedimiento que convierte los elementos de una lista de sintaxis abstracta a concreta.

(define numlit->number
  (lambda (ls)
    (map unparse-expresion ls)
    )
  )

; intercalar-variables: list, list -> list.
; Procedimiento que intercala los valores de las listas uno a uno y retorna una lista con esos valores.

(define intercalar-variables
  (lambda (ids values)
    (cond
      [(null? ids) empty]
      [else (append (list (car ids) '= (car values)) (intercalar-variables (cdr ids) (cdr values)))]
      )
    )
  )

; ================== sllgen =========================================

; Se crea el parser que recibe el string y convierte lo ingresado a un arbol de sintaxis abstracta.
(define string-parser
  (sllgen:make-string-parser lexical-s grammatical-s)
  )

; Se crea el parser que recibe el flujo de entrada del interpretador.
(define parser
  (sllgen:make-stream-parser lexical-s grammatical-s)
  )

; Se crea el interpretador.
(define interpretador
  (sllgen:make-rep-loop "&> " unparse parser)
  )









