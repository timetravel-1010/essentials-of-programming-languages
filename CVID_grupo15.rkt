#lang eopl
(require racket/string)


;; La definición BNF para las expresiones del lenguaje:
#|

<program>          ::=  <global> <expression>

<global>           ::=  {<identifier> = <expression>}*(,)
 
<expression>       ::=  <number>

                   ::=  \" <text> \"

                   ::=   ´ (<letter> | <number>)  ´

                   ::=  <identifier>

                   ::=  <expr-bool>

                   ::=  []

                   ::=  x8 ({<expression>}*)

                   ::=  var {<identifier> = <expression>}*(,) ; in {<expression>}

                   ::=  const {<identifier> = <expression>}*(,) ; in {<expression>}

                   ::=  rec {<identifier> {(<identifier>)}*(,) = <expression>}* in {<expression>}

                   ::=  unic {<identifier> = <expression>}*(,) ; in {<expression>}

                   ::=  assign {<identifier> => <expression>}*(,) ; in {<expression>}

                   ::=  #non-value

                   ::=  op (<expression> <arith-prim> <expression>)

                   ::=  <arith-prim> <expression>

                   ::=  func {(<identifier>)}*(,) {<expression>}

                   ::=  eval <expression> [{<expression>}*(,)]

                   ::=  [{expression}*(,)]

                   ::=  vector [{expression}*(,)]

                   ::=  {<expression> : <expression>}+(,)

                   ::=  sequence {<expression>}+(,)

                   ::=  if (<expression>) {<expression>} else {<expression>}

                   ::=  cond {[<expression> : <expression>]}+ ; [else : <expression>]

                   ::=  while (<expr-bool>) {<expression>}

                   ::=  for ( <identifier> = <expression> ; <arith-prim> ; <expr-bool> ) { <expression> }

                   ::=  <prim-str>

                   ::=  <prim-list>

                   ::=  <prim-vec>

                   ::=  <prim-reg>

<prim-str>         ::=  concat ( <expression> , <expression> )

                   ::=  length ( <expression> )

<identifier>       ::=  <letter> {<letter> | 0,...,9}*

                   ::=  @ <letter> {<letter> | 0,...,9}*

<letter>           ::=  A...Z | a...z

<expr-bool>        ::=  compare (<expression> <pred-prim> <expression>)

                   ::=  (<expr-bool> <oper-bin-bool> <expr-bool>)

                   ::=  <oper-un-bool> (<expr-bool>)

                   ::=  <bool>

<bool>             ::=  true | false

<arith-prim>       ::=  + | - | * | / | % | ++ | --    

<oper-bin-bool>    ::=  and | or | xor | not

<pred-prim>        ::=  > | >= | < | <= | == | <>

<prim-list>        ::=  isEmpty? ( <expression> )
                   ::=  empty
                   ::=  cons ( <expression> , <expression> )
                   ::=  isList? ( <expression> )
                   ::=  first ( <expression> )
                   ::=  rest ( expression )
                   ::=  append ( <expression> , <expression> )

<prim-vec>         ::=  new-vector ( <number> )
                   ::=  index ( <expression> , <number> )
                   ::=  update ( <expression> , <number> , <expression>)
                   ::=  isVector? ( <expression> )

<prim-reg>         ::=  newRegister ( <expression> ) 
                   ::=  getRegister ( <expression> , <expression> )
                   ::=  updateRegister ( <expression> , <expression> , <expression> )
                   ::=  isRegister? ( <expression> )

|#


;******************************************************************************************

; Especificacion lexica.
(define lexical-s
  '(
    (space (whitespace) skip)
    (comment ("$" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit))) symbol)
    (char ((or "\"" "\" ") (or letter digit) (or "\"" "\" ")) string)
    (text ((or "\"" "\" ") letter (arbno (or letter digit whitespace)) "\"") string) ;; CAMBIO
    )
  )

; Especificacion gramatical.
(define grammar
  '(
    (program (global expression) a-program)
    (global ("global" "(" (separated-list identifier "=" expression ",") ")") global-def)
    
    ; Data.
    (expression (number) number-exp) 
    (expression (char) char-exp)
    (expression (text) string-exp)
    (expression ("[]") empty-exp)
    (expression ("x8" "(" (arbno number) ")" ) octal-exp) 

    (bool ("true") true-exp)
    (bool ("false") false-exp)

    (expression (identifier) identifier-exp)
    (expression ("@" identifier) ref-var-exp) 

    ; Definitions.
    (expression ("var" (separated-list identifier "=" expression ",") ";" "in" "{" expression "}") var-exp)
    (expression ("const" (separated-list identifier "=" expression ",") ";" "in" "{" expression "}") const-exp)
    (expression ("rec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" "{" expression "}") rec-exp)
    (expression ("unic" (separated-list identifier "=" optional-unic-exp ",") ";" "in" "{" expression "}") unic-exp)
    (expression ("assign" (separated-list identifier "=>" expression ",") ";" "in" "{" expression "}") assign-exp)
    (optional-unic-exp ("#non-value") non-value-exp)
    (optional-unic-exp (expression) optional-exp)

    ; Data constructors.
    (expression ("[" (separated-list expression ",") "]") list-exp) 
    (expression ("vector" "(" (separated-list expression ",") ")") vector-exp)
    (expression ("{" identifier ":" expression (arbno "," identifier ":" expression) "}") register-exp) 
    (expression (expr-bool) exp-bool)

    ; Operations
    (expression ("op" "(" expression arith-bin-prim expression ")" ) primapp-bin-exp)
    (expression (arith-un-prim expression) primapp-un-exp) ; -> --4 | ++1

    ; Control structures.
    (expression ("sequence" "{" expression (arbno "," expression) "}") secuence-exp)
    (expression ("if" "(" expression  ")" "{" expression "}" "else" "{" expression "}") if-exp)
    (expression ("cond" (arbno "[" expression  ":" expression "]") ";" "[" "else" ":" expression "]") cond-exp)
    (expression ("while" "(" expr-bool ")" "{" expression "}") while-exp)
    (expression ("for" "(" identifier "=" expression ";" arith-un-prim ";" expr-bool ")" "{" expression "}") for-exp)
    
    (expr-bool ("compare" "(" expression pred-prim expression ")") compare-exp)
    (expr-bool ("(" expr-bool oper-bin-bool expr-bool ")") oper-bin-exp)
    (expr-bool (oper-un-bool "(" expr-bool ")") oper-un-exp)
    (expr-bool (bool) bool-exp)

    ; Boolean primitives.
    (oper-bin-bool ("and") and-prim)
    (oper-bin-bool ("or") or-prim)
    (oper-bin-bool ("xor") xor-prim)
    
    (oper-un-bool ("not") not-prim)
    
    ; Comparison primitives.
    (pred-prim (">") greater-prim)
    (pred-prim (">=") greaterEqual-prim)
    (pred-prim ("<") less-prim)
    (pred-prim ("<=") lessEqual-prim)
    (pred-prim ("==") equal-prim)
    (pred-prim ("<>") notEqual-prim)

    (arith-bin-prim (arith-bin-integers) prim-integers)
    (arith-bin-prim (arith-bin-octals) prim-octals)

    ; Arithmetic primitives for integers.
    (arith-bin-integers ("+") add-prim)
    (arith-bin-integers ("-") sub-prim)
    (arith-bin-integers ("*") mul-prim)
    (arith-bin-integers ("%") mod-prim)
    (arith-bin-integers ("/") div-prim)
    
    (arith-un-prim ("++") inc-prim)
    (arith-un-prim ("--") dec-prim)

    ; Arithmetic primitives for octals.
    (arith-bin-octals ("+_oct") add-octal-prim)
    (arith-bin-octals ("-_oct") sub-octal-prim)
    (arith-bin-octals ("*_oct") mul-octal-prim)
    
    (arith-un-prim ("++_oct") inc-octal-prim)
    (arith-un-prim ("--_oct") dec-octal-prim)
    
    ; String primitives.
    (expression (prim-str) pri-str)
    (prim-str ("concat" "(" expression "," expression ")") concat-exp)
    (prim-str ("length" "(" expression ")") length-exp)
    
    ; List primitives.
    (expression (prim-list) pri-list)
    (prim-list ("isEmpty?" "(" expression ")") isEmpty-prim)
    (prim-list ("empty") empty-prim)
    (prim-list ("cons" "(" expression "," expression ")") cons-prim)
    (prim-list ("isList?" "(" expression ")") isList-prim)
    (prim-list ("first" "(" expression ")") first-prim)
    (prim-list ("first-l" "(" expression ")") first-prim-l)
    (prim-list ("rest" "(" expression ")") rest-prim)
    (prim-list ("append" "(" expression "," expression ")") append-prim)
    
    ; Vectors primitives.
    (expression (prim-vec) pri-exp)
    (prim-vec ("new-vector" "(" number ")") new-vector) ; number -> number of elements.
    (prim-vec ("index" "(" expression "," number ")") index-vec)
    (prim-vec ("update" "(" expression "," number "," expression")") update-vec) ; vector, position, new element.
    (prim-vec ("isVector?" "(" expression ")") isVector-vec)
    
    ; Register primitives.
    (expression (prim-reg) pri-reg)
    (prim-reg ("newRegister" "(" identifier ":" expression (arbno "," identifier ":" expression) ")") new-register) 
    (prim-reg ("getRegister" "(" expression "," identifier ")") get-register) ; register, key. 
    (prim-reg ("updateRegister" "(" expression "," identifier "," expression ")") update-register) ; register, key, value.
    (prim-reg ("isRegister?" "(" expression ")") isRegister-register)

    ; Procedures.
    (expression ("func" "(" (separated-list identifier ",") ")" "{" expression "}")  func-exp)
    (expression ("eval" expression "[" (separated-list expression ",") "]") app-exp)

    ; Print.
    (expression ("write" "(" expression ")") write-exp)
    (expression ("writeln" "(" expression ")") writeln-exp)
  ))

(sllgen:make-define-datatypes lexical-s grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexical-s grammar)))

; ======================================================================================================

;El FrontEnd (Analisis lexico (scanner) y sintactico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexical-s grammar))

;El Analizador Lexico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexical-s grammar))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "&> "
                         (lambda (pgm) (eval-program  pgm)) 
                         (sllgen:make-stream-parser 
                          lexical-s
                          grammar))
  )

; =======================================================================================================
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (global-defs exp) (eval-exp exp (init-env) (eval-global global-defs))))
    ))


; ========================== Para el ambiente global =============================================

(define eval-global
  (lambda (global-defs)
    (cases global global-defs
      [global-def (ids exps) (create-global-env ids exps)]
      )
    ))

(define create-global-env
  (lambda (ids exps)
    (if (null? ids) (empty-env)
        (letrec (; declaration zone.
              (env (empty-env))
              (id (list (car ids)))
              (exp (list (direct-target (eval-exp (car exps) (empty-env) (empty-env)))))
              )
          (add-value (cdr ids) (cdr exps) (create-extended-env id exp (empty-env) (var-type)))
          ))
    ))

(define add-value
  (lambda (ids exps env)
    (if (null? ids) env
        (letrec (
             (id (car ids))
             (exp (direct-target (eval-exp (car exps) env (empty-env))))
             (ids-old (cases environment env
                     [empty-env () '()]
                     [extended-env (ids exps old-env type) ids]))
             )
                             (if (null? ids-old) env
                                 (if (not (rib-find-position id ids-old)) (add-value (cdr ids) (cdr exps) (create-extended-env (list id) (list exp) env (var-type)))
                                     (eopl:error 'create-global-exp "There is already a variable with identifier ~s" id))
                                 )))
    ))

; Initial environment.
(define init-env
  (lambda ()
     (empty-env)))


; ============================================ Expresiones ====================================================


;eval-exp: <expression> <enviroment> -> 
; evalua la expresión en el ambiente de entrada
(define eval-exp
  (lambda (exp env global-env)
    (cases expression exp
      ; Valores
      [number-exp (datum) datum]
      [identifier-exp (id) (apply-env env id global-env)]
      [string-exp (str) (string-trim str "\"")]
      [char-exp (ch)  (string-trim ch "\"")]
      [func-exp (ids exp) (closure ids exp env)]
      [list-exp (exp) exp]
      [octal-exp (vals) (a-octal 'x8 vals)]
      [empty-exp () (empty-exp)]
      [vector-exp (exp) (a-vector exp)]
      [register-exp (exp1 exp2 list-exp1 list-exp2) (a-register (append (list exp1) list-exp1) (append (list exp2) list-exp2))] 
      [ref-var-exp (id) (apply-env env id global-env)]

      ; Definiciones 
      [var-exp (ids exps body) (let ((args (eval-def-exp-rands exps env global-env)))
                                  (eval-exp body (create-extended-env ids args env (var-type)) global-env))]
      [const-exp (ids exps body) (let ((args (eval-def-exp-rands exps env global-env)))
                                  (eval-exp body (create-extended-env ids args env (const-type)) global-env))]
      [unic-exp (ids exps body) (begin
                                  (let
                                      ((args (eval-unic-exp-rands exps env global-env)))                                   
                                    (eval-exp body (create-extended-env ids args env (unic-type)) global-env)))]
      [rec-exp (proc-names idss bodies letrec-body) (eval-exp letrec-body
                                                              (rec-extended-env proc-names idss bodies env)
                                                              global-env)]
      [assign-exp (ids exps body)  (for-each (lambda (id rhs-exp)
                                                     (let ( (id-type (find-definition-type id env global-env)) )
                                                       (cases definition-type id-type
                                                         [var-type () (apply-setref id rhs-exp env global-env)]
                                                         [const-type () (eopl:error 'eval-exp "Cannot modify const definition with identifier ~s" id)]
                                                         [unic-type () (if (equal? (apply-unic-env env id global-env) 'non-value) 
                                                                           [apply-setref id rhs-exp env global-env]
                                                                           [eopl:error 'eval-exp "Definition ~s is already declared" id]
                                                                           )]  
                                                         )
                                                       )) ids exps)
                                         (eval-exp body env global-env)]

      [exp-bool (exp) (eval-bool exp env global-env)]

      ; Manejo de listas
      [pri-list (exp) 
          (cases prim-list exp
            [isEmpty-prim (exp) (let ((ls (eval-exp exp env global-env)))
                                  (if (equal? ls (empty-exp)) #t
                                      (equal? (empty-exp) (car ls)))
                                  )]
            [empty-prim ()  (empty-exp)]
            [cons-prim (elem exp) (cons (eval-exp elem env global-env)  (eval-exp exp env global-env))]
            [isList-prim (exp) (list? (eval-exp exp env global-env))]
            [first-prim (exp) (car (eval-exp exp env global-env))]
            [first-prim-l (exp)  (eval-exp (car (eval-exp exp env global-env)) env global-env)]
            [rest-prim (exp) (cond
                               [(equal? empty-exp (car (eval-exp exp env global-env))) (eopl:error 'apply-ref-env "list empty []")]
                               [(empty? (cdr (eval-exp exp env global-env))) (empty-exp)]
                               [else (cdr (eval-exp exp env global-env))]
                               )
                       ]
            [append-prim (exp1 exp2) (append (eval-exp exp1 env global-env) (eval-exp exp2 env global-env))]
            [else 'F])]


      ; Manejo de vectores
      [pri-exp (exp)
         (cases prim-vec exp
           [index-vec (exp index) (eval-exp (search (de-vector (eval-exp exp env global-env)) index) env global-env)]
           
           [update-vec (exp index elem) (apply-setref (cases expression exp
                                                                  [identifier-exp (id) id]
                                                                  [else (eopl:error 'eval-exp
                                                                                    "not valid expression ~s" exp)]
                                                                  )
                                                                (a-vector (replace (de-vector (eval-exp exp env global-env)) index elem)) env global-env)]
           [isVector-vec (exp) (f-vector? (eval-exp exp env global-env))]
           [new-vector (number) (a-vector (create-vector number))]
           )]


      [secuence-exp (exp list-exp) (sequence (eval-exp exp env global-env) list-exp env global-env)] 
      [if-exp (condition true-exp false-exp) (if (eval-exp condition env global-env)
                                                (eval-exp true-exp env global-env)
                                                (eval-exp false-exp env global-env)
                                                )]
      [cond-exp (list-condition list-true-exp false-exp) (eval-cond list-condition list-true-exp false-exp env global-env)]
      
      [app-exp (rator rands) (let (
                                   (proc (eval-exp rator env global-env))
                                   (args (eval-rands rands env global-env))
                                   )
                               (if (procval? proc)
                                   (apply-procedure proc args global-env)
                                   (eopl:error 'eval-exp
                                               "Attempt to apply non-procedure ~s" proc)))]
      [write-exp (exp) (write (eval-exp exp env global-env))]
      [writeln-exp (exp) (begin (write (eval-exp exp env global-env))
                                (newline))]
      [primapp-bin-exp (exp1 a-prim exp2) (eval-primapp-bin-exp a-prim (eval-exp exp1 env global-env) (eval-exp exp2 env global-env) env global-env)]
      [primapp-un-exp (prim exp) (eval-primapp-un-exp exp prim env global-env)]
      ;; Primitivas string
      [pri-str (exp) 
          (cases prim-str exp
            [concat-exp (exp1 exp2) (string-append (eval-exp exp1 env global-env) (eval-exp exp2 env global-env))]
            [length-exp (exp) (string-length (eval-exp exp env global-env))])]
      
      ;; Primitivas registros
      [pri-reg (exp)
               (cases prim-reg exp
                 [new-register (exp1 exp2 list-exp1 list-exp2) (a-register (append (list exp1) list-exp1) (append (list exp2) list-exp2))]
                 [get-register (exp key) (source-key key (car (de-register (eval-exp exp env global-env))) (cadr (de-register (eval-exp exp env global-env))) env global-env)]
                 [update-register (exp key value) (apply-setref (cases expression exp
                                                                  [identifier-exp (id) id]
                                                                  [else (eopl:error 'eval-exp
                                                                                    "not valid expression ~s" exp)]
                                                                  )
                                                                (update-key key value (car (de-register (eval-exp exp env global-env)))
                                                                            (cadr (de-register (eval-exp exp env global-env))) env global-env) env global-env)]    
                 [isRegister-register (exp) (register? (eval-exp exp env global-env))])] 
      [for-exp (identifier val arith-prim expr-bool body)
               (eval-for-exp identifier val arith-prim expr-bool body env global-env)]
      [while-exp (expr-bool body)
                 (eval-while-exp expr-bool body env global-env)]
      
      [else 'nada] 
      )))



; ===========================================================================================================

(define eval-unic-exp
  (lambda (exp)
    (cases optional-unic-exp exp
      [non-value-exp () 'non-value]
      [optional-exp (exp) exp])))

(define create-vector
  (lambda (n)
    (cond
      [(< n 1) (eopl:error 'create-vector "number elements invalid ~s" n)] 
      [(= n 1) (list (number-exp 0))]
      [else (append (list (number-exp 0)) (create-vector (- n 1)))]
      )))

(define is-list?
  (lambda (exp)
    (cond
      [(null? exp) #t]
      [(list? exp) #t]
      [(not (expression? exp)) #f]
      [else (cases expression exp
                  [list-exp (exp) #t]
                  [empty-exp () #t]
                  [pri-list (exp) (cases prim-list exp
                                    [empty-prim () #t]
                                    [cons-prim (elem exp) #t]
                                    [else #f])]
                  [else #f])])))


(define apply-list-prim
  (lambda (exp env global-env)
    [pri-list (exp) 
          (cases prim-list exp
            [isEmpty-prim (exp) (null? (eval-exp exp env global-env))]
            [empty-prim () '()] 
            [cons-prim (elem exp) (cons elem (eval-exp exp env global-env))]
            [isList-prim (exp) (list? (eval-exp exp env global-env))]
            [first-prim (exp)  (eval-exp (car (eval-exp exp env global-env)) env global-env)]
            [rest-prim (exp) (cond
                               [(equal? empty-exp (car (eval-exp exp env global-env))) (eopl:error 'apply-ref-env "list empty []")]
                               [(empty? (cdr (eval-exp exp env global-env))) '()]
                               [else (cdr (eval-exp exp env global-env))]
                               )
                       ]
            [append-prim (exp1 exp2) (append (eval-exp exp1 env global-env) (eval-exp exp2 env global-env))]
            [else 'F])]
    ))

(define is-vector?
  (lambda (exp)
    (cases expression exp
      [vector-exp (exp) #t]
      [pri-exp (exp) (cases prim-vec exp
                       [new-vector (n) #t]
                       [else #f])]
      [else #f])))

(define is-register?
  (lambda (exp env global-env)
    (cases expression exp
      [register-exp (exp1 exp2 list-exp1 list-exp2) #t]
      [pri-reg (reg) (cases prim-reg reg
                       [new-register (a1 a2 a3 a4) #t]
                       [else #f]
                        )]
      [identifier-exp (id) (is-register? (apply-env env id global-env) env global-env)]
      [else #f])))

(define source-key
  (lambda (key list-keys list-values env global-env)
    (cond
      [(empty? list-keys) (eopl:error source-key "Is not found ~s" key)]
      [(equal? key (car list-keys)) (eval-exp (car list-values) env global-env)] 
      [else (source-key key (cdr list-keys) (cdr list-values) env global-env)]
      )
    ))


(define update-key
  (lambda (key value list-keys list-values env global-env)
    (a-register list-keys (replace list-values (rib-find-position key list-keys) value))
    )) 

(define eval-for-exp
  (lambda (identifier val arith-prim expr-bool body env global-env)
    (letrec 
        ((iterador (direct-target (eval-exp val env global-env)))
         (vector-temp (make-vector 1))
         (env-for (extended-env (list identifier) vector-temp env (var-type))))
      (vector-set! vector-temp 0 iterador)
      (let
          ((condicion (eval-exp (exp-bool expr-bool) env-for global-env)))
                        
        (if condicion
            (begin
              (eval-exp body env-for global-env)
              (eval-primapp-un-exp (identifier-exp identifier) arith-prim env-for global-env)
              (aux-eval-for-exp identifier arith-prim expr-bool body env-for global-env))
            1)))))

                          
(define aux-eval-for-exp
  (lambda (identifier arith-prim expr-bool body env-for global-env)
    (letrec 
        ((condicion (eval-exp (exp-bool expr-bool) env-for global-env)))
    
      (if condicion
          (begin
            (eval-exp body env-for global-env)
            (eval-primapp-un-exp (identifier-exp identifier) arith-prim env-for global-env)
            (aux-eval-for-exp identifier arith-prim expr-bool body env-for global-env))
          1))))

(define eval-while-exp
  (lambda (expr-bool body env global-env)
      (let
          ((condicion (eval-exp (exp-bool expr-bool) env global-env)))
                        
        (if condicion
            (begin
              (eval-exp body env global-env)
              (eval-while-exp expr-bool body env global-env))
            1))))



(define eval-primapp-un-exp
  (lambda (exp prim env global-env)
    (cases expression exp
      [identifier-exp (id) (let (
                                 (id-type (find-definition-type id env global-env))
                                 )
                             (cases definition-type id-type
                               [var-type () (let ((val (apply-env env id global-env)))
                                              (apply-setref id (apply-un-primitive val prim) env global-env)
                                              )]
                               [const-type () (eopl:error 'eval-exp "Cannot modify const definition with identifier ~s" id)]
                               [unic-type () (if (equal? (apply-env env id global-env) 'non-value)
                                                 [eopl:error 'eval-exp "Definition ~s doesn't have been declared" id]
                                                 [eopl:error 'eval-exp "Definition ~s is already declared" id]
                                                                           )]
                               )
                             )]
      [else (apply-un-primitive (eval-exp exp env global-env) prim)]
      )
    ))


(define eval-primapp-bin-exp
  (lambda (a-prim exp1 exp2 env global-env)
     (cases arith-bin-prim a-prim
       [prim-integers (a-prim) (apply-bin-primitive  exp1 exp2 a-prim)]
       [prim-octals (a-prim) (apply-octal-bin-primitive (octal->list exp1) (octal->list exp2) a-prim)]
       )
    ))


(define list->octal
  (lambda (values)
    (a-octal 'x8 values)
    ))


(define octal->list
  (lambda (oc)
    (cases octal oc
      [a-octal (base values) values]
      )
    ))

(define-datatype octal octal?
  [a-octal (base symbol?)
           (values (list-of number?))]
  )

(define-datatype f-vector f-vector?
  [a-vector (values (list-of expression?))]
  )

(define-datatype register register?
  [a-register (keys (list-of symbol?))
              (values (list-of expression?))]
  )

(define de-register
  (lambda (reg)
    (cases register reg
      [a-register (keys values) (list keys values)]
      ))
  )
;; ------------------------

; funciones auxiliares para aplicar eval-exp a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env global-env)
    (map (lambda (x) (eval-rand x env global-env)) rands))
  )

(define eval-rand
  (lambda (rand env global-env)
    (cases expression rand
      [ref-var-exp (id) (indirect-target
                                [let
                                    ((ref (apply-ref-env env id global-env)))
                                  (cases target (primitive-deref ref)
                                    [direct-target (val) ref]
                                    [indirect-target (ref2) ref2])
                                  ]
                                )]
      [else (direct-target (eval-exp rand env global-env))]
      )
    )
  )

(define eval-def-exp
  (lambda (rand env global-env)
    (if (expression? rand) (direct-target (eval-exp rand env global-env)) 'non-value)
    )
  )

(define eval-def-exp-rands
  (lambda (rands env global-env)
    (map (lambda (n) (eval-def-exp n env global-env))rands)
    )
  )

(define eval-unic-exp-rand
  (lambda (rand env global-env)
    (cases optional-unic-exp rand
      [optional-exp (exp) (eval-def-exp exp env global-env)]
      [non-value-exp () 'non-value]
      )
    ))

(define eval-unic-exp-rands
  (lambda (rands env global-env)
    (map (lambda (n) (eval-unic-exp-rand n env global-env))rands)
    )
  )

;apply-primitive: <primitiva> <list-of-expression> -> numero

(define apply-bin-primitive
  (lambda (val1 val2 prim)
    (cases arith-bin-integers prim
      [add-prim () (+ val1 val2)]
      [sub-prim () (- val1 val2)]
      [mul-prim () (* val1 val2)]
      [mod-prim () (modulo val1 val2)]
      [div-prim () (/ val1 val2)]
    )))

(define apply-un-primitive
  (lambda (val prim)
    (cases arith-un-prim prim
      [inc-prim () (+ val 1)]
      [dec-prim () (- val 1)]
      [inc-octal-prim () (list->octal (increment-bignum (octal->list val) 8))]
      [dec-octal-prim () (list->octal (decrement-bignum (octal->list val) 8))]
      )
    ))

(define apply-octal-bin-primitive
  (lambda (oc1 oc2 a-prim)
    (cases arith-bin-octals a-prim
      [add-octal-prim () (list->octal (suma-bignum oc1 oc2 8))]
      [sub-octal-prim () (list->octal (resta-bignum oc1 oc2 8))]
      [mul-octal-prim () (list->octal (multiplicacion-bignum oc1 oc2 8))]
      )
    )
  )

(define apply-primitive
  (lambda (a prim b env global-env)
    (let (
          (x (if (expression? a) (eval-exp a env global-env) a))
          (y (if (expression? b) (eval-exp b env global-env) b))
          )
          (cases pred-prim prim
            [greater-prim () (> x y)]
            [greaterEqual-prim () (>= x y)]
            [less-prim () (< x y)]
            [lessEqual-prim () (<= x y)]
            [equal-prim () (equal? x y)]
            [notEqual-prim () (not (equal? x y))]
            )
          )))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))


(define apply-setref
  (lambda (id rhs-exp env global-env)
    (begin
      (setref! (apply-ref-env env id global-env)
               (if (expression? rhs-exp) (eval-exp rhs-exp env global-env)
                   rhs-exp))
      1)
    ))


; Para el sequence
(define sequence
  (lambda (exp list-exp env global-env)
    (cond
      [(empty? list-exp) exp]
      [(not (empty? list-exp)) (sequence (eval-exp (car list-exp) env global-env) (cdr list-exp) env global-env)])))


(define empty? 
  (lambda (list)
    (eq? list empty)))




; ======================================================= Datos =============================================

; Representacion BigNum para los octales.

(define zero
  (lambda ()
    empty
    )
  )

(define is-zero?
  (lambda (n)
    (if (list? n) (null? n)
        (zero? n))
  ))

(define successor-N
  (lambda (n N)
    (cond
      [(is-zero? n) (cons 1 (zero))]
      [(is-zero? (car n)) (cons 1 (cdr n))]
      [(equal? (- N 1) (car n)) (cons 0 (successor-N (cdr n) N))]
      [else (cons (+ 1 (car n)) (cdr n))]
      )
    )
  )

(define predecessor-N
  (lambda (n N)
    (cond
      [(is-zero? n) (zero)]
      [(is-zero? (car n)) (cons (- N 1) (predecessor-N (cdr n) N))]
      [(equal? '(1) n) (zero)]
      [(equal? 1 (car n)) (cons (zero) (cdr n) )]
      [else (cons (- (car n) 1) (cdr n))]
      )
    )
  )


(define suma-bignum
  (lambda (x y N)
    (if (is-zero? x) y
        (successor-N (suma-bignum (predecessor-N x N) y N) N)
        )
    )
  )

(define resta-bignum
  (lambda (x y N)
    (cond
      [(is-zero? x) y]
      [(is-zero? y) x]
      [else (resta-bignum (predecessor-N x N) (predecessor-N y N) N)]
      )
    )
  )

(define multiplicacion-bignum
  (lambda (x y N)
    (cond
      [(or (is-zero? x) (is-zero? y)) (zero)]
      [(suma-bignum (multiplicacion-bignum (predecessor-N x N) y N) y N)]
      )
    )
  )

(define increment-bignum
  (lambda (x N)
    (suma-bignum x (successor-N (zero) N) N) 
    ))

(define decrement-bignum
  (lambda (x N)
    (resta-bignum x (successor-N (zero) N) N)
    ))

(define to-bignum
  (lambda (n N)
    (cond
      [(null? n) (zero)]
      [(zero? n) empty]
      [else (successor-N (to-bignum (- n 1) N) N)]
      )
    )
  )

(define to-number
  (lambda (n N)
    (cond
      [(is-zero? n) 0]
      [(is-zero? (car n)) (*  (to-number (cdr n) N) N)]
      [else (+ (car n) (* (to-number (cdr n) N) N))]
      )
    )
  )

;; Operar listas
;(define list-prim
;  (lambda (list-expr env global-env)
;    (eval-exp (car list-expr) env global-env)))

;; Evaluar primitivas
(define eval-bool
  (lambda (exp env global-env)
    (cases expr-bool exp
      [compare-exp (a prim b) (apply-primitive (eval-exp a env global-env) prim  (eval-exp b env global-env) env global-env)]
      [oper-bin-exp (a op b) (apply-operate (eval-bool a env global-env) op (eval-bool b env global-env))]
      [oper-un-exp (un prim) (apply-un-exp un (eval-bool prim env global-env))]
      [bool-exp (bool-prim) (cases bool bool-prim
                              [true-exp () #t]
                              [false-exp () #f])]
      [else #f]
      )
    )) 

;; Evaluar cond
(define eval-cond
  (lambda (list-exp list-true-exp false-exp env global-env)
    (letrec
        (
         (check (lambda (list-exp list-true-exp false-exp env global-env)
                  (cond
                    [(empty? list-exp) (eval-exp false-exp env global-env)]
                    [(eval-exp (car list-exp) env global-env) (eval-exp (car list-true-exp) env global-env)]
                    [else (eval-cond (cdr list-exp) (cdr list-true-exp) false-exp env global-env)]
                    ))))
      (check list-exp list-true-exp false-exp env global-env)
      )))

; apply-oper-bin
(define apply-operate
  (lambda (a op b)
    (cases oper-bin-bool op
      [and-prim () (and a b)]
      [or-prim () (or a b)]
      [xor-prim () (and (not (and a b)) (or a b))]
      )))

; apply-un-exp -> negación de una comparación
(define apply-un-exp
  (lambda (un prim)
    (cases oper-un-bool un
      [not-prim () (not prim)]
      )))

; ================================================= Procedimientos ==========================================

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args global-env)
    (cases procval proc
      (closure (ids body env)
               (eval-exp body (create-extended-env ids args env (var-type)) global-env)))))

; ================================================ Ambientes ===================================================

;definición del tipo de dato ambiente
(define-datatype environment environment?
  [empty-env]
  [extended-env (syms (list-of scheme-value?))
                (vec vector?)
                (env environment?)
                (type definition-type?)
   ])

(define-datatype definition-type definition-type?
  [var-type]
  [const-type]
  [unic-type]
  )

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define create-empty-env  
  (lambda ()
    (empty-env)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define create-extended-env
  (lambda (syms vals env type)
    (extended-env syms (list->vector vals) env type)))

;rec-extended-env: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define rec-extended-env
  (lambda (proc-names idss bodies old-env)
    (let* (
           (len (length proc-names))
           (vec (make-vector len))
           (env (extended-env proc-names vec old-env (var-type)))
           )
      (for-each (lambda (pos ids body)
                  (vector-set! vec pos (direct-target (closure ids body env)))
                  )
                (iota len) idss bodies)
      env)
    ))

;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))
    ))

(define gen-num-list
  (lambda (n)
    (if (zero? n) (cons 0 empty)
        (cons n (gen-num-list (- n 1)))
        )
    ))

; función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env search-value global-env)
    (deref (apply-ref-env env search-value global-env)))
  )

(define apply-unic-env
  (lambda (env search-value global-env)
    (unic-deref (apply-ref-env env search-value global-env)))
  )

; Buscar un dato en el ambiente.
(define apply-ref-env
  (lambda (env search-value global-env)
    (cases environment env
      [empty-env () (cases environment global-env
                      [empty-env () (eopl:error 'apply-ref-env "No binding for ~s" search-value)]
                      [else (apply-ref-env global-env search-value (empty-env))]
                      )]
      [extended-env (syms vals env type) (let (
                                               (pos (list-find-position search-value syms))
                                               )
                                           (if (number? pos) (a-ref pos vals)
                                               (apply-ref-env env search-value global-env))
                                           )] 
      )
    )
  )

; Buscar el tipo de definicion de un dato.
(define find-definition-type
  (lambda (search-value env global-env)
    (cases environment env
      [empty-env () (cases environment global-env
                      [empty-env () (eopl:error 'apply-ref-env "No binding for ~s" search-value)]
                      [else (find-definition-type search-value global-env (empty-env))]
                      )]
      [extended-env (syms vals env type) (let (
                                               (pos (list-find-position search-value syms))
                                               )
                                           (if (number? pos) type
                                               (find-definition-type search-value env global-env))
                                           )]
      )
    ))

; ===================================== Referencias ==========================================
;rib-find-position

; Definicion del tipo de dato reference
(define-datatype reference reference?
  [a-ref (position integer?)
         (vec vector?)]
  )

(define setref!
  (lambda (ref val)
    (let
        ((ref1 (let ((un-tar (primitive-deref ref)))
                 (if (eqv? un-tar 'non-value) ref
                     (cases target un-tar
                       [direct-target (val) ref]
                       [indirect-target (ref2) ref2]
                       )
                     )
                 )))
      (primitive-setref! ref1 (direct-target val))
      )
    ))

; Establecer un nuevo valor a una referencia.
(define primitive-setref!
  (lambda (ref val)
    [cases reference ref
      (a-ref (pos vec) (vector-set! vec pos val))
      ]
    ))

(define unic-deref
  (lambda (ref)
    (let ((un-tar (primitive-deref ref)))
      (if (eqv? un-tar 'non-value) un-tar
        (cases target un-tar
          [direct-target (val) val]
          [indirect-target (ref2) (cases target (primitive-deref ref2)
                                    [direct-target (val) val]
                                    [indirect-target (ref3) (eopl:error 'deref
                                                                        "Illegal reference: ~s" ref2)]
                                    )]
          )))
    ))

(define deref
  (lambda (ref) 
    (let ((un-tar (primitive-deref ref)))
      (if (eqv? un-tar'non-value) (eopl:error 'deref "Definition ~s have not been assigned." ref)
        (cases target un-tar
          [direct-target (val) val]
          [indirect-target (ref2) (cases target (primitive-deref ref2)
                                    [direct-target (val) val]
                                    [indirect-target (ref3) (eopl:error 'deref
                                                                        "Illegal reference: ~s" ref2)]
                                    )]
          )))
    ))


(define primitive-deref
  (lambda (ref)
    (cases reference ref
      [a-ref (pos vect) (vector-ref vect pos)]
      )
    ))

; Paso por referencia.

(define-datatype target target?
  [direct-target (exp-val exp-val?)]
  [indirect-target (ref ref-direct-tar?)]
  )

; Debe retornar true para cualquiera de los valores expresados.
(define exp-val?
  (lambda (n)
    (or [boolean? n]
        [number? n]
        [procval? n]
        [string? n]
        [list? n]
        [symbol? n]
        [f-vector? n]
        [register? n]
        [octal? n]
        [cases expression n
          [empty-exp () #t]
          [else  #f]]
        )
    ))

(define ref-direct-tar?
  (lambda (n)
    (and [reference? n]
         [cases reference n
           (a-ref (pos vect) [cases target (vector-ref vect pos)
                               (direct-target (exp-val) #t)
                               (indirect-target (ref) #f) ; no debería suceder.
                               ])
           ])
    ))


; =============================================================================================================
; Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;; Busca un indice en espcifico de una lista 
(define search
  (lambda (list index)
    (letrec
        (
         (aux (lambda (list index i)
                (if (= i index) (car list) (aux (cdr list) index (+ i 1))) 
                ))
             )(aux list index 0)
      )))

(define de-vector
  (lambda (v)
    (cases f-vector v
      [a-vector (values) values]
      )))

(define replace 
  (lambda (list index elem)
    (letrec
        (
         (aux (lambda (list index elem i)
                (cond
                  [(empty? list) '()]
                  [(= i index) (cons elem (cdr list))]
                  [else (cons (car list) (aux (cdr list) index elem (+ i 1))) ]   
                  )
                ))
             )(aux list index elem 0)
      ))) 

(interpretador)






