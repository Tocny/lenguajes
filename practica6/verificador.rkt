#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; typeof :: SCFBWAE Type-Context -> Type
(define (typeof sexpr context)
  (type-case SCFBWAE sexpr
    
    ;; Id
    [idS (i)
      (lookup-type i context)]

    ; numero.
    [numS (n)
      (numberT)]

    ;boolean.
    [boolS (b)
      (booleanT)]
    
    ;; operaciones
    [opS (op args)
      (let ([arg-types (map (lambda (arg) (typeof arg context)) args)])
        (case op
          ;; Operaciones aritméticas (returns numberT)
          [(+ - * / modulo expt add1 sub1)
           (for-each (lambda (arg-type);checamos que todos los argumento sean numeros.
                       (unless (numberT? arg-type)
                         (error 'typeof (format "Operador ~a espera numberT, obtuovo: ~a" op arg-type))))
                     arg-types)
           (numberT)]
          
          ;; Comparadores (returns booleanT)
          [(< <= = > >= zero?)
           (for-each (lambda (arg-type) ;checamos que todos los argumentos sean números.
                       (unless (numberT? arg-type)
                         (error 'typeof (format "Operación ~a es numberT, obtuvo: ~a" op arg-type))))
                     arg-types)
           (booleanT)]
          
          ;; Operaciones lógicas (returns booleanT)
          [(and or not)
           (for-each (lambda (arg-type) ;checamos que todos los argumentos sean booleanos.
                       (unless (booleanT? arg-type)
                         (error 'typeof (format "Operación ~a es booleanT, obtuvo: ~a" op arg-type))))
                     arg-types)
           (booleanT)]
          
          [else (error 'typeof (format "Operación desconocida: ~a" op))]))]
    
    ;; Condicional: IF
    [iFS (condition then-expr else-expr)
      (let ([cond-type (typeof condition context)] ;tipo del if
            [then-type (typeof then-expr context)] ;tipo del then
            [else-type (typeof else-expr context)]);tipo del else
        ;; La condición debe ser booleana
        (unless (booleanT? cond-type)
          (error 'typeof "La condición del if debe ser booleana"))
        ;; Verificar que el then y el else tengan sean del mismo tipo
        (unless (type-equal? then-type else-type)
          (error 'typeof "then y else deben tener el mismo tipo"))
        then-type)]
    
    ;; Condicional: COND 
    [condS (cases)
      (let ([else-case (findf else-cond? cases)]); sacamos el else.
        ;; Si no existe, aventamos el error.
        (unless else-case
          (error 'typeof "Cond debe tener una cláusula else"))
        
        ;; Verificamos que todas las condiciones sean del mismo tipo
        (let ([result-type #f]) ; resultado de tipo #f (porque aún no sabemos el tipo concreto)
          (for-each (lambda (c) ; recorremos las cond
                      (type-case Condition c
                        [condition (test then) ;construimos la condición del case
                          (let ([test-type (typeof test context)]
                                [then-type (typeof then context)])
                            (unless (booleanT? test-type); si el tipo de la condición no es booleano.
                              (error 'typeof "La condición del cond debe ser booleana"))
                            (if result-type
                                (unless (type-equal? result-type then-type) ;si se rompe la homogeneidad de las condiciones
                                  (error 'typeof "Todas las condiciones del cond deben ser del mismo tipo"))
                                (set! result-type then-type)))]
                        [else-cond (else-expr)
                          (let ([else-type (typeof else-expr context)])
                            (if result-type
                                (unless (type-equal? result-type else-type) ; si el caso else no es del mismo tipo que el resultado.
                                  (error 'typeof "El else debe tener el mismo tipo que las demás"))
                                (set! result-type else-type)))]))
                cases)
          result-type))]
    
    [funS (params rType body)
      (error 'typeof "2.2 c) fun")]
    
    [withS (bindings body)
      (error 'typeof "2.2 d) with")]
    
    [withS* (bindings body)
      (error 'typeof "2.2 d) with*")]
    
    [appS (fun-expr args)
      (error 'typeof "2.2 e) app")]))


;Auxiliar que busca el primer elemento en una lista ue cumple una condición.
(define (findf condition lst)
  (cond [(null? lst) #f]
        [(condition (car lst)) (car lst)]
        [else (findf condition (cdr lst))]))

;; pruebas:
(printf "~a: ~a\n" '{+ 1 2} (typeof (parse '{+ 1 2}) (phi)))
(printf "~a: ~a\n" '{- 5 3} (typeof (parse '{- 5 3}) (phi)))
(printf "~a: ~a\n" '{add1 5} (typeof (parse '{add1 5}) (phi)))

(printf "~a: ~a\n" '{< 1 2} (typeof (parse '{< 1 2}) (phi)))
(printf "~a: ~a\n" '{= 3 3} (typeof (parse '{= 3 3}) (phi)))
(printf "~a: ~a\n" '{zero? 0} (typeof (parse '{zero? 0}) (phi)))

(printf "~a: ~a\n" '{and true false} (typeof (parse '{and true false}) (phi)))
(printf "~a: ~a\n" '{or true false} (typeof (parse '{or true false}) (phi)))
(printf "~a: ~a\n" '{not true} (typeof (parse '{not true}) (phi)))

(printf "~a: ~a\n" '{if true 1 2} (typeof (parse '{if true 1 2}) (phi)))
(printf "~a: ~a\n" '{if false true false} (typeof (parse '{if false true false}) (phi)))
(printf "~a: ~a\n" '{cond [true 1] [else 2]} (typeof (parse '{cond [true 1] [else 2]}) (phi)))
(printf "~a: ~a\n" '{cond [false 2] [true 1] [else 3]} (typeof (parse '{cond [false 2] [true 1] [else 3]}) (phi)))
