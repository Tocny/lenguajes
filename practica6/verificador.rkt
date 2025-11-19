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
           (for-each (lambda (arg arg-type);checamos que todos los argumento sean numeros.
                       (unless (numberT? arg-type)
                         (error 'typeof
                                (format "Error in parameter ~a\n Expected type: (numberT)\nGiven type: (booleanT)" arg)
                                )))
                     args arg-types)
           (numberT)]

          ;; Comparadores (returns booleanT)
          [(< <= = > >= zero?)
           (for-each (lambda (arg arg-type) ;checamos que todos los argumentos sean números.
                       (unless (numberT? arg-type)
                         (error 'typeof
                                (format "Error in parameter ~a\n Expected type: (booleanT)\nGiven type: (booleanT)" arg)
                                )))
                     args arg-types)
           (booleanT)]
          
          ;; Operaciones lógicas (returns booleanT)
          [(and or not)
           (for-each (lambda (arg arg-type) ;checamos que todos los argumentos sean booleanos.
                       (unless (booleanT? arg-type)
                         (error 'typeof (format "Error in parameter ~a\nExpected type: (booleanT)\nGiven type: (numberT)" arg))))
                     args arg-types)
           (booleanT)]
          
          [else (error 'typeof (format "Operación desconocida: ~a" op))]))]
    
    ;; Condicional: IF
    [iFS (condition then-expr else-expr)
      (let ([cond-type (typeof condition context)] ;tipo del if
            [then-type (typeof then-expr context)] ;tipo del then
            [else-type (typeof else-expr context)]);tipo del else
        ;; La condición debe ser booleana
        (unless (booleanT? cond-type)
          (error 'if (string-append "Type error\nConditional's type must be a boolean\nGiven: " (format "~a" cond-type))))

        ;; Verificar que el then y el else tengan sean del mismo tipo
        (unless (type-equal? then-type else-type)
          (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr"))
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
                                  (error 'typeof "Type error\nconditionals must have same type in then-expr and else-expr"))
                                (set! result-type else-type)))]))
                cases)
          result-type))]
    
    [funS (params rType body)
      (let ([extended-ctx (extend-context-with-params params context)])
        (let ([body-type (typeof body extended-ctx)])
          (unless (type-equal? body-type rType)
            (error 'typeof (format "El cuerpo de la función tiene tipo ~a pero se declaró ~a" 
                                   body-type rType)))
          (funT (append (map (lambda (p) (param-tipo p)) params) 
                        (list rType)))))]
    
    [withS (bindings body)
      (for-each (lambda (b)
                  (let ([declared-type (binding-tipo b)]
                        [value-type (typeof (binding-value b) context)])
                    (unless (type-equal? declared-type value-type)
                      (error 'typeof (format "Variable ~a: se declaró ~a pero tiene tipo ~a"
                                             (binding-id b) declared-type value-type)))))
                bindings)
      (let ([new-context (extend-context-with-bindings bindings context)])
        (typeof body new-context))]
    
    [withS* (bindings body)
      (let ([final-context 
             (foldl (lambda (b ctx)
                      (let ([declared-type (binding-tipo b)]
                            [value-type (typeof (binding-value b) ctx)])
                        (unless (type-equal? declared-type value-type)
                          (error 'typeof (format "Variable ~a: se declaró ~a pero tiene tipo ~a"
                                                 (binding-id b) declared-type value-type)))
                        (gamma (binding-id b) declared-type ctx)))
                    context
                    bindings)])
        (typeof body final-context))]
    
    [appS (fun-expr args)
          (let* ([fun-type (typeof fun-expr context)])
            (unless (funT? fun-type)
              (error 'typeof
                     (format "Se intentó aplicar algo que no es función. Tipo encontrado: ~a"
                     fun-type)))
            
            (define param+ret-types (funT-params fun-type))

            (define return-type (last param+ret-types))

            (define param-types (take param+ret-types (sub1 (length param+ret-types))))

            (unless (= (length args) (length param-types))
              (error 'typeof
                     (format "Número incorrecto de argumentos. Esperados: ~a, dados: ~a"
                             (length param-types) (length args))))

            (for-each
             (lambda (arg param-type)
               (let ([arg-type (typeof arg context)])
                 (unless (type-equal? arg-type param-type)
                   (error 'typeof
                          (format "Tipo incorrecto de argumento.\nEsperado: ~a\nObtenido: ~a"
                                  param-type arg-type)))))
             args param-types)
           return-type)]))


;Auxiliar que busca el primer elemento en una lista ue cumple una condición.
(define (findf condition lst)
  (cond [(null? lst) #f]
        [(condition (car lst)) (car lst)]
        [else (findf condition (cdr lst))]))

;; Auxilar para regresar el contexto de la lista de parametros
(define (extend-context-with-params params ctx)
  (foldl (lambda (p context)
           (gamma (param-param p) (param-tipo p) context))
         ctx
         params))

;; Auxiliar para regresar el contexto con una lista para with
(define (extend-context-with-bindings bindings ctx)
  (foldl (lambda (b context)
           (gamma (binding-id b) (binding-tipo b) context))
         ctx
         bindings))


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
