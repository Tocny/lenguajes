#lang plai

;; Practica 4.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

(require (file "./grammars.rkt"))

;; Función auxiliar duplicados.
(define (check-duplicate-params params)
  (define duplicate (check-duplicates params)) 
  (when duplicate
    (error (format "parser: parámetro definido dos veces: ~a" duplicate))))

;; parse : s-expression -> CFWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)] ;; Case: Num
    [(symbol? sexp) (id sexp)]  ;; Case: Id
    [(list? sexp) ;; Case: compound.
     (cond
       ;; Operaciones (Op)
       [(and (>= (length sexp) 2)
             (member (first sexp) '(+ - * / modulo expt add1 sub1)))
        (let ([op-symbol (first sexp)]
              [operands (rest sexp)])
          ;; Validamos aridad de unarios.
          (when (member op-symbol '(add1 sub1))
            (unless (= (length operands) 1)
              (error (format "parse: Aridad incorrecta en '~a" op-symbol))))
          (op (get-op-procedure op-symbol)
              (map parse operands)))]
       
       ;; if0
       [(and (eq? (first sexp) 'if0) (= (length sexp) 4))
        ;;Componemos el if
        (if0 (parse (second sexp))
             (parse (third sexp))
             (parse (fourth sexp)))]
       
       ;; fun
       [(and (eq? (first sexp) 'fun) (= (length sexp) 3))
        (let ([param-list (second sexp)] ;;parametros
              [body-expr (third sexp)])  ;;body
          (unless (and (list? param-list) (andmap symbol? param-list))
            (error "parse: Parámetros de fun deben ser símbolos"))
          
          ;; checamos si hay oparámetros duplicados.
          (check-duplicate-params param-list)
          ;;Componemos la función.
          (fun param-list (parse body-expr)))]
       
       ;; with (app + fun)
       [(and (eq? (first sexp) 'with) (>= (length sexp) 3))
        (parse-with (second sexp) (third sexp))] ;;usamos el auxiliar.
       
       ;; with* (app + fun)
       [(and (eq? (first sexp) 'with*) (>= (length sexp) 3))
        (parse-with* (second sexp) (third sexp))] ;;usamos el auxiliar.
       
       ;;App (default, n argumentos)
       [(>= (length sexp) 1)
        (let ([parsed-fun (parse (first sexp))])
          ;; Si el primer elemento es un número, es un error
          (when (num? parsed-fun)
            (error "parse: No se puede aplicar un número como función"))
   
          (app parsed-fun
               (parse-app-args (rest sexp))))]  ; ← NUEVA función para argumentos

       [else (error "Syntax Error: Expresión mal formada en parse")])]
    [else (error "Syntax Error: Expresión mal formada en parse")]))

;; Parser para: with -> app + fun
(define (parse-with bindings body)
  (let* ([binding-pairs (parse-bindings bindings)] ;;parseamos los bindings
         [params (map binding-id binding-pairs)]   ;;extraemos los ids de cada par
         [values (map binding-value binding-pairs)]);;extraemos los valores de cada par
    (app (fun params (parse body)) values))) ;; construimos una función equivalente con app y fun.

;; Parser para: with* -> with anidados
(define (parse-with* bindings body)
  (if (null? bindings) ;; si no hay bindings, parseamos el body
      (parse body)
      (let* ([first-binding (first bindings)];;primer binding
             [first-id (first first-binding)];;primer id
             [first-value (second first-binding)];primer valor
             [rest-bindings (rest bindings)]) ;;bindings restantes.
        ;;Construimos un with con lo anterior, lo anidamos con el resto y lo volvemos a parsear
        (parse `(with (,first-binding) 
                  (with* ,rest-bindings ,body))))))

;; Parser de los argumentos en app.
(define (parse-app-args args)
  (cond
    [(null? args) '()]  ; Caso base
    
    ;; Caso particular: listas de valores. 
    [(and (list? (first args)) ;;primer elemento lista
          (not (null? (first args))) ;; esa lista no es vacía
          (not (symbol? (first (first args)))))  ;; y su primer elemento no es un simbolo.
     (append (map parse (first args))            ;; Parseamos cada elemento y hacemos recursión.
             (parse-app-args (rest args)))]
    
    ;; Caso general, se parsea todo.
    [else (cons (parse (first args))
                (parse-app-args (rest args)))]))


;;Operaciones.
(define (get-op-procedure op-symbol)
  (cond
    [(eq? op-symbol '+) +]
    [(eq? op-symbol '-) -]
    [(eq? op-symbol '*) *]
    [(eq? op-symbol '/) /]
    [(eq? op-symbol 'modulo) modulo]
    [(eq? op-symbol 'expt) expt]
    [(eq? op-symbol 'add1) (lambda (x) (+ x 1))]
    [(eq? op-symbol 'sub1) (lambda (x) (- x 1))]
    [else (error "Syntax Error: Operador no válido")]))

(define (parse-bindings binding-list)
  ;;verificamos que sea una lista de listas.
  (unless (and (list? binding-list)
               (andmap list? binding-list)
               (andmap (lambda (b) (= (length b) 2)) binding-list))
    (error "Syntax Error: Estructura de bindings inválida"))

  ;; sacamos los ids
  (define ids (map first binding-list))
  ;; Checamos si hay duplicados.
  (check-duplicate-params ids)
  ;; parseamos cada binding
  (map (lambda (binding-pair)
         (binding (first binding-pair) 
                  (parse (second binding-pair))))
       binding-list))
