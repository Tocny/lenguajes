#lang plai
(require (file "./grammars.rkt"))


;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (match sexp
    [(? symbol?) (idS sexp)]               
    [(? number?) (numS sexp)]              
    [(? boolean?) (boolS sexp)]        
    [(list 'if cond then else)
     (iFS (parse cond) (parse then) (parse else))]
    [(list 'cond cases ...)
     (condS (parse-cond cases))]
    [(list 'with (list (list id expr) ...) body)
     (let ([bindings (map (λ (i e) (binding i (parse e))) id expr)])
       (if (check-duplicates (map binding-id bindings))
           (error 'parse "identificador duplicado en with: ~a" 
                  (check-duplicates (map binding-id bindings)))
           (withS bindings (parse body))))]
    [(list 'with* (list (list id expr) ...) body)
     (withS* (map (λ (i e) (binding i (parse e))) id expr) (parse body))]
    [(list 'fun (list params ...) body)
     (if (check-duplicates params)
         (error 'parse "parámetro duplicado en función: ~a" 
                (check-duplicates params))
         (funS params (parse body)))]
    [(list op args ...)
     (if (and (symbol? op) (operator? op))
         (opS (get-op op) (map parse args)) 
         (appS (parse op) (map parse args)))]
    [else (error 'parse "expresión no válida: ~a" sexp)]))
(define (parse-cond cases)
  (match cases
    [(list) (error 'parse "cond sin casos")]
    [(list (list 'else expr)) (list (else-cond (parse expr)))]
    [(list (list test expr) rest ...)
     (cons (condition (parse test) (parse expr)) (parse-cond rest))]
    [else (error 'parse "sintaxis de cond incorrecta")]))

;; operator?: symbol -> boolean
(define (operator? op)
  (member op '(+ - * / modulo expt add1 sub1 
               < <= = > >= not and or zero?)))

;; get-op: symbol -> procedure
(define (get-op op)
  (match op
    ['+ +] ['- -] ['* *] ['/ /]
    ['modulo modulo] ['expt expt]
    ['add1 add1] ['sub1 sub1]
    ['< <] ['<= <=] ['= =] ['> >] ['>= >=]
    ['not not] 
    ['and (λ (x y) (and x y))] 
    ['or (λ (x y) (or x y))]
    ['zero? zero?]
    [else (error 'parse "operador desconocido: ~a" op)]))