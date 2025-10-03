#lang plai

;; Practica 3.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

(require (file "./grammars.rkt"))

;; parse : s-expression - > FWAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (cond
       [(and (>= (length sexp) 2)
             (member (first sexp) '(+ - * / = modulo expt add1 sub1)))
        (let ([op-symbol (first sexp)]
              [operands (rest sexp)])
          ;; Validar aridad para operadores unarios
          (when (member op-symbol '(add1 sub1))
            (unless (= (length operands) 1)
              (error (format "parser: Aridad incorrecta en la función '~a" op-symbol))))
   
          (op (get-op-procedure op-symbol)
              (map parse operands)))]
       [(and (>= (length sexp) 3)
             (eq? (first sexp) 'with))
        (with (parse-bindings (second sexp))
              (parse (third sexp)))]
       [(and (>= (length sexp) 3)
             (eq? (first sexp) 'with*))
        (with* (parse-bindings (second sexp))
               (parse (third sexp)))]
       [else (error "Syntax Error: Bad sexp expression")])]
    [else (error "Syntax Error: Bad sexp expression")]))

(define (get-op-procedure op-symbol)
  (cond
    [(eq? op-symbol '+) +]
    [(eq? op-symbol '-) -]
    [(eq? op-symbol '*) *]
    [(eq? op-symbol '/) /]
    [(eq? op-symbol '=) =]
    [(eq? op-symbol 'modulo) modulo]
    [(eq? op-symbol 'expt) expt]
    [(eq? op-symbol 'add1) add1]
    [(eq? op-symbol 'sub1) sub1]
    [else (error "Syntax Error: Not valid operator")]))

(define (parse-bindings binding-list)
  ;; Verificar que binding-list sea una lista de listas
  (unless (and (list? binding-list)
               (andmap list? binding-list)
               (andmap (lambda (b) (= (length b) 2)) binding-list))
    (error "Syntax Error: Invalid binding structure"))
  
  ;; Extraer IDs y verificar duplicados
  (define ids (map first binding-list))
  (define duplicate (check-duplicates ids))
  (when duplicate
    (error (format "parse: El símbolo '~a está declarado más de una vez" duplicate)))
  
  ;; Parsear cada binding
  (map (lambda (binding-pair)
         (binding (first binding-pair) 
                  (parse (second binding-pair))))
       binding-list))