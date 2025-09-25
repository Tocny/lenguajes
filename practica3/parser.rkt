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
        (op (get-op-procedure (first sexp))
            (map parse (rest sexp)))]
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
  (map (lambda (binding-pair)
         (if (and (list? binding-pair) (= (length binding-pair) 2))
             (binding (first binding-pair) (parse (second binding-pair)))
             (error "Syntax Error: Invalid id assignment ")))
       binding-list))