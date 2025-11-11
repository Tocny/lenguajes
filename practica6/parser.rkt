#lang plai
(require (file "./grammars.rkt"))
(require racket/match)
(require racket/list) ; para take, drop, index-of

;; ================================
;; parser.rkt — parse :: s-expr -> SCFBWAE
;; ================================

(define (parse-type t)
  (cond
    [(symbol? t)
     (case t
       [(number) (numberT)]
       [(boolean) (booleanT)]
       [else (error 'parse-type (format "Tipo desconocido: ~a" t))])]
    [(and (list? t) (member '-> t))
     (define idx (index-of t '->))
     (unless (and idx (< idx (sub1 (length t))))
       (error 'parse-type "Falta '->' o retorno en tipo de función"))
     (define lhs (take t idx))
     (define rhs (drop t (add1 idx)))
     (unless (= (length rhs) 1)
       (error 'parse-type "El tipo de retorno debe ser único"))
     (funT (append (map parse-type lhs) (list (parse-type (car rhs)))))]
    [else (error 'parse-type (format "Tipo inválido: ~a" t))]))

(define (parse-param p)
  (match p
    [(list x ': T) (param x (parse-type T))]
    [_ (error 'parse (format "Parámetro mal formado: ~a" p))]))

(define (parse-binding b)
  (match b
    [(list x ': T v) (binding x (parse-type T) (parse v))]
    [_ (error 'parse (format "Binding mal formado: ~a" b))]))

(define (parse-cond-cases cs)
  (map (lambda (c)
         (match c
           [(list 'else e) (else-cond (parse e))]
           [(list tst th)  (condition (parse tst) (parse th))]
           [_ (error 'parse (format "Caso cond mal formado: ~a" c))]))
       cs))

(define (boolean-symbol? s) (or (eq? s 'true) (eq? s 'false)))
(define (parse-bool s) (boolS (eq? s 'true)))

(define arith '(+ - * / modulo expt add1 sub1))
(define cmp   '(< <= = > >= zero?))
(define logi  '(and or not))

(define (parse sexp)
  (cond
    [(number? sexp) (numS sexp)]
    [(boolean? sexp) (boolS sexp)]
    [(and (symbol? sexp) (boolean-symbol? sexp)) (parse-bool sexp)]
    [(symbol? sexp) (idS sexp)]
    [(list? sexp)
     (match sexp
       [(list 'if c t e)          (iFS (parse c) (parse t) (parse e))]
       [(cons 'cond rst)          (condS (parse-cond-cases rst))]
       [(list 'with bs body)      (withS  (map parse-binding bs) (parse body))]
       [(list 'with* bs body)     (withS* (map parse-binding bs) (parse body))]
       [(list 'fun params ': rT body)  
        (funS (map parse-param params) (parse-type rT) (parse body))]
       [(cons op rs)
        (cond
          [(or (memq op arith) (memq op cmp) (memq op logi))
           (opS op (map parse rs))]
          [else
           (appS (parse op) (map parse rs))])]
       [_ (error 'parse "Expresión mal formada")])]
    [else (error 'parse (format "S-expr inválido: ~a" sexp))]))
