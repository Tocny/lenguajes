#lang plai

;; ================================
;; grammars.rkt — Tipos y AST de Typed-CFBWAE
;; ================================

;; Tipo de datos para tipos del lenguaje
(define-type Type
  [numberT]
  [booleanT]
  ;; Lista de tipos [t1 ... tn r], donde r es el tipo de retorno
  [funT (params (listof Type?))])

;; Contexto de tipos (Γ)
(define-type Type-Context
  [phi]
  [gamma (id symbol?) (tipo Type?) (rest Type-Context?)])

;; Enlaces para with / with*
(define-type Binding
  [binding (id symbol?) (tipo Type?) (value SCFBWAE?)])

;; Parámetros tipados de funciones
(define-type Param
  [param (param symbol?) (tipo Type?)])

;; Casos de cond
(define-type Condition
  [condition (test-expr SCFBWAE?) (then-expr SCFBWAE?)]
  [else-cond (else-expr SCFBWAE?)])

;; AST con azúcar sintáctica
(define-type SCFBWAE
  [idS   (i symbol?)]
  [numS  (n number?)]
  [boolS (b boolean?)]
  [iFS   (condicion SCFBWAE?) (then SCFBWAE?) (else SCFBWAE?)]
  [opS   (op symbol?) (args (listof SCFBWAE?))]
  [condS (cases (listof Condition?))]
  [withS  (bindings (listof Binding?)) (body SCFBWAE?)]
  [withS* (bindings (listof Binding?)) (body SCFBWAE?)]
  [funS  (params (listof Param?)) (rType Type?) (body SCFBWAE?)]
  [appS  (fun SCFBWAE?) (args (listof SCFBWAE?))])

;; Helpers de tipos
(define (type-equal? t1 t2)
  (type-case Type t1
    [numberT () (numberT? t2)]
    [booleanT () (booleanT? t2)]
    [funT (params1)
      (and (funT? t2)
           (let ([params2 (funT-params t2)])
             (and (= (length params1) (length params2))
                  (andmap (lambda (p1 p2) (type-equal? p1 p2)) params1 params2))))]))

(define (lookup-type x ctx)
  (type-case Type-Context ctx
    [phi () (error 'typeof (format "Identificador no declarado: ~a" x))]
    [gamma (id tipo rest)
      (if (symbol=? x id) tipo (lookup-type x rest))]))

(define (andmap f . lists)
  (cond [(null? (car lists)) #t]
        [else (and (apply f (map car lists))
                   (apply andmap f (map cdr lists)))]))


