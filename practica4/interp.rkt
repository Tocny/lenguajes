#lang plai
;; Practica 4.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; lookup.
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () 
           (error (format "lookup: Hay un identificador libre: ~a" name))]
    [aSub (n v rest-ds)
          (if (symbol=? n name)
              v
              (lookup name rest-ds))]))

;; interp : CFWAE DefrdSub -> CFWAE-Value
(define (interp expr ds)
  (type-case CFWAE expr
    [id (i) 
        (lookup i ds)]
    
    [num (n) 
         (numV n)]
    
    [op (f args)
        (let* ([vals (map (λ (a) (interp a ds)) args)]
               [nums (map (λ (v)
                            (type-case CFWAE-Value v
                              [numV (n) n]
                              [else (error "interp: se esperaba número en operación")]))
                          vals)]
               [res  (apply f nums)])
          (numV res))]
    
    [if0 (condition then-expr else-expr)
         (let ([cv (interp condition ds)])                             
           (type-case CFWAE-Value cv
             [numV (n)
                   (if (zero? n)
                       (interp then-expr ds)
                       (interp else-expr ds))]
             [else (error "interp: Símbolo no esperado la condicional de if0, no es un número")]))]
    
    [fun (params body)
         (closure params body ds)]
    
    [app (fun-expr arg-exprs)
         (let ([fv (interp fun-expr ds)])
           (type-case CFWAE-Value fv
             [closure (params body fds)
                      (let ([vals (map (λ (a) (interp a ds)) arg-exprs)])
                        (unless (= (length params) (length vals))
                          (error "parser: La cardinalidad de los argumentos difiere de la aridad de la función"))
                        (define new-ds
                          (extend-ds params vals fds)
                         )
                        (interp body new-ds))]
             [else (error "interp: intento de aplicar un valor que no es función")]))]))

(define (extend-ds params vals base-ds)
  (foldr (λ (p v acc) (aSub p v acc)) base-ds params vals)) 
