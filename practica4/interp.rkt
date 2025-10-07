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
           (error (format "interp: hay un identificador libre: ~a" name))]
    [aSub (n v rest-ds)
          (if (symbol=? n name)
              v
              (lookup name rest-ds))]))

;; interp : CFWAE DefrdSub -> CFWAE-Value
(define (interp expr ds)
  (type-case CFWAE expr
    [id (i) 
        "lol"]
    
    [num (n) 
         "lol"]
    
    [op (f args)
        "lol"]
    
    [if0 (condition then-expr else-expr)
         "lol"]
    
    [fun (params body)
         "lol"]
    
    [app (fun-expr arg-exprs)
         "lol"]))
