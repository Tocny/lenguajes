#lang plai

;; Practica 4.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value CFWAE?)])

;; Definición del tipo CFWAE
(define-type CFWAE
  [id    (i symbol?)]
  [num   (n number?)]
  [op    (f procedure?) (args (listof CFWAE?))]
  [if0   (condition CFWAE?) (then CFWAE?) (else CFWAE?)]
  [fun   (params (listof symbol?)) (body CFWAE?)]
  [app   (fun CFWAE?) (args (listof CFWAE?))])

;; Definición del tipo CFWAE-Value (para ejr 3, no se sabe commo funciona.)
(define-type CFWAE-Value
  [numV     (n number?)]
  [closure (params (listof symbol?)) (body CFWAE?) (ds DefrdSub?)])

;; Definición del tipo DefrdSub (caché de sustituciones)
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value CFWAE-Value?) (ds DefrdSub?)])
