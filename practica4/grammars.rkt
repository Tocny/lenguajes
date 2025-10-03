#lang plai

;; Practica 3.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

;; Definición del tipo Binding
(define-type Binding
  [binding (id symbol?) (value FWAE?)])

;; Definición del tipo WAE
(define-type FWAE
  [id    (i symbol?)]
  [num   (n number?)]
  [op    (f procedure?) (args (listof FWAE?))]
  [with  (bindings (listof binding?)) (body FWAE?)]
  [with* (bindings (listof binding?)) (body FWAE?)])
