#lang plai
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
