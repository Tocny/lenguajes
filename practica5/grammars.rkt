#lang plai

(define-type Binding
  [binding (id symbol?) (value SCFWBAE?)])
(define-type Condition
  [condition (test-expr SCFWBAE?) (then-expr SCFWBAE?)]
  [else-cond (else-expr SCFWBAE?)])

;; Definición del tipo SCFWBAE
;; SCFWBAE: Sintaxis concreta (con azúcar sintáctica)
(define-type SCFWBAE
  [idS    (i symbol?)]
  [numS   (n number?)]
  [boolS  (b boolean?)]
  [iFS    (cond SCFWBAE?) (then SCFWBAE?) (else SCFWBAE?)]
  [opS    (f procedure?) (args (listof SCFWBAE?))]
  [condS  (cases (listof Condition?))]
  [withS  (bindings (listof binding?)) (body SCFWBAE?)]
  [withS* (bindings (listof binding?)) (body SCFWBAE?)]
  [funS   (params (listof symbol?)) (body SCFWBAE?)]
  [appS   (fun SCFWBAE?) (args (listof SCFWBAE?))])


;; Definición del tipo CFWBAE
;; CFWBAE: Sintaxis abstracta (sin azúcar sintáctica)
(define-type CFWBAE
  [id   (i symbol?)]
  [num  (n number?)]
  [bool (b boolean?)]
  [iF   (condicion CFWBAE?) (then CFWBAE?) (else CFWBAE?)]
  [op   (f procedure?) (args (listof CFWBAE?))]
  [fun  (params (listof symbol?)) (body CFWBAE?)]
  [app  (fun CFWBAE?) (args (listof CFWBAE?))])

;; Data-type que representa un caché de sustituciones
;;Implementa una pila de sustituciones (environment stack):
;; mtSub: Ambiente vacío (base)
;; aSub: Agrega un nuevo binding al ambiente existente


(define-type DefrdSub
  [mtSub]
  [aSub  (name symbol?) (value CFWBAE-Value?) (ds DefrdSub?)])

;;Data-type que representa la sintaxis abstracta de CFWBAE-Value
;; CFWBAE-Value: Valores producidos por el intérprete
;; Implementa funciones de primera clase con ámbito estático

(define-type CFWBAE-Value
  [closure  (param (listof symbol?)) (body CFWBAE?) (env DefrdSub?)]
  [numV     (n number?)]
  [boolV    (b boolean?)])
