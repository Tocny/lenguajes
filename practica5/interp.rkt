#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookup name env)
  (type-case DefrdSub env
    [mtSub () (error 'lookup "Variable libre: ~a" name)]  
    [aSub (id val rest)  
          (if (symbol=? id name)
              val 
              (lookup name rest))]))  

;; interp: CFWBAE DefrdSub-> CFWBAE-Value
;; Toma dos argumentos: Una expresión del tipo CFWBAE, ya sin azúcar sintáctica.
;; Un ambiente (DefrdSub) con las variables ya ligadas.


(define (interp expr env)
  (type-case CFWBAE expr
    [id (i) (lookup i env)]  
    [num (n) (numV n)]  
    [bool (b) (boolV b)]  
    [if (c t e)  
        (let ([c-val (interp c env)]) 
          (if (boolV? c-val)  
              (if (boolV-b c-val)  
                  (interp t env)
                  (interp e env))
              (error 'interp "La condición debe ser booleana")))]
    [op (f args)  
        (let ([arg-vals (map (λ (a) (interp a env)) args)])
          (if (andmap numV? arg-vals)  
          f
              (numV (apply f (map numV-n arg-vals))) 
              (error 'interp "Operación aplicada a no número")))]
    [fun (params body)  ;
         (closure params body env)]  
    [app (fun-expr arg-exprs)  
         (let ([fun-val (interp fun-expr env)]  ; Evalúa función
               [arg-vals (map (λ (a) (interp a env)) arg-exprs)])  ;
           (if (closure? fun-val)  
               (if (= (length (closure-params fun-val))  
                   (let* ([params (closure-params fun-val)]
                   [env0 (closure-env fun-val)]
                   [new-env (foldr (λ (p v acc) (aSub p v acc))
                       env0
                       params
                       arg-vals)])
                     (interp (closure-body fun-val) new-env)) 
                   (error 'interp "Número incorrecto de argumentos"))
               (error 'interp "Intento de aplicar no-función")))]))
