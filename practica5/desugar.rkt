#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

(define (desugar expr)

  (type-case SCFWBAE expr
  ;Estas construcciones no tienen que desazucararse, solo se convierten directamente:
    [idS (i) (id i)]  
    [numS (n) (num n)]  
    [boolS (b) (bool b)]  
    [iFS (c t e)  
     (if (desugar c) (desugar t) (desugar e))]
    [opS (f args)  
     (op f (map desugar args))]
    [condS (cases)  
           (desugar-cond cases)]
    [withS (bindings body) 
           (let ([ids (map binding-id bindings)]
                 [vals (map (λ (b) (desugar (binding-value b))) bindings)])
             (app (fun ids (desugar body)) vals))]
    [withS* (bindings body)  
            (if (null? bindings)
                (desugar body)
                (let ([first-binding (first bindings)]
                      [rest-bindings (rest bindings)])
                  (app (fun (list (binding-id first-binding))
                       (list (desugar (binding-value first-binding))))
                       (desugar (withS* rest-bindings body)))))]
    [funS (params body)  
          (fun params (desugar body))]
    [appS (fun args) 
          (app (desugar fun) (map desugar args))]))
