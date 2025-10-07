#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

#| Pruebas EXCLUSIVAS de parse |#
(printf "___________________________________________________________________________________________________________________________\n")
(printf "PRUEBAS EXCLUSIVAS DEL PARSER\n")
(printf "___________________________________________________________________________________________________________________________\n\n")

;; Prueba 1: Error de parámetro duplicado en fun
(test/exn (parse '{fun {x y x} {+ x {+ y z}}})
          "parse: El símbolo 'x está declarado más de una vez")

;; Prueba 2: Aplicación simple
(test (parse '{{fun {x y} {+ x y}} 10 8})
      (app (fun '(x y) (op + (list (id 'x) (id 'y)))) (list (num 10) (num 8))))

;; Prueba 3: with con referencia a variable no definida (esto es LEGAL en parsing)
(test (parse '{with {{x 1} {y x} {z 3}} x})
      (app (fun '(x y z) (id 'x)) (list (num 1) (id 'x) (num 3))))

;; Prueba 4: with anidados complejos
(test (parse '{with {{x 1} {y 2}}
                    {with {{z 4} {w 5}}
                          {with {{f {fun {x} x}}}
                                {f 3}}}})
      (app (fun '(x y)
                (app (fun '(z w)
                          (app (fun '(f)
                                    (app (id 'f) (list (num 3))))
                               (list (fun '(x) (id 'x)))))
                     (list (num 4) (num 5))))
           (list (num 1) (num 2))))

;; Prueba 5: Función con operaciones complejas
(test (parse '{{fun {x y} {+ {* x y} x}} 3 4})
      (app (fun '(x y) (op + (list (op * (list (id 'x) (id 'y))) (id 'x)))) 
           (list (num 3) (num 4))))

;; Prueba 6: if0 (NUEVO en CFWAE)
(test (parse '{if0 0 10 20})
      (if0 (num 0) (num 10) (num 20)))

;; Prueba 7: with* (debe desgararse)
(test (app? (parse '{with* {{x 1} {y 2}} {+ x y}}))
      #t)

(printf "✅ TODAS LAS PRUEBAS DEL PARSER COMPLETADAS CORRECTAMENTE\n")