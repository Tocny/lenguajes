#lang plai

;; Practica 1.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador
;; -
;; -
;; -




;; 1. letras-repetidas? : String -> Boolean
(define (letras-repetidas? s)
  (let ([char (string->list s)]) ;creo una lista con los caracteres del string para facilitar la recursion
    (cond
      [(empty? char) #f] ; caso base, si la lista es vacia devuelve false
      [(member (first char) (rest char)) #t] ; si el primer elemento vuelve a aparecer, devuelve true
      [else (letras-repetidas? (list->string (rest char)))] ; si no, intenta con el segundo
      )))

;; 2. rotacion-de : (listof number) (listof number) -> boolean
(define (rotacion-de? a b)
  (and (= (length a) (length b)) ;verifica que sean del mismo tamaño
       (sublista? b (append a a)))) ; una lista es rotacion de otra, si es sublista de la primera concatenada consigo misma

;; AUX prefijo? : list list -> boolean
(define (prefijo? sub lst) ;funcion auxiliar para ver si una lista es prefijo de otra (si tiene sus mismos primeros elementos)
  (cond
    [(empty? sub) #t] ; base, la sublista se quedo sin elementos
    [(empty? lst) #f] ; base, si la lista grande es vacia, no puede tener prefijos (salvo el caso 1)
    [(equal? (first sub) (first lst)) ; si coinciden en los primeros elementos
     (prefijo? (rest sub) (rest lst))] ; checa con los siguientes
    [else #f])) ;devuelve false si dejan de coincidir

;; AUX sublista? : list list -> boolean
(define (sublista? sub lst) ;predicado que compruba si una lista es sublista de otra
  (cond
    [(empty? sub) #t] ; si la sublista es vacia si es sublista de otra
    [(empty? lst) #f] ; si la grande es vacia, no tiene sublistas mas que la vacia
    [(prefijo? sub lst) #t] ; verifica si la sublista es prefijo de la grande y devuelve directamente true
    [else ; si no entonces checa si, comprueba pero a partir del siguiente elemento de la grande
     (sublista? sub (rest lst))]))

;; 3. anagrama-profundo? : string string - boolean
(define (anagrama-profundo? s1 s2)
  (let ([l1 (sort (string->list s1) char<?)] ;hago listas de los strings para facilitar comparacion
        [l2 (sort (string->list s2) char<?)])
    (and (= (length l1) (length l2)) (prefijo? l1 l2)))) ; se verifica si tienen el mismo tamaño y despues a terminos practicos verifica si son iguales

;; 4. intercalar :  (listof number) -> (listof number)

;; 5. tipo-de-orden : (listof number) -> String

;; 6. dividir-prefijos : string -> (listof (pair string string))

;; 7. caracteres-unicos : string -> (listof char)

;; 8. zigzag-sum : (listof number) -> number

;; 9. generate-brackets : (listof number) -> number
;; Genera todas las combinaciones válidas de paréntesis balanceados para n pares
(define (generate-brackets n)
  (cond
    [(= n 0) '("")]        ; caso base: lista con cadena vacía
    [else
     (apply append
            (for/list ([k (in-range n)])
              (formar-p k (- n 1 k))))]))

;; formar-p : number number -> (listof string)
;; Genera todas las combinaciones de la forma (p)q donde p tiene k pares y q tiene m pares
(define (formar-p k m)
  (define p-combinations (generate-brackets k))
  (define q-combinations (generate-brackets m))
  (apply append
         (for/list ([p p-combinations])
           (for/list ([q q-combinations])
             (string-append "(" p ")" q)))))
