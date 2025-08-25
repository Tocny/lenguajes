#lang plai

;; Practica 1.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador
;; - Valencia Pérez Guillermo Emanuel
;; - Rubio Resendiz Marco Antonio
;; - Sautto Ramirez Seldon 321084163


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
(define (intercalar a b)
  (cond
    [(empty? a) b] ; caso base: a vacia
    [(empty? b) a] ; caso base: b vacia
    [else (cons (first a) ; toma el primero de a
                (cons (first b) ; toma el primero de b
                      (intercalar (rest a) (rest b))))])) ; recursion con los restos

;; 5. tipo-de-orden : (listof number) -> String
(define (tipo-de-orden lst)
  (cond
    ;; Verificamos que sea ascendente o descendente (se amortiguan casos base de ambos auxiliares).
    [(and (ascendente? lst) (not (descendente? lst))) "ascendente"] 
    [(and (descendente? lst) (not (ascendente? lst))) "descendente"]
    [else "no ordenada"]))


;; Auxiliar que revisa si es estrictamente ascendente
(define (ascendente? lst)
  (cond
    [(or (empty? lst) (empty? (rest lst))) #t] ;; Base: si es vacía o solo tiene un elemento (por vacuidad verdadero).
    [(>= (first lst) (second lst)) #f] ;; Falso si el primero es mayor o igual que el segundo (desciende en un paso).
    [else (ascendente? (rest lst))]))  ;; No se falseó, entonces verificará si sigue ascendiendo (recursión).

;; Auxiliar que revisa si es estrictamente descendente
(define (descendente? lst)
  (cond
    [(or (empty? lst) (empty? (rest lst))) #t] ;; Base: si es vacía o solo tiene un elemento (por vacuidad verdadero).
    [(<= (first lst) (second lst)) #f] ;; Falso si el primero es menor o igual que el segundo (asciende en un paso).
    [else (descendente? (rest lst))])) ;; No se falseó, entonces verificará si sigue descendiendo (recursión).


;; 6. dividir-prefijos : string -> (listof (pair string string))
(define (dividir-prefijos s)
  (if (<= (string-length s) 1) ;;verifica si la longitud de s es menor que 1
      '() ;; si, entonces no puede procesarla para dividirla.
      (pares s ""))) ;; no, entonces deberá generar los pares.

;; Auxiliar para generar pares recursivamente.
(define (pares s acumulado)
  (if (>= (string-length acumulado) (- (string-length s) 1))
      '()  ; Base: si el acumulado está a un caracter de ser la cadena completa, se detiene.
      (let* ([prefijo (string-append acumulado (substring s (string-length acumulado) (+ (string-length acumulado) 1)))]  ;; agrega el siguiente caracter de s al acumulado.
             [resto (substring s (+ (string-length acumulado) 1))] ;; toma el restante que aun no ha sido procesado
             [par-actual (cons prefijo resto)]) ;; construye el par del prefijo con el restante.
        (cons par-actual (pares s prefijo))))) ;; construye el par del recien construido con una recursión para seguir procesando a s.

;; 7. caracteres-unicos : string -> (listof char)
(define (caracteres-unicos s)
  (let ([char (string->list s)]) ; convertimos el string a una lista de chars
    (let rec ([xs char] [vistos '()]) ; creamos lista para acumular los vistos y funcion recursiva
      (cond
        [(empty? xs) '()] ; caso base, sin mas caracteres
        [(member (first xs) vistos) ; si ya lo vimos, lo saltamos
         (rec (rest xs) vistos)]
        [else (cons (first xs) (rec (rest xs) (cons (first xs) vistos)))])))) ; si es nuevo, lo conservamos y marcamos como visto
  

;; 8. zigzag-sum : (listof number) -> number
(define (zigzag-sum lst)
  (let rec ([xs lst] [sign 1]) ; sign = 1 para sumar, -1 para restar, funcion recursiva
    (cond
      [(empty? xs) 0] ; caso base: lista vacia suma 0
      [else (+ (* sign (first xs)) ; aplica el signo al primer elemento
               (rec (rest xs) (- sign)))]))) ; cambia el signo multiplicando por -1 

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
