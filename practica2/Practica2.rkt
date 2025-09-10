 #lang plai

;; Practica 2.
;; Integrantes del Equipo:
;; - Gonzalez Castillo Patricio Salvador 321142391
;; - Valencia Pérez Guillermo Emanuel 321018689
;; - Rubio Resendiz Marco Antonio 320209763
;; - Sautto Ramirez Seldon 321084163

;; 2.1.1. divisor?: number  number -> boolean
(define (divisor? m n)
  (cond
    [(= m 0) (error "El cero no es divisor de nadie")] ;si m es 0 entonces no es divisor de nadie
    [(= n 0) #t] ;si n es cero, cualquier numero es su divisor
    [(not (empty? ;si la lista que devolvió filter no es vacía devuelve true
        (filter (λ (k) (= (* m k) n)) ;filtra los numeros de 1 hasta n dependiendo si m multiplicado por alguno de esos es n
                (range 1 (+ 1 n)))))]))

;; 2.1.2. concat-numero: (listof number) -> number
(define (concat-numero list) 
  (foldl (λ (x acc) (+ (* acc 10) x)) 0 list)) ;usa la funcion foldl para aplicarle la funcion a cada
;elemento y sumar el resultado, primero hace 0 * 10 y suma el primer elemento, luego hace lo que salió por 10
;y suma el segundo elemento y asi con todos, usa una funcion anonima como se pide en las instrucciones


;; 2.2.1 Estructura Juego.
;; Estructura "personaje"
(struct personaje (nombre pelo gafas sombrero genero) #:transparent)

;; Estructura "juego"
(struct juego (tablero oculto preguntas) #:transparent)

;; 2.2.2 Constructor crear-juego.

;; Función auxiliar del documento:
(define (elegir-personaje-aleatorio lista)
  (if (null? lista)
      (error "No hay personajes disponibles.")
      (list-ref lista (random (length lista)))))

;; El constructor:
(define (crear-juego lista-personajes)
  (juego lista-personajes ;;tablero
         (elegir-personaje-aleatorio lista-personajes) ;;personaje oculto
         '())) ;;historial de preguntas

;; 2.2.3 hacer-pregunta.
(define (hacer-pregunta j pred)
  (let* ((personaje-secreto (juego-oculto j))
         (respuesta (pred personaje-secreto))
         (nuevo-historial (cons pred (juego-preguntas j)))
         (nuevo-juego (juego (juego-tablero j) 
                            (juego-oculto j) 
                            nuevo-historial)))
    (values respuesta nuevo-juego)))


;; 2.2.4 filtrar-tablero


;; Punto extra.




;; Ejemplos de uso.
(define carlos (personaje 'carlos 'negro #t #f 'hombre))
(define maria (personaje 'maria 'castaño #f #f 'mujer))
(define vivian (personaje 'vivian 'negro #f #f 'mujer))
(define federico (personaje 'federico 'negro #f #t 'hombre))
(define david (personaje 'david 'rojo #t #t 'hombre))
(define silvia (personaje 'silvia 'rubio #f #f 'mujer))


;; Los metemos a una lista
(define personajes (list carlos maria vivian federico david silvia))


;; Ejemplos de uso:
(define juego1 (crear-juego personajes))
(define (usa-gafas? p) (personaje-gafas p))
(define-values (respuesta1 juego2) (hacer-pregunta juego1 usa-gafas?))

