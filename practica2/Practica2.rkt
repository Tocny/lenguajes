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
    [(= m 0) (error "El cero no es divisor de nadie")] ;primer caso base
    [(= m 1) #t] ;segundo caso base, 1 siempre es divisor de cualquier numero
    [(= (remainder n m) 0) #t])) ;si el residuo de la division es 0, entonces es divisor

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