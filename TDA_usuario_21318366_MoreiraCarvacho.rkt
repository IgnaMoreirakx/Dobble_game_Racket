#lang racket
(provide (all-defined-out))

;------------------Constructor-------------------------------
;representación: nombre X turno(boolean) X score
;Fuunción: Crea un tipo de dato usuario
;Dominio: nombre X turno X score
;recorrido: Usuario
;recursividad: NO aplica
(define usuario (lambda (nombre turno score)
                  (cons nombre(cons turno (cons score null)))))
;------------------Selectores-------------------------------
;Fuunción: Obtener el nombre de un usuario
;Dominio: Usuario
;recorrido: Nombre del usuario
;recursividad: No aplica
(define getName (lambda (usuario)
                  (car usuario)))

;Fuunción: Obtener el turno de un usuario
;Dominio: Usuario
;recorrido: Turno del usuario
;recursividad: No aplica
(define getTurno (lambda (usuario)
                   (car(cdr usuario))))
;Fuunción: Obtener el score de un usuario
;Dominio: Usuario
;recorrido: Score del usuario
;recursividad: No aplica
(define getScore (lambda (usuario)
                   (if (or (null? usuario) (equal? usuario #f))
                      null
                      (car (reverse usuario)))))
;-----------------Modificadores---------------------------
;Fuunción: Modificar el turno de un usuario
;Dominio: Usuario
;recorrido: Usuario con nuevo turno del usuario (boolean)
;recursividad: No aplica
(define setturno (lambda (lista)
                   (usuario (getName lista) (not (getTurno lista)) (getScore lista))))

;Fuunción: Modificar el score de un usuario
;Dominio: Usuario
;recorrido: Usuario con nuevo score del usuario (boolean)
;recursividad: No aplica
(define setScore (lambda (lista)
                   (usuario (getName lista) (getTurno lista) (+ (getScore lista) 1))))
;-----------------Otras funciones-----------------------------
;Fuunción: Modificar el turno y score al mismo tiempo
;Dominio: Usuario
;recorrido: Usuario con nuevo turno del usuario (boolean) y score
;recursividad: No aplica
(define setmarcador (lambda (lista)
                      (usuario (getName lista) (not(getTurno lista)) (+ (getScore lista) 1))))