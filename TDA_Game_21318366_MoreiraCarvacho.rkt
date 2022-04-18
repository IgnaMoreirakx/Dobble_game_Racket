#lang racket
(require "TDA_usuario_21318366_MoreiraCarvacho.rkt")
(require "TDA_CardsSet_21318366_MoreiraCarvacho.rkt")
(require "funciones_auxiliares.rkt")
;representacion de este TDA: jugadores X numjugadores X mazo X modo X estado X random

;Fuunción: Crear un tipo de dato Game
;Dominio: Numbjugadores X mazo X modo X random
;recorrido: Game
;recursividad: No aplica
(define game (lambda (numj mazo modo fn)
               (cons (list) (cons numj (cons (cdr mazo) (cons modo(cons "iniciado" (cons fn null))))))))

;---------------------selectores-----------------------

;Fuunción: Obtener los jugadores del game
;Dominio: game
;recorrido: Jugadores
;recursividad: No aplica
(define getUsers (lambda (game)
                   (car game)))

;Fuunción: Obtener el num de jugadores del game
;Dominio: game
;recorrido: Num de jugadores
;recursividad: No aplica
(define getnum (lambda (game)
                   (car (cdr game))))

;Fuunción: Obtener el mazo del juego
;Dominio: game
;recorrido: Mazo
;recursividad: No aplica
(define getMazo (lambda (game)
                  (car (cdr (cdr game)))))

;Fuunción: Obtener el Modo de juego
;Dominio: game
;recorrido: Modo de juego
;recursividad: No aplica
(define getmodo (lambda (game)
                  (car (cdr (cdr(cdr game))))))

;Fuunción: Obtener el area de juego
;Dominio: game
;recorrido: area de juego
;recursividad: No aplica
(define getArea (lambda (game)
                  (car (cdr (reverse game)))))
;Fuunción: Obtener el random para temas de aleatoridad
;Dominio: game
;recorrido: random
;recursividad: No aplica
(define getfn (lambda (game)
                (car (reverse game))))

;Fuunción: Obtener el estado del jeugo
;Dominio: game
;recorrido: estado de juego
;recursividad: No aplica
(define getEstado (lambda (game)
                    (car(cdr (cdr (cdr (cdr game)))))))
;-----------------------------------------

;representación: una lista donde las ultimas 2 cartas son las que los jugadores ven y corresponde al area de juego, que asu vez es una sublista y las demás cartas estan en otra lista que corresponde al mazo
;Fuunción: Se emplea el modo de juego
;Dominio: CardsSet
;recorrido: CardsSet
;recursividad: de cola
(define StackMode (lambda (lista)
                   (define calcular (lambda (mazo area)
                                       (if (= (length area) 2)
                                           (cons (delay mazo) area)
                                           (calcular (cdr mazo) (cons (car mazo) area)))))
                    (calcular (cdr lista) (list))))

;------------------Modificador-------------------------
;Fuunción: registra a los jugadores
;Dominio: game X jugador
;recorrido: game
;recursividad: No aplica, el de la funcion interna si (compararname), ver "funciones auxiliares"
(define register (lambda (jugador lista)
                   (if (= (getnum lista) 0)
                       null
                       (if (not (compararname jugador (getUsers lista)))
                           null
                           (if (null? (car lista))
                               (append (list (append (car lista) (list (usuario jugador #t 0)))) (cdr (game (- (getnum lista) 1) (getMazo lista) (getmodo lista) (getfn lista))))
                               (append (list (append (car lista) (list (usuario jugador #f 0)))) (cdr (game (- (getnum lista) 1) (getMazo lista) (getmodo lista) (getfn lista)))))))))
;-----------------------------------------------
;Fuunción: Saber de quien es el turno
;Dominio: game
;recorrido: jugador
;recursividad: de cola
(define whoseTurnIsIt? (lambda (game)
                         (define calcular (lambda (lista)
                           (if (null? lista)
                               null
                               (if (eq? (getTurno (car lista)) #t)
                                   (getName (car lista))
                                   (calcular (cdr lista))))))
                         (calcular (car game))))
;-----El siguiente bloque corresponden a funciones y subfunciones que corresponden a "play"--------------------------------------------

;Fuunción: elegir la "jugada"
;Dominio: game X juagda
;recorrido: game
;recursividad: de cola
(define play (lambda (game action)
               (if (equal? action null)
                   game
                   (if (equal? action finish)
                       (play (finish game) null)
                       (if (equal? action pass)
                           (play (pass game) null)
                           null)))))
                           
;Fuunción: comparar 2 cartas con el elemento que dice el usuario y cambio de turno y de puntaje si es necesario
;Dominio: game X elemento
;recorrido: game
;recursividad: no aplica
(define spotIt (lambda (game elemento)
                 (if ( member elemento (car (reverse (getmodo game))))
                     (if ( member elemento (car (cdr (reverse (getmodo game)))))
                         (append (list (buscar (getUsers game) (jugadorlista (getUsers game) (whoseTurnIsIt? game)) (setmarcador (jugadorlista (getUsers game) (whoseTurnIsIt? game))))) (cdr game))
                         (append (list (buscar (getUsers game) (jugadorlista (getUsers game) (whoseTurnIsIt? game)) (setturno (jugadorlista (getUsers game) (whoseTurnIsIt? game))))) (cdr game)))
                     (append (list (buscar (getUsers game) (jugadorlista (getUsers game) (whoseTurnIsIt? game)) (setturno (jugadorlista (getUsers game) (whoseTurnIsIt? game))))) (cdr game)))))

;Fuunción: Pasar de turno
;Dominio: game
;recorrido: game
;recursividad: no aplica
(define pass (lambda (game)
               (append (list (buscar (getUsers game) (jugadorlista (getUsers game) (whoseTurnIsIt? game)) (setturno (jugadorlista (getUsers game) (whoseTurnIsIt? game))))) (cdr game))))

;Fuunción: terminar el juego y dar una conclusión de este
;Dominio: game
;recorrido: game con estado finalizado
;recursividad: no aplica
(define finish (lambda (lista)
                 (if (not (null? (empate lista)))
                     (append (buscar lista "iniciado" "finalizado") (empate lista))
                     (append (buscar lista "iniciado" "finalizado") (ganador lista)))))

;Fuunción: determina el ganador
;Dominio: game
;recorrido: ganador
;recursividad: no aplica
(define ganador (lambda (game)
                  (string-append "ganador: " (getName (jugadorlista (getUsers game) (maximo (scores game)))))))

;Fuunción: determina si hay empate o no
;Dominio: game
;recorrido: boolean
;recursividad: no aplica
(define empate (lambda (game)
                       (define calcular (lambda (lista i)
                                          (if (null? lista)
                                              "empate"
                                              (if (= (car lista) i)
                                                  (calcular (cdr lista) i)
                                                  null))))
                       (calcular (scores game) (car (scores game)))))

;Fuunción: determina los scores de todos los jugadores
;Dominio: game
;recorrido: lista de scores
;recursividad: no aplica

(define scores (lambda (lista)
                 (define calcular (lambda (lissta)
                                    (if (null? lissta)
                                        null
                                        (cons (getScore (car lissta)) (calcular (cdr lissta))))))
                 (calcular (getUsers lista))))
;-------------------------------------------------------------------------------

;Fuunción: determina el estado de un juego
;Dominio: game
;recorrido: Estado del juego
;recursividad: no aplica
(define status (lambda (game)
                 (getEstado game)))
;Fuunción: determina el score de un jugador
;Dominio: game X jugador
;recorrido: Score
;recursividad: no aplica
(define score (lambda (game jugador)
                (if(null? (getUsers game))
                   null
                   (getScore (jugadorlista (getUsers game) jugador)))))
;--------------------------------------------------------------------------
(provide (all-defined-out))
;---------------------------------------------------------------------------                      