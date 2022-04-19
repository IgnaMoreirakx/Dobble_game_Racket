#lang racket
(require "TDA_usuario_21318366_MoreiraCarvacho.rkt")
(require "TDA_CardsSet_21318366_MoreiraCarvacho.rkt")
(require "funciones_auxiliares.rkt")
(require "TDA_Game_21318366_MoreiraCarvacho.rkt")

;En este archivo se realizarán los ejemplos de cada función realizadas

(define Numeros (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
(define letras (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "nn" "x"))
;----------------CardSet-----------------------------
;Constructor
(define cardset1 (CardsSet 3 -1 Numeros (randomFn 1))) ;Se crea un cardset con 3 elementos por cartas, con todas sus cartas
(define cardset2 (CardsSet 3 2 letras (randomFn 1))) ;Se crea un cardset con 3 elementos por cartas, con 2 de sus cartas
(define cardset3 (CardsSet 4 5 Numeros (randomFn 1))) ;Se crea un cardset con 4 elementos por cartas, con todas sus cartas
;pertenencia
(dobble? cardset1) ;se verifica si el cardset es uno válido o no
(dobble? cardset2) ; se verifica si el cardset es uno válido o no
(dobble? cardset3) ; se verifica si el cardset es uno válido o no

;numcard
(define numcard1 (numCards cardset1)) ;se obtiene la cantidad de cartasd el cardset1
(define numcard2 (numCards cardset2));se obtiene la cantidad de cartasd el cardset2
(define numcard3 (numCards cardset3));se obtiene la cantidad de cartasd el cardset3

;nthCards
(define nthcard1 (nthCard cardset1 4)) ;se obtiene la carta n°4
(define nthcard2 (nthCard cardset2 2)) ;se obtiene la carta n°2
(define nthcard3 (nthCard cardset3 3)) ;se obtiene la carta n°3

;FindTotalCards
(define findTotalCards1 (findTotalCards nthcard1)) ;se obtienen las cartas totales sabiendo solo una carta
(define findTotalCards2 (findTotalCards nthcard2))
(define findTotalCards3 (findTotalCards nthcard3))

;requiredElements
(define requiredElements1 (requiredElements nthcard1)) ;se obtienen las cartas totales sabiendo solo una carta
(define requiredElements2 (requiredElements nthcard2))
(define requiredElements3 (requiredElements nthcard3))

;missingcard
(define missingcard1 (missingCards cardset1)) ;se obtienen las cartas restantes apartir de un set de cartas valido, pero incompleto
(define missingcard2 (missingCards cardset2))
(define missingcard3 (missingCards cardset3))

;cardset->string
(define string1 (cardsSet->string cardset1)) ;covierte el cardset en string para poder visualizarlo con la funciion display
(define string2 (cardsSet->string cardset2))
(define string3 (cardsSet->string cardset3))


;addCard
(define addcard1 (addCard cardset2  (list "b" "e" "g")))
(define addcard2 (addCard cardset1  (list 1 2 3))) ;como el cardSet esta completo no se agrega la carta
(define addcard3 (addCard cardset3  (list "b" "e" "g"))) ;como el cardset y la carta que se quieren agregar no tienen el mismo tipo de simbolo no se agrega la carta

;--------------Se pasa al tda usuario-----------
(define jugador1 (usuario "juan" #f 0))        ;se crean jugadores
(define jugador2 (usuario "francisca" #f 0))
(define jugador3 (usuario "tomas" #f 0))

;selectores
(define nombre1 (getName jugador1))   ;se obtiene el nombre del jugador
(define nombre2 (getName jugador2))
(define nombre3 (getName jugador3))

(define turno1 (getTurno jugador1))  ;se obtiene el turno del jugador
(define turno2 (getTurno jugador2))
(define turno3 (getTurno jugador3))

(define score1 (getScore jugador1)) ;se obtiene el score del jugador
(define score2 (getScore jugador2))
(define score3 (getScore jugador3))

;modificadores
(define cambioturno1 (setturno jugador1)) ;se cambia el turno del jugador
(define cambioturno2 (setturno jugador2))
(define cambioturno3 (setturno jugador3))

(define cambioscore1 (setScore jugador1))  ;se cambia el score del jugador, se suma 1 
(define cambioscore2 (setScore jugador2))
(define cambioscore3 (setScore jugador3))

(define marcador1 (setmarcador jugador1)) ;se cambia el turno y el score
(define marcador2 (setmarcador jugador2))
(define marcador3 (setmarcador jugador3))

;---------------se pasa al Tda game----------------
(define game1 (game 3 cardset1 (StackMode cardset1) (randomFn 1))) ;se definen los games
(define game2 (game 2 cardset2 (StackMode cardset2) (randomFn 1)))
(define game3 (game 4 cardset3 (StackMode cardset3) (randomFn 1)))
;selectores
(define users1 (getUsers game1)) ;se obtienen los jugadores registrados del game
(define users2 (getUsers game2))
(define users3 (getUsers game3))

(define num1 (getnum game1)) ;se obtiene el numero de jugadores registrados del game
(define num2 (getnum game2))
(define num3 (getnum game3))

(define mazo1 (getMazo game1)) ;se obtiene el mazo  del game
(define mazo2 (getMazo game2))
(define mazo3 (getMazo game3))

(define modo1 (getmodo game1)) ;se obtiene el modo del game
(define modo2 (getmodo game2))
(define modo3 (getmodo game3))

(define estado1 (getEstado game1)) ;se obtiene el estado del game
(define estado2 (getEstado game2))
(define estado3 (getEstado game3))
;StackMOde
(define stackmode1 (StackMode cardset1)) ;se crea el modo con el mazo dado
(define stackmode2 (StackMode cardset2))
(define stackmode3 (StackMode cardset3))


(define register2 (register "Tamara" (register "Jose" game2)))
(define register1 (register "Arantza" (register "Tamara" (register "Jose" game1))))
(define register3 (register "Roberto" (register "Arantza" (register "Tamara" (register "Jose" game3))))) ;se registran jugadores, dependiendo del numero de jugadores del game

(define turno1? (whoseTurnIsIt? register1)) ;se obtiene el jugador que le toca jugar
(define turno2? (whoseTurnIsIt? register2))
(define turno3? (whoseTurnIsIt? register3))

(define play1 (play register1 finish))
(define play2 (play (spotIt (play register2 null) "a") finish)) ;Se define la jugada del jugador y se obtiene el game con el estado como resultado
(define play3 (play register3 pass))

;status
(define status1 (status game1)) ;se entrega el estado del juego
(define status2 (status play2))
(define status3 (status game3))
;scores
(define scorejugador1 (score register1 "Arantza")) ;se obtiene el score de un jugador ingresado por el usuario
(define scorejugador2 (score play2 "Jose"))
(define scorejugador3 (score play3 "Roberto"))