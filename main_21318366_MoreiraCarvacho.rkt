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
(define cardset1 (CardsSet 3 -1 Numeros (randomFn 1)))
(define cardset2 (CardsSet 3 2 letras (randomFn 1)))
(define cardset3 (CardsSet 4 5 Numeros (randomFn 1)))
;pertenencia
(dobble? cardset1)
(dobble? cardset2)
(dobble? cardset3)

;numcard
(define numcard1 (numCards cardset1))
(define numcard2 (numCards cardset2))
(define numcard3 (numCards cardset3))

;nthCards
(define nthcard1 (nthCard cardset1 4))
(define nthcard2 (nthCard cardset2 2))
(define nthcard3 (nthCard cardset3 3))

;FindTotalCards
(define findTotalCards1 (findTotalCards nthcard1))
(define findTotalCards2 (findTotalCards nthcard2))
(define findTotalCards3 (findTotalCards nthcard3))

;requiredElements
(define requiredElements1 (requiredElements nthcard1))
(define requiredElements2 (requiredElements nthcard2))
(define requiredElements3 (requiredElements nthcard3))

;missingcard
(define missingcard1 (missingCards cardset1))
(define missingcard2 (missingCards cardset2))
(define missingcard3 (missingCards cardset3))

;cardset->string
(define string1 (cardsSet->string cardset1))
(define string2 (cardsSet->string cardset2))
(define string3 (cardsSet->string cardset3))

;--------------Se pasa al tda usuario-----------
(define jugador1 (usuario "juan" #f 0))
(define jugador2 (usuario "francisca" #f 0))
(define jugador3 (usuario "tomas" #f 0))

;selectores
(define nombre1 (getName jugador1))
(define nombre2 (getName jugador2))
(define nombre3 (getName jugador3))

(define turno1 (getTurno jugador1))
(define turno2 (getTurno jugador2))
(define turno3 (getTurno jugador3))

(define score1 (getScore jugador1))
(define score2 (getScore jugador2))
(define score3 (getScore jugador3))

;modificadores
(define cambioturno1 (setturno jugador1))
(define cambioturno2 (setturno jugador2))
(define cambioturno3 (setturno jugador3))

(define cambioscore1 (setScore jugador1))
(define cambioscore2 (setScore jugador2))
(define cambioscore3 (setScore jugador3))

(define marcador1 (setmarcador jugador1))
(define marcador2 (setmarcador jugador2))
(define marcador3 (setmarcador jugador3))

;---------------se pasa al Tda game----------------
(define game1 (game 3 cardset1 (StackMode cardset1) (randomFn 1)))
(define game2 (game 2 cardset2 (StackMode cardset2) (randomFn 1)))
(define game3 (game 4 cardset3 (StackMode cardset3) (randomFn 1)))
;selectores
(define users1 (getUsers game1))
(define users2 (getUsers game2))
(define users3 (getUsers game3))

(define num1 (getnum game1))
(define num2 (getnum game2))
(define num3 (getnum game3))

(define mazo1 (getMazo game1))
(define mazo2 (getMazo game2))
(define mazo3 (getMazo game3))

(define modo1 (getmodo game1))
(define modo2 (getmodo game2))
(define modo3 (getmodo game3))

(define estado1 (getEstado game1))
(define estado2 (getEstado game2))
(define estado3 (getEstado game3))
;StackMOde
(define stackmode1 (StackMode cardset1))
(define stackmode2 (StackMode cardset2))
(define stackmode3 (StackMode cardset3))


(define register2 (register "Tamara" (register "Jose" game2)))
(define register1 (register "Arantza" (register "Tamara" (register "Jose" game1))))
(define register3 (register "Roberto" (register "Arantza" (register "Tamara" (register "Jose" game3)))))

(define turno1? (whoseTurnIsIt? register1))
(define turno2? (whoseTurnIsIt? register2))
(define turno3? (whoseTurnIsIt? register3))

(define play1 (play register1 finish))
(define play2 (play (spotIt (play register2 null) "a") finish))
(define play3 (play register3 pass))

;status
(define status1 (status game1))
(define status2 (status play2))
(define status3 (status game3))
;scores
(define scorejugador1 (score register1 "Arantza"))
(define scorejugador2 (score play2 "Jose"))
(define scorejugador3 (score play3 "Roberto"))