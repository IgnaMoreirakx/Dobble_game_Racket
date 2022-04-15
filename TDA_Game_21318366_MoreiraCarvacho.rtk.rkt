#lang racket
(require "TDA_usuario_21318366_MoreiraCarvacho.rkt")

;crea el tda game, con int X cartas X modo X fn
(define game (lambda (numj mazo modo fn)
               (cons (list) (cons numj (cons mazo (cons modo(cons (list) (cons fn null))))))))

;---------------------selectores-----------------------
(define getMazo (lambda (game)
                  (car (cdr (cdr game)))))

(define getnum (lambda (game)
                   (car (cdr game))))
(define getmodo (lambda (game)
                  (car (cdr (cdr(cdr game))))))
(define getArea (lambda (game)
                  (car (cdr (reverse game)))))
(define getfn (lambda (game)
                (car (reverse game))))
;-----------------------------------------

;representación: mazo X area de juego, la primera lista corresponde al mazo resultante y las últimas 2 cartas corresponden a las que estan en el area de juego
(define StackMode (lambda (lista)
                   (define calcular (lambda (mazo area)
                                       (if (= (length area) 2)
                                           (append (list mazo) area)
                                           (calcular (cdr mazo) (cons (car mazo) area)))))
                    (calcular lista (list))))

;-------------------------------------------
;representacion: usuario a registrar X game
(define register (lambda (jugador lista)
                   (if (= (getnum lista) 0)
                       null
                       (append (list (append (car lista) (list jugador))) (cdr (game (- (getnum lista) 1) (getMazo lista) (getmodo lista) (getfn lista)))))))
;-----------------------------------------------
