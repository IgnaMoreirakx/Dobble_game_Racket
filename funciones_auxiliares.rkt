#lang racket
(provide (all-defined-out))



;Funcion: funcion para comparar los elementos de la lista con los numeros
;Dominio: 2 enteros, uno corresponde a los elementos de la lista y el otro un numero entero
;Recorrido:  boolean, t si son iguales, f si son distintos
;recursion; no aplica
(define i?
  (lambda (e i)
    (define calcular
      (lambda ( e i)
        (if (equal? i e)
            #t
            #f)))
    (calcular e i)))

;Funcion: Hace un cambio de los elementos si estos son iguales
;Dominio: 2 listas y una función de comparación
;Recorrido:  lista con los elementos cambiados
;recursion; Natural


(define myfilter2
  (lambda ( lista funct lista2)
    (define calcular
      (lambda (lista funct i lista2)
                     (if (or (empty? lista)(empty? lista2))
                         null
                         (if (funct (car lista) i)
                             (cons (car lista2) (calcular (cdr lista) funct i lista2))
                             (calcular lista funct (+ i 1) (cdr lista2))))))
    (calcular lista funct 1 lista2)))


;Funcion: Agrupa las cartas ya con los simbolos cambiados
;Dominio: 2 listas, una seran las cartas y el otro los elemetos que se quieran que esten en las cartas
;Recorrido:  genera las cartas con los simbolos que se requieren
;recursion; De cola
(define transformar
  (lambda (cartas elementos)
    (define aplicar
      (lambda (cartas funct elementos)
        (myfilter2 cartas funct elementos)))
    (aplicar cartas i? elementos)))


;Fuunción: verifica si un numero es primo o no
;Dominio: entero
;recorrido: boolean
;recursividad: de cola
(define primo?
  (lambda (x)
    (define calcular
      (lambda (x cont acum)
        (if (and (>= x cont) (= (remainder x cont) 0))
            (calcular x (+ cont 1) (+ acum 1))
            (if (>= x cont)
                (calcular x (+ cont 1) acum)
                (if (= acum 2)
                    #t
                    #f)))))
    (calcular x 1 0)))

;Fuunción: arma una lista desde 0 hasta n
;Dominio: entero n
;recorrido: lista
;recursividad: natural
(define listan (lambda (e)
                (define calcular (lambda (e i)
                  (if (= e 0)
                      null
                      (cons i (calcular (- e 1) (+ i 1))))))
                (calcular e 1)))


;-------------------------------------------------------------

(define comparar (lambda (lista)
                   (define aux (lambda (lista lista2)
                                 (if (or (= (length lista) 1) (null? lista))
                                     #t
                                     (if (null? lista2)
                                         (aux (cdr lista) (cdr (cdr lista)))
                                     (if (= (car lista) (car lista2))
                                         #f
                                         (aux lista (cdr lista2)))))))
                   (aux lista (cdr lista))))
;---------------------------------------------------------
(define m 2147483647)
(define a 1103515245)
(define c 12345)
(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)