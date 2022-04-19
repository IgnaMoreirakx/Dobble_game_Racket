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


;-------------------------------------------------------------
;Fuunción: compara si se repite un elemento de una lista
;Dominio: lista
;recorrido: boolean
;recursividad: de cola
(define comparar (lambda (lista)
                   (define aux (lambda (lista lista2)
                                 (if (or (= (length lista) 1) (null? lista))
                                     #t
                                     (if (null? lista2)
                                         (aux (cdr lista) (cdr (cdr lista)))
                                     (if (eq? (car lista) (car lista2))
                                         #f
                                         (aux lista (cdr lista2)))))))
                   (aux lista (cdr lista))))

;----------------------------funcion random del documento-----------------------------
(define m 2147483647)
(define a 1103515245)
(define c 12345)
(define randomFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)
;------------------------------------------------------------------------------

;Fuunción: compara si hay nombres repetidos en una lista
;Dominio: game
;recorrido: boolean
;recursividad: de cola
(define compararname (lambda (jugador lista)
                       (if (null? lista)
                           #t
                           (if (member jugador (car lista))
                               #f
                               (compararname jugador (cdr lista))))))


;Fuunción: calcula el maximo de los scores
;Dominio: scores
;recorrido: int
;recursividad: de cola
(define maximo (lambda (lista)
                 (define calcular (lambda (lista i)
                                    (if (null? lista)
                                        i
                                        (if (> i (car lista))
                                            (calcular (cdr lista) i)
                                            (calcular (cdr lista) (car lista))))))
                 (calcular lista (car lista))))


;Fuunción: encuentra un elemento en una lista y lo reemplaza por otro elemento2
;Dominio: lista X elemento X elemento2
;recorrido: lista con el nuevo elemento
;recursividad: No aplica
(define buscar (lambda (lista elemento elemento2)
                 (if (equal? (car lista) elemento)
                     (cons elemento2 (cdr lista))
                     (cons (car lista) (buscar (cdr lista) elemento elemento2)))))

;Fuunción: Devuelve todo la informacion de un usuario solo con saber su nombre
;Dominio: jugadores X jugador
;recorrido: jugador
;recursividad: de cola
(define jugadorlista (lambda (lista jugador)
                       (if (null? lista)
                           #f
                           (if (member jugador (car lista))
                               (car lista)
                               (jugadorlista (cdr lista) jugador)))))

;Fuunción: junta sublistas en una lista
;Dominio: lista de listas
;recorrido: lista
;recursividad: Natural
(define juntar (lambda (lista)
                 (if (null? lista)
                     null
                     (append (car lista) (juntar (cdr lista))))))


;Fuunción: verifica si un par de cartas tienen mas de 1 elemento en comun
;Dominio: carta X numero de elementoss por carta
;recorrido: boolean
;recursividad: De cola
(define serepite? (lambda (lista e)
                   (define aux (lambda (lista lista2 i)
                                 (if (or (= (length lista) 1) (null? lista))
                                     #f
                                     (if (null? lista2)
                                         (aux (cdr lista) (cdr (cdr lista)) 0)
                                         (if (> i e)
                                             #t
                                             (if (equal? (car lista) (car lista2))
                                                 (aux lista (cdr lista2) (+ i 1))
                                                 (aux lista (cdr lista2) i)))))))
                   (aux lista (cdr lista) 0)))

