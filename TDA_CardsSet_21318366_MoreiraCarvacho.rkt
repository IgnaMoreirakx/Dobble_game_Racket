#lang racket
(define Simbolos (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
(define letras (list "a" "b" "c" "d" "e" "f" "g" "h"))
(require "funciones_auxiliares.rkt")
;Este tda corresponde a al representancion del mazo de cartas de dobble
;se busca abarcar el crear un mazo valido de dobble con "cartas"
;conociendo las restricciones matematicas de dobble
;------------------Constructor-------------
;en esta seccion se buscar crear un mazo valido
;para esto se intenta traducir el algoritmo dejado en el documento
;al lenguaje scheme
;es por esto que habrá varias funciones que pertenecerán
;a la función final "CardsSet" (la idea es al final poder encapsular todas las funciones

;Fuunción: es la encargada de encapsular todo el proceso de la creacion del cardsset
;Dominio: Lista de elementos o simbolos y un entero (e) que significa la cantidad de simbolos por carta y un entero c que significa la cantidad de cartas a entregar
;recorrido: Cardsset
;recursividad: Natural
(define Finallyset
  (lambda (e c elementos)
    (define calcular
      (lambda (e c lista)
        (if (or (primo? (- e 1)) (= (- e 1) 1))
            (if (<= c -1)
                lista
                (if (or (= c 0) (> c (+ (* (- e 1)(- e 1)) (- e 1) 1)))
                    null
                    (cons (car lista) (calcular e (- c 1) (cdr lista)))))
            null)))
     (calcular e c (mazo e elementos))))

;esta función irá en el main
(define CardsSet (lambda ( e c elementos fn)
                   (if (not(comparar elementos))
                       null
                       (if (odd? fn)
                           (shuffle (Finallyset e c elementos))
                           (shuffle (shuffle (Finallyset e c elementos)))))))

;Fuunción: es la encargada de crear el mazo en su totalidad
;Dominio: Lista de elementos o simbolos y un entero (e) que significa la cantidad de simbolos por carta y un entero c que significa la cantidad de cartas a entregar
;recorrido: Cardsset
;recursividad: No aplica

(define mazo
  (lambda (e elementos)
    (append (cons (Firstcard e elementos)(Next_cards e 1 elementos)) (Last_cards e 1 1 elementos))))

;Funcion: Armar la primera carta del set, la cual contiene los elementos
; 1 hasta e
;Dominio: Conjunto de elementos o simbolos(list ()) y un entero que representa la cantidad de elementos en cada carta(e)
;Recorrido: Primera carta
;recursion; De cola
(define Firstcard
  (lambda (e elementos)
    (define calcular
      (lambda (e i [L null])
        (if (= e 0)
            (transformar L elementos)
            (calcular (- e 1) (+ i 1) (append L (list i))))))
    (calcular e 1)))

;Funcion: crea la siguiente carta, correspondiente a las N cartas siguientes despues de la primera
;Dominio: entero que representa la cantidad de elementos en cada carta(e)
;Recorrido: N cartas representadas en listas
;recursion; De cola
(define A_card
  (lambda (e j)
    (define calcular
      (lambda (n k j [L null])
        (if (<= k n)
            (calcular n (+ k 1) j (append L (list (+ (* n j) (+ 1 k)))))
            (cons 1 L))))
    (calcular (- e 1) 1 j)))

;Funcion: Es la siguiente parte la función para generar las N cartas
;Dominio: Un entero (e) que representa los simbolos por cada carta y la función A_card
;Recorrido:  Simbolos para las n cartas siguientes
;recursion; De cola
(define Next_cards
  (lambda (e j elementos)
    (define calcular
      (lambda (lista j)
        (if (= j (- e 1))
            lista
            (calcular (append lista (list (transformar (A_card e (+ 1 j)) elementos))) (+ j 1)))))
    (calcular (list(transformar (A_card e j) elementos)) j)))

;Funcion: generar las N**2 cartas siguientes
;Dominio: Un entero (e) que representa los simbolos por cada carta
;Recorrido:  La primera carta de las N**2
;recursion; De cola
(define NN_cards
  (lambda (e i j)
    (define calcular
      (lambda (n i j k [L null])
        (if (> k n)
            (cons (+ i 1) L)
            (calcular n i j (+ k 1) (append L (list (+ (+ (* n (- k 1)) n 2) (modulo (+ (*(- i 1) (- k 1)) (- j 1)) n))))))))
    (calcular (- e 1) i j 1)))

;Funcion: Generar las N**2 cartas siguietnes( es parte de la construcción)
;Dominio: Un entero (e) que representa los simbolos por cada carta y la función NN_cards
;Recorrido:  genera las N de las N**2 cartas
;recursion; De cola
(define Finally_cards
  (lambda (e i j elementos)
    (define calcular
      (lambda (lista i j)
        (if (= j (- e 1))
            lista
            (calcular (append lista (list (transformar (NN_cards e i (+ j 1)) elementos))) i (+ j 1)))))
    (calcular (list(transformar (NN_cards e i j) elementos)) i j)))

;Funcion: Generar las N**2 cartas siguietnes( es parte de la construcción)
;Dominio: Un entero (e) que representa los simbolos por cada carta y la funcion Finally_cards
;Recorrido:  genera las N**2 cartas
;recursion; De cola
(define Last_cards
  (lambda (e i j elementos)
    (define calcular
      (lambda (lista i j)
        (if (= i (- e 1))
            lista
            (calcular (append lista (Finally_cards e (+ i 1) j elementos)) (+ i 1) j))))
    (calcular (Finally_cards e i j elementos) i j)))


;Funcion: Verifica que un set sea válido
;Dominio: CardsSet
;Recorrido:  boolean
;recursion; No aplica
(define dobble? (lambda (lista)
                  (if (null? lista)
                      #f
                      #t)))

;Funcion: Contar la cantidad de cartas en el set
;Dominio: CardsSet
;Recorrido:  número de cartas (int)
;recursion; De cola
(define numCards (lambda (lista)
                   (define aux (lambda (lista cont)
                                 (if (null? lista)
                                     cont
                                     (aux (cdr lista) (+ 1 cont)))))
                   (aux lista 0)))


;Funcion: Encargadad de entregar la N-esíma carta
;Dominio: CardsSet
;Recorrido:  N-esíma carta
;recursion; de cola
(define nthCard
  (lambda (lista i)
    (if (> i (length lista))
        null
        (if (= 1 i)
            (car lista)
            (nthCard (cdr lista) (- i 1))))))


;Funcion: Encargada de entregar la cantidad total de cartas apartir de una sola carta
;Dominio: N-esíma carta
;Recorrido:  Numero total de cartas (entero)
;recursion: No aplica
(define findTotalCards
  (lambda (lista)
    (if (null? lista)
        null
        (+ (* (- (length lista) 1) (- (length lista) 1)) (- (length lista) 1) 1))))


;Funcion: Encargada de entregar la cantidad total de elementos para generar un setcard valido
;Dominio: N-esíma carta
;Recorrido:  Numero total de elementos (entero)
;recursion: No aplica
(define requiredElements
  (lambda (lista)
    (if (primo? (- (length lista) 1))
        (+ (* (- (length lista) 1) (- (length lista) 1)) (- (length lista) 1) 1)
        null)))








