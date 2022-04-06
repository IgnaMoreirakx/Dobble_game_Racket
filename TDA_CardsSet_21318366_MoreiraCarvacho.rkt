#lang racket
(define Simbolos (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
(define letras (list "a" "b" "c" "d" "e" "f" "g" "h"))
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
;Dominio: Lista de elementos o simbolos y un entero (e) que significa la cantidad de simbolos por carta
;recorrido: Cardsset
;recursividad: no aplica
(define creando
  (lambda (elements e)
    (cons (Firstcard elements e) (armar_n_cartas e))))

;esta funcion sigue en construccion ya que será la principal para crea el cardset


;Funcion: Armar la primera carta del set, la cual contiene los elementos
; 1 hasta e
;Dominio: Conjunto de elementos o simbolos(list ()) y un entero que representa la cantidad de elementos en cada carta(e)
;Recorrido: Primera carta
;recursion; Natural
(define (Firstcard elements e)
  (if(= e 0)
     null
     (if (empty? elements)
         null
         (cons(car elements) (Firstcard(cdr elements) (- e 1))))))

;Funcion: Corresponde a la siguiente parte del algoritmo, agrega el simbolo inical a las cartas [2 - e]
;Dominio: Conjunto de elementos o simbolos(list ()) y un entero que representa la cantidad de elementos en cada carta(e)
;Recorrido: N cartas representadas en listas
;recursion; Natural
(define Firstsymbolo
  (lambda (e)
    (define calcular
      (lambda (n j)
        (if (> j n)
            null
            (cons (list 1) (calcular n (+ j 1))))))
    (calcular (- e 1) 1)))


;Funcion: Entregar los proximos simbolos para las n cartas siguientes despues de la primera 
;Dominio: Un entero (e) que representa los simbolos por cada carta
;Recorrido:  Simbolos para las n cartas siguientes
;recursion; Natural
(define n_cards
  (lambda (e)
    (define calcular
      (lambda (n j k)
        (if (> j n)
            null
            (if (< n k)
                (calcular n (+ j 1) 1)
                (cons (list(+(+ k 1)(* n j))) (calcular n j (+ k 1)))))))
    (calcular (- e 1) 1 1)))

;Funcion: Entregar las n cartas siguientes con todos sus simbolos 
;Dominio: Un entero (e) que representa los simbolos por cada carta
;Recorrido:  N cartas
;recursion;cola
(define armar_n_cartas
  (lambda (e)
    (define calcular
      (lambda (e lista1 lista2 card)
        (if (or (empty? lista1) (empty? lista2))
                 card
                 (if (= (length (car lista1)) e)
                     (calcular e (cdr lista1) lista2 card)
                     (calcular e (cons (append (car lista1) (car lista2)) (cdr lista1)) (cdr lista2) (list(append (car lista1) (car lista2))))))))
    (calcular e (Firstsymbolo e) (n_cards e) (list))))
;revisar recursion
