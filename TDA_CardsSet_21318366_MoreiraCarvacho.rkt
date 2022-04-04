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
    (cons (Firstcard elements e)(Firstsymbolo elements (- e 1)))))

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
(define (Firstsymbolo elements e)
  (if (= e 0)
      null
      (cons (list(car elements)) (Firstsymbolo elements (- e 1)))))


;Funcion: Agrega los siguietnes simbolos a las cartas ya creadas segun las restricciones matematicas 
;Dominio: El conjunto de elementos y un entero (e) que representa los simbolos por cada carta
;Recorrido:  E cartas con la totalidad de simbolos
;recursion; Natural
(define Nextscard
  (lambda (elements e)
    (define calcular1
      (lambda (elements lista e i)
        (if (= (length(car(reverse lista))) e)
            lista
            (if (member (car elements)(car lista))
                (calcular1 (cdr elements) lista e i)
                (if (= (length(car(cdr lista))) e)
                    (calcular1 elements (cdr lista) e i)
                    (if (= i e)
                        (calcular1 elements lista e 0)
                        ((append (car(cdr lista)) (list(car elements))) (calcular1 elements lista e (+ i 1)))))))))
    (calcular1 elements (creando letras e) e 1)))

;problema de recursividad, seguir revisandola