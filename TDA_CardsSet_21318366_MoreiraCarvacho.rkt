#lang racket
(define Simbolos (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24))
(define Letras (list "a" "b" "c" "d" "e" "f" "g" "h"))
;Este tda corresponde a al representancion del mazo de cartas de dobble
;se busca abarcar el crear un mazo valido de dobble con "cartas"
;conociendo las restricciones matematicas de dobble
;------------------Constructor-------------
;en esta seccion se buscar crear un mazo valido
;para esto se intenta traducir el algoritmo dejado en el documento
;al lenguaje scheme
;es por esto que habrá varias funciones que pertenecerán
;a la función final "CardsSet" (la idea es al final poder encapsular todas las funciones

;Funcion: Armar la primera carta del set, la cual contiene los elementos
; 1 hasta el n+1
;Dominio: Conjunto de elementos o simbolos(list ()) y un entero que representa la cantidad de elementos en cada carta(n)
;Recorrido: Primera carta
;recursion; Natural
(define (Firstcard elements num )
  (if(= (+ 1 num) 0)
     null
     (if (empty? elements)
         null
         (cons(car elements) (Firstcard(cdr elements) (- num 1))))))

;Funcion: COrresponde a la siguiente parte del algoritmo, agrega el simbolo inical a las cartas [2 - n+1]
;Dominio: Conjunto de elementos o simbolos(list ()) y un entero que representa la cantidad de elementos en cada carta(n)
;Recorrido: N cartas representadas en listas
;recursion; Natural
(define (Firstsymbolo elements num)
  (if (= num 0)
      null
      (cons (list(car elements)) (Firstsymbolo elements (- num 1)))))


;Funcion: Agrega los siguietnes simbolos a las cartas ya creadas segun las restricciones matematicas 
;Dominio: Un entero que representa la cantidad de elementos en cada carta(n)(deberia recibir un lista de elementos, pero esta en proceso
;Recorrido: Cartas con la totalidad de simbolos
;recursion; Cola
(define calcular3
  (lambda (n)
    (define calcular
      (lambda (n j k)
        (if (> j n)
            j
            (if (> k n)
                (list(calcular n (+ j 1) 1))
                (cons (+(+ k 1)(* n j)) (calcular n j (+ k 1)))))))
    (calcular n 1 1)))