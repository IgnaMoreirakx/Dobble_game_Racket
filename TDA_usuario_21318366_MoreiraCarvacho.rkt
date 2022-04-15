#lang racket
(provide (all-defined-out))

;funcion que crea un usario con su nombre, su turno y un score
(define usuario (lambda (nombre turno score)
                  (cons nombre(cons turno (cons score null)))))