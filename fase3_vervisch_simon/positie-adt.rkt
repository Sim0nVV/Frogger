#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Positie ADT ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-positie)
(require "abstracties.rkt")

(define (maak-adt-positie x y)
  (let ((x-pos x)
        (y-pos y))
    
    (define (x! x)
      (set! x-pos x))

    (define (y! y) 
      (set! y-pos y))

    (define (dispatch-pos msg)
      (case msg
        ('x! x!)
        ('y! y!)
        ('pos (cons x-pos y-pos))
        ('x x-pos)
        ('y y-pos)))
    
    dispatch-pos))
