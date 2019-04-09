#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Positie ADT ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-positie)

(define (maak-adt-positie x y)
  (let ((x-pos x)
        (y-pos y))
    
    (define (x! x)
      (set! x-pos x))

    (define (y! y)
      (set! y-pos y))
    
    (define (dispatch-pos msg)
      (cond ((eq? msg 'x!) x!)
            ((eq? msg 'y!) y!)
            ((eq? msg 'pos) (cons x-pos y-pos))
            ((eq? msg 'x) x-pos)
            ((eq? msg 'y) y-pos)))
    
    dispatch-pos))