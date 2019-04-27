#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Positie ADT ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-positie)
(require "abstracties.rkt")

(define (maak-adt-positie x y);zorg dat positie-ADT het object ook vasthoudt
  (let ((x-pos x)
        (y-pos y)
        (object-adt #f))
    
    (define (x! x)
      (set! x-pos x))

    (define (y! y  ; rijstrook-adt object-adt
                ) ;positie-ADT inherit van Rijstrook-ADT
      ;((rijstrook-adt 'nieuwe-rijstrook!) (/ y px-element-hoogte) object-adt)
      (set! y-pos y))


    (define (dispatch-pos msg)
      (case msg
        ('x! x!)
        ('y! y!)
        ('pos (cons x-pos y-pos))
        ('x x-pos)
        ('y y-pos)
        #;('object! )))
    
    dispatch-pos))
