#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   INSECT-ADT   ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-insect)
(require "positie-adt.rkt")
(require "abstracties.rkt")

(define (maak-adt-insect insect-pos)
  (let ((type 'insect)
        (soort (random 4)))

    (define (verwijder! teken-adt score-adt)
      (verwijder-eetbaar-adt! teken-adt dispatch-insect score-adt))

    (define (teken! teken-adt)
      ((teken-adt 'teken-eetbaar-adt!) dispatch-insect))

    (define (reset! teken-adt)
      (set! soort (random 4))
      (reset-eetbaar-adt! teken-adt dispatch-insect) )


    (define (dispatch-insect msg)
      (case msg
        ('x (insect-pos 'x))
        ('y (insect-pos 'y))
        ('type type)
        ('pos insect-pos)
        ('verwijder! verwijder!)
        ('teken! teken!)
        ('soort soort)
        ('reset! reset!)))

    dispatch-insect))

