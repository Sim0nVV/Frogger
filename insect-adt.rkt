#lang racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   INSECT-ADT   ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-insect)
(require "positie-adt.rkt")
(require "abstracties.rkt")

(define (maak-adt-insect x-pos y-pos)
  (let ((insect-pos (maak-adt-positie x-pos y-pos))
        (type 'insect))

    (define (verwijder! teken-adt score-adt)
      (verwijder-eetbaar-adt! teken-adt dispatch-insect score-adt))

    (define (teken! teken-adt)
      ((teken-adt 'teken-eetbaar-adt!) dispatch-insect))

    (define (reset! teken-adt)
      (reset-eetbaar-adt! teken-adt dispatch-insect))


    (define (dispatch-insect msg)
      (case msg
        ('x (insect-pos 'x))
        ('y (insect-pos 'y))
        ('type type)
        ('pos insect-pos)
        ('verwijder! verwijder!)
        ('teken! teken!)
        ('reset! reset!)))

    dispatch-insect))