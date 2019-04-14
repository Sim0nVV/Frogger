#lang racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   INSECT-ADT   ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide maak-adt-insect)
(require "positie-adt.rkt")
(require "abstracties.rkt")

(define (maak-adt-insect x-pos y-pos)
  (let ((insect-pos (maak-adt-positie x-pos y-pos)))

    (define (verwijder! teken-adt score-adt)
      ((score-adt 'update!) 'insect teken-adt)
      (set-x&y! insect-pos (- 1) (- 1))
      ((teken-adt 'verwijder-insect!) dispatch-insect))

    (define (teken! teken-adt)
      ((teken-adt 'teken-insect!) dispatch-insect))

    (define (reset! teken-adt)
      ((insect-pos 'x!) (random-x))
      ((insect-pos 'y!) (random-y))
      ((teken-adt 'verwijder-insect!) dispatch-insect)
      ((teken-adt 'teken-insect!) dispatch-insect))


    (define (dispatch-insect msg)
      (case msg
        ('x (insect-pos 'x))
        ('y (insect-pos 'y))
        ('verwijder! verwijder!)
        ('teken! teken!)
        ('reset! reset!)))

    dispatch-insect))