#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   SCORE-ADT   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "abstracties.rkt")
(require "positie-adt.rkt")
(require "teken-adt.rkt")
(provide maak-adt-score)


(define (maak-adt-score)
  (let ((score 0))
      
    (define (teken! teken-adt)
      ((teken-adt 'teken-score!) dispatch-score))

    (define (update-score! msg teken-adt)
      (case msg
        ('munt (set! score (+ score 100)))
        ('insect (set! score (+ score 75)))
        ('pil (set! score (+ score 50))))
      (teken! teken-adt))

    (define (reset! teken-adt)
      (set! score 0)
      (teken! teken-adt))

    
    (define (dispatch-score msg)
      (case msg
        ('reset! reset!)
        ('teken! teken!)
        ('score score)
        ('update! update-score!)))
    dispatch-score))