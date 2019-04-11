#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   SCORE-ADT   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "abstracties.rkt")
(require "positie-adt.rkt")
(require "teken-adt.rkt")
(provide maak-adt-score)


(define (maak-adt-score x-pos y-pos)
  (let ((score-pos (maak-adt-positie x-pos y-pos))
        (score 0))

    (define (verwijder! teken-adt)
      ((teken-adt 'verwijder-munt!)))
      
    (define (teken! teken-adt)
      ((teken-adt 'teken-score!) dispatch-score score))

    (define (update-score! msg teken-adt)
      (case msg
        ('munt (set! score (+ score 100)))
        ('pil (set! score (+ score 50)))
        ('insect (set! score (+ score 75))))
      
      (teken! teken-adt))

    (define (reset! teken-adt)
      (set! score 0)
      (teken! teken-adt))

    
    (define (dispatch-score msg)
      (cond ((eq? msg 'x) (score-pos 'x))
            ((eq? msg 'y) (score-pos 'y))
            ((eq? msg 'reset!) reset!)
            ((eq? msg 'verwijder!) verwijder!)
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'update!) update-score!)))
    dispatch-score))