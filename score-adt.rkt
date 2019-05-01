#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;   SCORE-ADT   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "abstracties.rkt")
(require "positie-adt.rkt")
(require "teken-adt.rkt")
(require 2htdp/batch-io)
(provide maak-adt-score)


(define (maak-adt-score)
  (let ((score 0)
        (highscore 0))

    
      
    (define (teken! teken-adt)
      ((teken-adt 'teken-score!) dispatch-score))

    (define (update-score! teken-adt type)
      (case type
        ('munt (set! score (+ score 100)))
        ('insect (set! score (+ score 75)))
        ('pil (set! score (+ score 50))))
      (teken! teken-adt))

    (define (next-level! teken-adt)
      (when (> score highscore)
        (write-file "highscore.txt" (number->string score))
        (set! highscore score)))

    (define (reset! teken-adt)
      (set! score 0)
      (teken! teken-adt))

    (define (init)
      (write-file "highscore.txt" (number->string 0)))

    (init)

    
    (define (dispatch-score msg)
      (case msg
        ('reset! reset!)
        ('next-level! next-level!)
        ('teken! teken!)
        ('score score)
        ('highscore highscore)
        ('update! update-score!)))
    dispatch-score))