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
  (let ((score 0) ;score is huidige score
        (level-score 0) ;level-score is de score aan de start van een level
        (highscore (string->number (file->string "highscore.txt"))))

    
    (define (teken! teken-adt)
      ((teken-adt 'teken-score!) dispatch-score))

    ;verhoogt score
    (define (update-score! teken-adt adt)
      (case (adt 'type)
        ('munt (set! score (+ score 100)))
        ('insect (case (adt 'soort)
                   ((0) (set! score (+ score 50)))
                   ((1) (set! score (+ score 75)))
                   ((2) (set! score (+ score 100)))
                   ((3) (set! score (+ score 150)))))
        ('pil (set! score (+ score 50))))
      (teken! teken-adt))

    ;past .txt bestand aan
    (define (write-new-highscore hs)
      (write-file "highscore.txt" (number->string hs))
      (set! highscore hs))

    ;verandert level-score
    (define (volgend-level! teken-adt)
      (set! level-score score)
      (teken! teken-adt))

    (define (reset-level! teken-adt)
      (set! score level-score)
      (teken! teken-adt))

    ;indien op h gedrukt wordt tijdens spel
    (define (reset-highscore! teken-adt)
      (write-new-highscore 0)
      (teken! teken-adt))

    
    (define (einde-spel! teken-adt)
      (when (> score highscore)
        (write-new-highscore score))
      (set! score 0)
      (set! level-score 0)
      (teken! teken-adt))

    
    (define (dispatch-score msg)
      (case msg
        ('reset-level! reset-level!)
        ('volgend-level! volgend-level!)
        ('teken! teken!)
        ('score score)
        ('highscore highscore)
        ('einde-spel! einde-spel!)
        ('update! update-score!)
        ('reset-highscore! reset-highscore!)))
    dispatch-score))