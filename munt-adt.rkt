#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Munt ADT ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide maak-adt-munt)

(define (maak-adt-munt x-pos y-pos) 
  (let ((munt-pos (maak-adt-positie x-pos y-pos))
        (verzameld? #f)) 

    (define (verwijder! teken-adt score-adt)
      ((score-adt 'update!) 'munt teken-adt)
      (set-x&y! munt-pos (- 1) (- 1))
      (set! verzameld? #t)
      ((teken-adt 'verwijder-munt!)))

    (define (teken! teken-adt)
      ((teken-adt 'teken-munt!) dispatch-munt))

    (define (reset! teken-adt)
      (set! verzameld? #f)
      ((munt-pos 'x!) (random-x))
      ((munt-pos 'y!) (random-y))
      ((teken-adt 'teken-munt!) dispatch-munt))

    
    (define (dispatch-munt msg)
      (case msg
        ('x (munt-pos 'x))
        ('y (munt-pos 'y))
        ('verwijder! verwijder!)
        ('teken! teken!)
        ('verzameld? verzameld?)
        ('reset! reset!)))
    dispatch-munt))
