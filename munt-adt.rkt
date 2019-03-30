#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Munt ADT ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide maak-adt-munt)

(define (maak-adt-munt) 
  (let ((munt-positie (maak-adt-positie (random-x) (random-y)))) 

    (define (verwijder! teken-adt)
      ((teken-adt 'verwijder-munt!)))

    (define (teken! teken-adt)
      ((teken-adt 'teken-munt!) dispatch-munt))

    (define (verander-plaats! teken-adt)
      ((munt-positie 'x!) (random-x))
      ((munt-positie 'y!) (random-y))
      ((teken-adt 'teken-munt!) dispatch-munt))

    
    (define (dispatch-munt msg)
      (cond ((eq? msg 'x) (munt-positie 'x))
            ((eq? msg 'y) (munt-positie 'y))
            ((eq? msg 'verwijder!) verwijder!)
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'verander-plaats!) verander-plaats!)))
    dispatch-munt))
