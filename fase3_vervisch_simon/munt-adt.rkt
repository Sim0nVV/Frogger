#lang racket 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Munt ADT ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide maak-adt-munt)

(define (maak-adt-munt munt-pos) 
  (let ((verzameld? #f)
        (type 'munt))

    (define (verwijder! teken-adt score-adt)
      (verwijder-eetbaar-adt! teken-adt dispatch-munt score-adt)
      (set! verzameld? #t))

    (define (teken! teken-adt)
      ((teken-adt 'teken-eetbaar-adt!) dispatch-munt))

    (define (reset! teken-adt)
      (set! verzameld? #f)
      (reset-eetbaar-adt! teken-adt dispatch-munt))

    
    (define (dispatch-munt msg)
      (case msg
        ('x (munt-pos 'x))
        ('y (munt-pos 'y))
        ('pos munt-pos)
        ('type type)
        ('verwijder! verwijder!)
        ('teken! teken!)
        ('verzameld? verzameld?)
        ('reset! reset!)))
    dispatch-munt))  
