#lang racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   PIL-ADT   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "abstracties.rkt")
(require "positie-adt.rkt")

(provide maak-adt-pil)


(define (maak-adt-pil x-pos y-pos)
  (let ((pil-pos (maak-adt-positie x-pos y-pos))
        (verzameld? #f)
        (type 'pil))

    (define (verwijder! teken-adt score-adt)
      (verwijder-eetbaar-adt! teken-adt dispatch-pil score-adt)
      (set! verzameld? #t))
      
    (define (teken! teken-adt)
      ((teken-adt 'teken-eetbaar-adt!) dispatch-pil))

    (define (reset! teken-adt)
      (set! verzameld? #f)
      (reset-eetbaar-adt! teken-adt dispatch-pil))


    
    (define (dispatch-pil msg)
      (case msg
        ('x (pil-pos 'x))
        ('y (pil-pos 'y))
        ('pos pil-pos)
        ('type type)
        ('reset! reset!)
        ('verwijder! verwijder!)
        ('verzameld? verzameld?)
        ('teken! teken!)))
    dispatch-pil))
