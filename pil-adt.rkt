#lang racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   PIL-ADT   ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "abstracties.rkt")
(require "positie-adt.rkt")

(provide maak-adt-pil)


(define (maak-adt-pil x-pos y-pos)
  (let ((pil-pos (maak-adt-positie x-pos y-pos))
        (verzameld? #f))

    (define (verwijder! teken-adt score-adt)
      ((score-adt 'update!) 'pil teken-adt)
      (set-x&y! pil-pos (- 1) (- 1))
      (set! verzameld? #t)
      ((teken-adt 'verwijder-pil!)))
      
    (define (teken! teken-adt)
      ((teken-adt 'teken-pil!) dispatch-pil))

    (define (reset! teken-adt)
      (set! verzameld? #f)
      ((pil-pos 'x!) (random-x))
      ((pil-pos 'y!) (random-y))
      ((teken-adt 'teken-pil!) dispatch-pil))

    
    (define (dispatch-pil msg)
      (cond ((eq? msg 'x) (pil-pos 'x))
            ((eq? msg 'y) (pil-pos 'y))
            ((eq? msg 'reset!) reset!)
            ((eq? msg 'verwijder!) verwijder!)
            ((eq? msg 'verzameld?) verzameld?)
            ((eq? msg 'teken!) teken!)))
    dispatch-pil))
