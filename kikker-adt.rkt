#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Kikker ADT ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide maak-adt-kikker)



(define (maak-adt-kikker x-pos y-pos)
  (let ((kikker-pos (maak-adt-positie x-pos
                                          y-pos))
        (beweging 'doe-niets)
        (onschendbaar? #f)) ;beweging is de richting waarin de kikker zal updaten na beweeg!

    (define px-afgelegde-afstand 10)

    ;berekent volgende positie aan de hand van de tag beweging
    (define (volgende-positie)
      (let ((x (kikker-pos 'x))
            (y (kikker-pos 'y)))
      (cond ((eq? 'omhoog beweging)
             (cons x (- y px-element-hoogte)))
            ((eq? 'omlaag beweging)
             (cons x (+ y px-element-hoogte)))
            ((eq? 'links beweging)
             (cons (- x px-afgelegde-afstand) y))
            ((eq? 'rechts beweging)
             (cons (+ x px-afgelegde-afstand) y))
            ((eq? 'doe-niets beweging)
             (cons x y)))))

    (define (reset! teken-adt)
      (set-x&y! kikker-pos x-pos y-pos)
      (set! beweging 'doe-niets)
      
      (when onschendbaar?
        (verander-kleur! teken-adt)))

    ;tekent kikker op nieuwe positie
    (define (teken! teken-adt)
      ((teken-adt 'teken-kikker!) dispatch-kikker))

    ;verandert tag beweging
    (define (set-beweging! b)
      (set! beweging b))

    ;Verandert coordinaten kikker
    (define (beweeg!)
      (let* ((volgende (volgende-positie)))
        (set-x&y! kikker-pos (car volgende) (cdr volgende))
        (set-beweging! 'doe-niets)))
    
    (define (verander-kleur! teken-adt)
      (set! onschendbaar? (not onschendbaar?))
      ((teken-adt 'verander-kleur-kikker!) dispatch-kikker))


    (define (dispatch-kikker msg)
      (cond ((eq? msg 'x) (kikker-pos 'x))
            ((eq? msg 'y) (kikker-pos 'y))
            ((eq? msg 'pos) (kikker-pos 'pos))
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'beweging!) set-beweging!)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'beweging) beweging)
            ((eq? msg 'reset!) reset!)
            ((eq? msg 'verander-kleur!) verander-kleur!)
            ((eq? msg 'onschendbaar?) onschendbaar?)
            ((eq? msg 'volgende) volgende-positie)))
        
    dispatch-kikker))