#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Kikker ADT ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide (all-defined-out))

(define kikker-refresh-rate 25)

(define (maak-adt-kikker x-pos y-pos)
  (let ((kikker-pos (maak-adt-positie x-pos
                                          y-pos))
        (beweging 'doe-niets)
        (onschendbaar? #f)
        (onschendbaarheid-tijd 0)) ;beweging is de richting waarin de kikker zal updaten na beweeg!

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
        (normaal! teken-adt)))

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
    
    (define (update-onschendbaarheid! delta-tijd teken-adt)
      (set! onschendbaarheid-tijd (+ delta-tijd onschendbaarheid-tijd))
      (when (> onschendbaarheid-tijd 5000)
        (normaal! teken-adt)))

    (define (verander-onschendbaarheid-hogere-orde teken-adt)
        (set! onschendbaar? (not onschendbaar?))
        (if onschendbaar? (set! kikker-refresh-rate 140)
          (set! kikker-refresh-rate 25))
        ((teken-adt 'verander-kleur-kikker!) dispatch-kikker))


    (define (onschendbaar! teken-adt)
      (when (not onschendbaar?)
        (verander-onschendbaarheid-hogere-orde teken-adt)))

    (define (normaal! teken-adt)
      (when onschendbaar? 
        (verander-onschendbaarheid-hogere-orde teken-adt))
      (set! onschendbaarheid-tijd 0))
    


    (define (dispatch-kikker msg)
      (case msg
        ('x (kikker-pos 'x))
        ('y (kikker-pos 'y))
        ('pos (kikker-pos 'pos))
        ('teken! teken!)
        ('beweging! set-beweging!)
        ('beweeg! (beweeg!))
        ('beweging beweging)
        ('reset! reset!)
        ('onschendbaar! onschendbaar!)
        ('normaal! normaal!)
        ('onschendbaar? onschendbaar?)
        ('update-onschendbaarheid! update-onschendbaarheid!)
        ('volgende volgende-positie)))
        
    dispatch-kikker))