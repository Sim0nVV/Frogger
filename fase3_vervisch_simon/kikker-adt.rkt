#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Kikker ADT ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "positie-adt.rkt")
(require "abstracties.rkt")
(provide (all-defined-out))

(define kikker-refresh-rate 25)

(define (maak-adt-kikker x-pos y-pos)
  (let ((kikker-pos (maak-adt-positie x-pos y-pos))
        (beweging 'doe-niets) ;beweging is de richting waarin de kikker zal updaten na beweeg!
        (onschendbaar? #f)
        (onschendbaarheid-tijd 5000)
        (levens  3)) 

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

    ;wanneer kikker dood is of nieuw level start
    (define (reset! teken-adt)
      (set-x&y! kikker-pos x-pos y-pos)
      (set! beweging 'doe-niets)
      (normaal! teken-adt)
      ((teken-adt 'teken-levens!) dispatch-kikker))

    ;vermindert aantal levens
    (define (dood! teken-adt)
      (set! levens (- levens 1))
      ((teken-adt 'teken-levens!) dispatch-kikker))           

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

    ;onschendbaarheid-loop
    (define (check-onschendbaarheid! delta-tijd teken-adt)
      (set! onschendbaarheid-tijd (- onschendbaarheid-tijd delta-tijd))
      ((teken-adt 'update-seconden!) dispatch-kikker)
      (when (< onschendbaarheid-tijd 0)
        (normaal! teken-adt)))

    (define (verander-onschendbaarheid-hogere-orde teken-adt)
      (if onschendbaar? (set! kikker-refresh-rate 100)
          (set! kikker-refresh-rate 25))
      ((teken-adt 'verander-kleur-kikker!) dispatch-kikker))

    ;maakt kikker onschendbaar
    (define (onschendbaar! teken-adt)
      (set! onschendbaar? #t)
      (verander-onschendbaarheid-hogere-orde teken-adt))

    ;maakt kikker normaal
    (define (normaal! teken-adt)
      (set! onschendbaar? #f)
      (teken-adt 'verwijder-seconden!)
      (set! onschendbaarheid-tijd 5000)
      (verander-onschendbaarheid-hogere-orde teken-adt))

    (define (levens-reset! teken-adt)
      (set! levens 3)
      ((teken-adt 'teken-levens!) dispatch-kikker))

 
    (define (dispatch-kikker msg)
      (case msg
        ('x (kikker-pos 'x))
        ('y (kikker-pos 'y))
        ('pos (kikker-pos 'pos))
        ('seconden onschendbaarheid-tijd)
        ('teken! teken!)
        ((init reset!) reset!)
        ('beweging! set-beweging!)
        ('beweeg! (beweeg!))
        ('beweging beweging)
        ('levens levens)
        ('levens-reset! levens-reset!)
        ('dood! dood!)
        ('onschendbaar! onschendbaar!)
        ('normaal! normaal!)
        ('onschendbaar? onschendbaar?)
        ('check-onschendbaarheid! check-onschendbaarheid!)
        ('volgende volgende-positie)))
        
    dispatch-kikker))