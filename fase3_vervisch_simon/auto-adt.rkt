#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   AUTO-ADT   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(require "abstracties.rkt")
(require "positie-adt.rkt")

(define normale-auto-type     0)
(define vertraagde-auto-type  1)
(define volgende-auto-type    2)
(define rare-auto-type        3)

(define normale-auto-afstand            3)
(define vertraagde-normale-auto-afstand 1)

(define (random-range)
  (random 20 60))

(define (maak-adt-auto start-x-pos start-y-pos soort)
  (let ((auto-pos (maak-adt-positie start-x-pos start-y-pos))
        (rest-seconds (random-range)) ;optioneel (enkel voor auto met type 3)
        (richting (if (> start-y-pos 210) 'links 'rechts)) 
        (vertraagd? #f)
        (type 'auto)) 

    (define (teken! teken-adt)
      ((teken-adt 'teken-eetbaar-adt!) dispatch-auto))

    ;Procedure checkt of auto naar links of rechts beweegt en verzet hem bepaalde afstand
    (define (rij-auto! afstand)
      (if (eq? richting 'links)
          ((auto-pos 'x!) (- (auto-pos 'x) afstand))
          ((auto-pos 'x!) (+ (auto-pos 'x) afstand))))

    ;herplaatst auto wanneer deze uit het scherm is.
    (define (uit-het-scherm?)
      (case richting
        ('links (when (< (auto-pos 'x) (- px-element-breedte))
                 (set-x&y! auto-pos 420 start-y-pos)))
        ('rechts (when (> (auto-pos 'x) (+ rechts-scherm px-element-breedte))
                   (set-x&y! auto-pos -30 start-y-pos)))))


    ;;; logica beweging auto's ;;; 
    (define (beweeg-normale-auto)
      (rij-auto! normale-auto-afstand))

    (define (beweeg-vertraagde-auto kikker-adt)
      (if (= (auto-pos 'y) (kikker-adt 'y))
             (rij-auto! vertraagde-normale-auto-afstand)
             (rij-auto! normale-auto-afstand)))

    (define (beweeg-volgende-auto kikker-adt)
      (when [and (member (kikker-adt 'y) onderste-rijstroken)
                    (< (+ (kikker-adt 'x) px-element-breedte)
                       (auto-pos 'x))]
           ((auto-pos 'y!) (kikker-adt 'y)))
         (rij-auto! normale-auto-afstand))

    (define (beweeg-rare-auto)
      (when (<= rest-seconds 0)
        (set! vertraagd? (not vertraagd?))
        (set! rest-seconds (random-range)))
      (if vertraagd? (rij-auto! vertraagde-normale-auto-afstand)
          (rij-auto! normale-auto-afstand))
      (set! rest-seconds (- rest-seconds 1)))
    
    (define (beweeg-auto teken-adt kikker-adt)
      (cond 
        [(= soort normale-auto-type)
         (beweeg-normale-auto)]
        [(= soort vertraagde-auto-type)
         (beweeg-vertraagde-auto kikker-adt)]
        [(= soort volgende-auto-type)
         (beweeg-volgende-auto kikker-adt)]
        [(= soort rare-auto-type)
         (beweeg-rare-auto)]))

    ;;; Algemene procedure om auto's te laten bewegen (roept procedures erboven op)
    (define (update! teken-adt kikker-adt)
      (uit-het-scherm?)
      (beweeg-auto teken-adt kikker-adt)
      ((teken-adt 'update-auto!) dispatch-auto)) 

    ;Als er een botsing is met de andere auto's, wordt de volgende auto hertekend
    (define (herteken-bij-botsing! teken-adt)
      ((auto-pos 'y!) 300)
      ((teken-adt 'update-auto!) dispatch-auto))

    (define (reset!)
      (set-x&y! auto-pos start-x-pos start-y-pos))

    (define (dispatch-auto msg)
      (case msg
        ('x (auto-pos 'x))
        ('y (auto-pos 'y))
        ('pos (auto-pos 'pos))
        ('update! update!)
        ('teken! teken!)
        ('reset! (reset!))
        ('type type)
        ('soort soort)
        ('richting richting)
        ('botsing herteken-bij-botsing!)))
    dispatch-auto))



  






