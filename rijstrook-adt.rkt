#lang racket


(require "abstracties.rkt")

; Collision detection: idee;

; 1) check-volgende met rijstrook
; 2) geef object of positie terug
; 3) reset het spel/level indien nodig. Stap 4 indien toepasselijk.
; 4) als object -> 'collision met kikker en 'collision met object (veroorzaakt door kikker).
; 5) reset. 



#;(define (maak-adt-rijstrook)
  (let ((rijstrook-vector (make-vector 14 '())))

    (define uit-het-scherm-nr 13)


    ;assq of member (afhankelijk van het feit of er een cdr is of niet). (bij rijstrook)
    #;(define (collision-detection kikker-adt object-adt)
        (

    (define (verwijder-uit! rijstrook-nr object-adt)
        (vector-set! rijstrook-vector rijstrook-nr
                     (remq (vector-ref rijstrook-vector rijstrook-nr) object-adt)))
    (define (voeg-toe-aan! rijstrook-nr object-adt)
        (vector-set! rijstrook-vector rijstrook-nr
                     (cons object-adt (vector-ref rijstrook-vector rijstrook-nr))))
    
    
    (define (verwijder-uit-voeg-toe-aan! oud-rijstrook-nr nieuw-rijstrook-nr object-adt) ;abstractie
      (verwijder-uit! oud-rijstrook-nr object-adt)
      (voeg-toe-aan! nieuw-rijstrook-nr object-adt))

    

    
    (define (nieuwe-rijstrook! nieuw-rijstrook-nr object-adt)
      (let ((oud-rijstrook-nr (/ (object-adt 'y) px-element-hoogte)))
        (verwijder-uit-voeg-toe-aan! oud-rijstrook-nr nieuw-rijstrook-nr object-adt)))

    (define (verwijder! object-adt)
      (let ((oud-rijstrook-nr (/ (object-adt 'y) px-element-hoogte)))
        (verwijder-uit-voeg-toe-aan! oud-rijstrook-nr uit-het-scherm-nr object-adt)))


    
    
    (define (dispatch msg)
      (case msg
        ('nieuwe-rijstrook! nieuwe-rijstrook!)
        ('verwijder verwijder!)))
    dispatch))


