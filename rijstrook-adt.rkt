#lang racket


(require "abstracties.rkt")

; Collision detection: idee;

; 1) check-volgende met rijstrook
; 2) geef object of positie terug
; 3) reset het spel/level indien nodig. Stap 4 indien toepasselijk.
; 4) als object -> 'collision met kikker en 'collision met object (veroorzaakt door kikker).
; 5) reset.


;probleem: hoe verander je de rijstrook van voorwerpen?

(provide (all-defined-out))


(define (maak-adt-rijstrook)
  (let ((rijstrook-vector (make-vector 14 '())))

    (define uit-het-scherm-nr 13)


    ;assq of member (afhankelijk van het feit of er een cdr is of niet). (bij rijstrook)
    #;(define (collision object-adt)
      (member ))

    (define (verwijder-uit-nr! rijstrook-nr object-adt)
        (vector-set! rijstrook-vector rijstrook-nr
                     (remq (vector-ref rijstrook-vector rijstrook-nr) object-adt)))
    
    (define (voeg-toe-nr! rijstrook-nr object-adt)
        (vector-set! rijstrook-vector rijstrook-nr
                     (cons object-adt (vector-ref rijstrook-vector rijstrook-nr))))

    (define (voeg-toe! object-adt)
      (voeg-toe-nr! (convert-naar-coord (object-adt 'y)) object-adt))
                   
    
    
    (define (verwijder-uit-voeg-toe-nr! oud-rijstrook-nr nieuw-rijstrook-nr object-adt) ;abstractie
      (verwijder-uit-nr! oud-rijstrook-nr object-adt)
      (voeg-toe-nr! nieuw-rijstrook-nr object-adt))
 
    (define (nieuwe-rijstrook! nieuw-rijstrook-nr object-adt)
      (let ((oud-rijstrook-nr (/ (object-adt 'y) px-element-hoogte)))
        (verwijder-uit-voeg-toe-nr! oud-rijstrook-nr nieuw-rijstrook-nr object-adt)))

    (define (verwijder! object-adt)
      (let ((oud-rijstrook-nr (/ (object-adt 'y) px-element-hoogte)))
        (verwijder-uit-voeg-toe-nr! oud-rijstrook-nr uit-het-scherm-nr object-adt)))

    

    (define (init!)
      (for-each [lambda (rijstrook-nr water-lijst)
                  (vector-set! rijstrook-vector rijstrook-nr water-lijst)]
                  pos-rivier
                '((0 1 2 9 10 11) (2 3 4 7 8 9) (5 6 7)))
      (vector-set! rijstrook-vector (convert-naar-coord y-pos-berm-met-struik)
                   (convert-naar-coord (struik-pos level))))

    (define (haal-lijst-op nr)
      (convert-naar-pixels (vector-ref rijstrook-vector nr)))

    (init!)


    
    
    (define (dispatch msg)
      (case msg
        ('nieuwe-rijstrook! nieuwe-rijstrook!)
        ('verwijder verwijder!)
        ('vind-rijstrook haal-lijst-op)
        ('voeg-toe! voeg-toe!)
        ('display (display rijstrook-vector))))
    
    dispatch))


