#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   AUTO-ADT   ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(require "abstracties.rkt")
(require "positie-adt.rkt")

(define (maak-adt-auto x-pos y-pos type)
  (let ((auto-pos (maak-adt-positie x-pos y-pos))
        (rest-seconds (random-range)) ;optioneel (enkel voor auto met type 3)
        (vertraagd? #f)) 

    (define (teken! teken-adt)
      ((teken-adt 'teken-auto!) dispatch-auto type))
    
    (define (rij-auto-links! afstand)
          ((auto-pos 'x!) (- (auto-pos 'x) afstand)))
   
    (define (update! teken-adt kikker-adt) ;deel op in deelprocedures
      (cond ((< (auto-pos 'x) (- px-element-breedte)) ((auto-pos 'x!) x-pos)) ((auto-pos 'x!) y-pos))
      (cond 
        [(= type normale-auto-type)
         (rij-auto-links! normale-auto-afstand)]
        
        [(= type vertraagde-auto-type)
         (if (= (auto-pos 'y) (kikker-adt 'y))
             (rij-auto-links! vertraagde-normale-auto-afstand)
             (rij-auto-links! normale-auto-afstand))]
        
        [(= type volgende-auto-type)
         (when [and (member (kikker-adt 'y) onderste-rijstroken)
                    (< (+ (kikker-adt 'x) px-element-breedte)
                       (auto-pos 'x))]
           ((auto-pos 'y!) (kikker-adt 'y)))
         (rij-auto-links! normale-auto-afstand)]
        
        [(= type rare-auto-type)
             (when (<= rest-seconds 0)
               (set! vertraagd? (not vertraagd?))
               (set! rest-seconds (random-range)))
             (if vertraagd? (rij-auto-links! vertraagde-normale-auto-afstand)
                 (rij-auto-links! normale-auto-afstand))
             (set! rest-seconds (- rest-seconds 1))])
      
      (teken! teken-adt)) ;teken! procedure moet naar auto-lijst

    (define (herteken-bij-botsing! botsing-y-pos teken-adt)
      ((auto-pos 'y!)  (car (remove botsing-y-pos onderste-rijstroken)))
      (teken! teken-adt))

    (define (reset!)
      (set-x&y! auto-pos x-pos y-pos))



    (define (dispatch-auto msg)
      (case msg
        ('x (auto-pos 'x))
        ('y (auto-pos 'y))
        ('pos (auto-pos 'pos))
        ('update! update!)
        ('teken! teken!)
        ('reset!( reset!))
        ('botsing herteken-bij-botsing!)))

    dispatch-auto))