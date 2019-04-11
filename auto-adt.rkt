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
        (vertraagd? #f)) ;dit zal later een assoc-lijst worden (met type)

    (define (teken! teken-adt nummer)
      ((teken-adt 'teken-auto!) dispatch-auto nummer))


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
             (rij-auto-links! normale-auto-afstand))] ;maak hiervan een grote conditional die delegeert
        [(= type volgende-auto-type)
         ((auto-pos 'y!) (kikker-adt 'y))
         (rij-auto-links! vertraagde-normale-auto-afstand)]
        [(= type rare-auto-type)
             (when (<= rest-seconds 0)
               (set! vertraagd? (not vertraagd?))
               (set! rest-seconds (random-range)))
             (if vertraagd? (rij-auto-links! vertraagde-normale-auto-afstand)
                 (rij-auto-links! normale-auto-afstand))
             (set! rest-seconds (- rest-seconds 1))])
      (teken! teken-adt type)) ;teken! procedure moet naar auto-lijst



    (define (dispatch-auto msg)
      (cond ((eq? msg 'x) (auto-pos 'x))
            ((eq? msg 'y) (auto-pos 'y))
            ((eq? msg 'pos) (auto-pos 'pos))
            ((eq? msg 'update!) update!)
            ((eq? msg 'teken!) teken!)))

    dispatch-auto))

;idee: Om te checken met auto's check car met alle elementen van cdr, ga dan naar volgende.
(define (maak-adt-auto-lijst)
  (let* ((lijst '()))

    
    ;(define empty-list '(0 0 0 0))
    (define (init)
      (define coordinate-list '(5 6 7 9))
      (define type-list '(0 1 2 3))
    
      (set! lijst (map (lambda (x y) (cons x y)) coordinate-list type-list))
      (set! lijst (map (lambda (x) (maak-adt-auto 390 (* (car x) px-element-hoogte)
                                                  (cdr x))) lijst)))

    (init)
    
    

    (define (update! teken-adt kikker-adt)
      (for-each (lambda (x) ((x 'update) teken-adt kikker-adt)) lijst)) ;verder herwerken

    (define (dispatch-auto-lijst msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'lijst) lijst)
            ((eq? msg 'reset!) (init))
            ((eq? msg 'x) (map (λ (auto) (auto 'x)) lijst))
            ((eq? msg 'y) (map (λ (auto) (auto 'y)) lijst))
            ((eq? msg 'pos) (map (λ (auto) (auto 'pos)) lijst))))

    dispatch-auto-lijst))