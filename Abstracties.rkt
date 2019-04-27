#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ABSTRACTIES ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide (all-defined-out))
         
         
;Venster grootte
(define px-venster-hoogte  390)
(define px-venster-breedte 420)

(define px-element-hoogte  30)
(define px-element-breedte 30)


;Aantal cellen per venster
(define elementen-per-rij (/ px-venster-breedte px-element-breedte))
(define elementen-per-kolom (/ px-venster-hoogte px-element-hoogte))

;Grenzen scherm (voor collision detection)
(define bovenaan-scherm 0)
(define onderaan-scherm (* 12 px-element-hoogte))
(define links-scherm 0)
(define rechts-scherm (* 13 px-element-breedte))


(define pos-baan '(11 10 9 7 6 5)) ;gebruikt voor muntgenerator
(define pos-rivier (map (lambda (rij-nummer) (* rij-nummer px-element-hoogte))  '(1 2 3)))
(define onderste-rijstroken (map (lambda (rij-nummer) (* rij-nummer px-element-hoogte)) '(9 10 11)))
(define y-pos-berm-met-struik (* 8 px-element-hoogte)) ;Abstractie voor x-positie struiken

(define level 5)

;; Kikker configuratie

(define auto-refresh-rate 30)

;posities die nodig zijn
(define midden-x (* (- (/ elementen-per-rij 2) 1) px-element-breedte))

;Startpositie Kikker
(define kikker-x-startpos midden-x)
(define kikker-y-startpos onderaan-scherm) 



; Procedure die posities struiken berekent adhv level
(define (struik-pos level)
  (map (lambda (x) (* px-element-hoogte x))
       (cond ((> level 5)
              '(0 4 9 13))
             (else '(1 12)))))


;; Eetbaren-objecten ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; hogere orde ;;
(define (reset-eetbaar-adt! teken-adt dispatch)
  (let ((positie-adt (dispatch 'pos)))
    (set-x&y! positie-adt (random-x) (random-y))
    ((teken-adt 'verwijder-eetbaar-adt!) dispatch)
    ((teken-adt 'teken-eetbaar-adt!) dispatch)))

(define (verwijder-eetbaar-adt! teken-adt adt score-adt)
  ((score-adt 'update!) teken-adt (adt 'type))
  (set-x&y! (adt 'pos) (- 1) (- 1))
  ((teken-adt 'verwijder-eetbaar-adt!) adt))
  
  


;Munt ADT
(define (random-x)
  (* (random 14) px-element-breedte))
(define (random-y) ;random uit lijst pos-baan gekozen
  (* (list-ref pos-baan (random 6)) px-element-hoogte))



;;; Teken ADT ;;;

; hogere-orde Teken procedure (verwacht tile en adt)

(define (hogere-orde-teken! adt tile)
  (let* ((x-pos (adt 'x))
         (y-pos (adt 'y)))
    ((tile 'set-x!) x-pos)
    ((tile 'set-y!) y-pos)))


;Abstractie die overlap met munt en kikker berekent
(define (controleer-x-volgende object-coordinaat volgende-kikker)
  (define (rechtergrens coordinaat)
    (+ px-element-breedte coordinaat))
  
  (define (linkergrens coordinaat)
    (- coordinaat px-element-breedte))
  (< (linkergrens object-coordinaat)
     volgende-kikker
     (rechtergrens object-coordinaat)))


;overloopt coordinaten van struik en checkt overlap
(define (controleer-lijst kikker-x-coordinaat lijst)
  (ormap (lambda (x)
           (controleer-x-volgende x kikker-x-coordinaat))
            lijst))

(define (set-x&y! adt new-x new-y)
  ((adt 'x!) new-x)
  ((adt 'y!) new-y))



;;;; AUTO-ADT Abstracties ;;;;

(define normale-auto-type     0)
(define vertraagde-auto-type  1)
(define volgende-auto-type    2)
(define rare-auto-type        3)

(define normale-auto-afstand            3)
(define vertraagde-normale-auto-afstand 1)

(define (random-range)
  (random 20 60))









