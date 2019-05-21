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


;hogere-orde-procedure die lijst of nummer neemt en omzet naar pixel of coordinaat
(define (convert-naar-higher lijst bewerking)
  (if (list? lijst)
      (map (lambda (x) (bewerking px-element-hoogte x)) lijst)
      (bewerking lijst px-element-hoogte)))

;omzet naar pixel
(define (convert-naar-pixels lijst)
  (convert-naar-higher lijst *))

;omzet naar coordinaat
(define (convert-naar-coord lijst)
  (convert-naar-higher lijst quotient))


;Aantal cellen per venster
(define elementen-per-rij (/ px-venster-breedte px-element-breedte))
(define elementen-per-kolom (/ px-venster-hoogte px-element-hoogte))

;Grenzen scherm (voor collision detection)
(define bovenaan-scherm 0)
(define onderaan-scherm (convert-naar-pixels 12))
(define links-scherm 0)
(define rechts-scherm (convert-naar-pixels 13))


(define pos-baan '(11 10 9 7 6 5)) ;gebruikt voor muntgenerator
(define pos-rivier '(1 2 3))
(define onderste-rijstroken (convert-naar-pixels '(9 10 11)))
(define y-pos-berm-met-struik (convert-naar-pixels 8)) ;Abstractie voor x-positie struiken
(define boomstronk-vector (list->vector '('dummy (0 1 2 9 10 11) (2 3 4 7 8 9) (5 6 7))))




;posities die nodig zijn
(define midden-x (convert-naar-pixels (- (/ elementen-per-rij 2) 1)))

;Startpositie Kikker
(define kikker-x-startpos midden-x)
(define kikker-y-startpos onderaan-scherm) 


;; Eetbaren-objecten (munt, pil, insect) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; hogere orde ;;
(define (reset-eetbaar-adt! teken-adt dispatch)
  (let ((positie-adt (dispatch 'pos)))
    (set-x&y! positie-adt (random-x) (random-y))
    ((teken-adt 'verwijder-eetbaar-adt!) dispatch)
    ((teken-adt 'teken-eetbaar-adt!) dispatch)))

(define (verwijder-eetbaar-adt! teken-adt adt score-adt)
  ((score-adt 'update!) teken-adt adt)
  (set-x&y! (adt 'pos) (- 1) (- 1))
  ((teken-adt 'verwijder-eetbaar-adt!) adt))
  
  


;wordt gebruikt door munt, pil, insect en positie-ADT
(define (random-x)
  (convert-naar-pixels (random 14)))
(define (random-y) ;random uit lijst pos-baan gekozen
  (convert-naar-pixels (list-ref pos-baan (random 6))))


;;; Teken ADT ;;;

; hogere-orde Teken procedure 
(define (hogere-orde-teken! adt tile)
  (let* ((x-pos (adt 'x))
         (y-pos (adt 'y)))
    ((tile 'set-x!) x-pos)
    ((tile 'set-y!) y-pos)))


;Abstractie die overlap object en kikker berekent (minder strenge overlap)
(define (controleer-x-volgende volgende-kikker object-coordinaat)
  (define (rechtergrens coordinaat)
    (+ px-element-breedte coordinaat))
  (define (linkergrens coordinaat)
    (- coordinaat px-element-breedte))
  (< (linkergrens object-coordinaat)
     volgende-kikker
     (rechtergrens object-coordinaat)))

;Abstractie die overlap object en kikker berekent (strenge overlap)
(define (controleer-volledig-vakje kikker-x object-coordinaat)
  (let ((object-coordinaat-px (convert-naar-pixels object-coordinaat)))
    (define (rechtergrens coordinaat)
      (+ px-element-breedte coordinaat))
    (<= object-coordinaat-px
        kikker-x
        (rechtergrens object-coordinaat-px))))

(define (hogere-orde-or-map proc kikker-x lijst)
  (ormap (lambda (x) (proc kikker-x x)) lijst))

(define (controleer-lijst kikker-x-coordinaat lijst)
 (hogere-orde-or-map controleer-x-volgende kikker-x-coordinaat lijst))

(define (collision-detection-lijst-boom kikker-x-coordinaat lijst)
  (hogere-orde-or-map controleer-volledig-vakje kikker-x-coordinaat lijst))

(define (set-x&y! adt new-x new-y)
  ((adt 'x!) new-x)
  ((adt 'y!) new-y))