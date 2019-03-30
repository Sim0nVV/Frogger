#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ABSTRACTIES ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide px-venster-hoogte
         px-venster-breedte
         px-element-hoogte
         px-element-breedte
         elementen-per-rij
         elementen-per-kolom
         element-breedte
         element-hoogte
         bovenaan-scherm
         onderaan-scherm
         links-scherm
         rechts-scherm
         pos-baan
         y-pos-berm-met-struik
         level
         kikker-refresh-rate
         midden-x
         kikker-x-startpos
         kikker-y-startpos
         struik-pos
         random-x
         random-y)
         
         
;Venster grootte
(define px-venster-hoogte  390)
(define px-venster-breedte 420)

(define px-element-hoogte  30)
(define px-element-breedte 30)


;Aantal cellen per venster
(define elementen-per-rij (/ px-venster-breedte px-element-breedte))
(define elementen-per-kolom (/ px-venster-hoogte px-element-hoogte))

(define element-breedte (/ 1 elementen-per-rij)) ;13 breed en 14 hoog
(define element-hoogte (/ 1 elementen-per-kolom))


;Grenzen scherm (voor collision detection)
(define bovenaan-scherm 0)
(define onderaan-scherm (* 12 px-element-hoogte))
(define links-scherm 0)
(define rechts-scherm (* 13 px-element-breedte))


(define pos-baan '(11 10 9 7 6 5)) ;gebruikt voor muntgenerator
(define y-pos-berm-met-struik (* 8 px-element-hoogte)) ;Abstractie voor x-positie struiken

(define level 5)

;; Kikker configuratie
(define kikker-refresh-rate 15)

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


;Munt ADT
(define (random-x)
  (* (random 14) px-element-breedte))
(define (random-y) ;random uit lijst pos-baan gekozen
  (* (list-ref pos-baan (random 6)) px-element-hoogte))