(#%require (only racket random))
(#%require "Graphics.rkt")

(load "Teken-ADT.rkt")

;Venster grootte
(define px-venster-hoogte  400)
(define px-venster-breedte 400)

(define px-element-breedte 20)
(define px-element-hoogte 20)


;Aantal cellen per venster
(define elementen-per-rij (/ px-venster-breedte px-element-breedte))
(define elementen-per-kolom (/ px-venster-hoogte px-element-hoogte))

(define element-breedte (/ 1 elementen-per-rij))
(define element-hoogte (/ 1 elementen-per-kolom))

;;Random-positie berekenen
;(define (random-x-positie)
;  (let ((random-kolom (random elementen-per-rij)))
;    (* random-kolom element-breedte)))
;
;(define (random-y-positie)
;  (let ((random-rij (random elementen-per-kolom)))
;    (* random-rij element-hoogte)))
;
;
;
;;TODO: maak random bermpositie
;
;(define (random-berm-pos)
;  (...))
;
;
;;positie ADT 
;(define (maak-positie x y)
;  (let ((x-pos x)
;        (y-pos y))
;    
;    (define (set-pos! x y)
;      (set! x-pos x)
;      (set! y-pos y))
;      
;    
;    (define (dispatch-pos msg)
;      (cond ((eq? msg 'set-x!) set-x!)
;            ((eq? msg 'set-y!) set-y!)
;            ((eq? msg 'get-x) x-pos)
;            ((eq? msg 'get-y) y-pos)))
;    
;    dispatch-pos))
;
;
;;munt ADT
;
;
;(define (maak-munt) 
;  (let ((munt-positie (random-berm-pos ... ...))); maak aan met positie ADT, 
;
;    (define (delete!)
;      )
;
;    
;    (define (dispatch-munt msg)
;      (cond ((eq? msg 'delete!) delete!)
;            ((eq? msg 'x) x)
;            ((eq? msg 'y) y)))))
;
;;Kikker ADT 
;(define (maak-kikker)
;  (let ((kikker-positie (maak-positie ... ...)) ;geef start-positie
;        (munt-gegeten 0)) 
;
;    (define)
;
;    (define (kikker?))
;    
;    (define (beweeg! richting)
;      (cond ((eq richting 'boven) ...) 
;            ((eq richting 'onder) ...)
;            ((eq richting 'recht) ...)
;            ((eq richting 'links) ...)))
;    
;    (define (op-munt?)
;      (if (and (eq? (kikker-positie 'get-x) (munt-positie 'get-x))
;               (eq? (kikker-positie 'get-y) (munt-positie 'get-y)))
;          ((munt 'delete!)) ;maak je verwijzing naar munten anders?
;      (set! munt-gegeten (+ munt-gegeten 1)))))
;
;    (define (dispatch msg)
;      (cond ((eq? msg 'op-munt?) op-munt)
;            ((eq? msg ')))

;Spelwereld ADT


;TODO: 1. maak venster (yes) 2. voeg basisloop eraan toe 2. maak berm 3. test kikker 4. interactie muntjes kikker
      
(define (maak-adt-spel)

  (define teken-adt (maak-teken-adt "Frogger" px-venster-hoogte px-venster-breedte))

   (define (toets-functie-tijdens-spel type toets)
    (debug "Ingedrukte toets: " toets)
    (cond
      ((eq? toets 'right)
       ((slang-adt 'richting!) 'rechts))
      ((eq? toets 'left)
       ((slang-adt 'richting!) 'links))
      ((eq? toets 'up)
       ((slang-adt 'richting!) 'omhoog))
      ((eq? toets 'down)
       ((slang-adt 'richting!) 'beneden))))

  ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
  ((teken-adt 'set-toets-functie!) toets-functie-tijdens-spel)

  (define (dispatch-frogger msg)
    (cond ((eq? msg 'start) start)))

  dispatch-frogger)


(define spel (maak-adt-spel))

((spel 'start))
  

  


  
    



    

    



    
