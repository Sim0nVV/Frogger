(#%require (only racket random))
(#%require "Graphics.rkt")

(load "Teken-ADT.rkt")
(load "helpers.rkt")
(load "kikker-adt.rkt")
(load "positie-adt.rkt")
(load "munt-adt.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ABSTRACTIES ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(define onderaan-scherm 12)
(define links-scherm 0)
(define rechts-scherm 13)


(define pos-baan '(11 10 9 7 6 5)) ;gebruikt voor muntgenerator
(define x-pos-berm-met-struik 8) ;Abstractie voor x-positie struiken

(define level 5)

;; Kikker configuratie
(define kikker-refresh-rate 20)

;posities die nodig zijn
(define midden-x (- (/ elementen-per-rij 2) 1))

;Startpositie Kikker
(define kikker-x-startpos midden-x)
(define kikker-y-startpos onderaan-scherm)



; Procedure die posities struiken berekent adhv level
(define (struik-pos level)
  (cond ((> level 5)
         '(0 4 9 13))
        (else '(1 12))))

(define (maak-adt-auto x-pos y-pos)
  (let ((auto-pos (maak-adt-positie x-pos y-pos))) ;dit zal later een assoc-lijst worden (met tag)
    

    (define (teken! teken-adt)
      ((teken-adt 'teken-auto!) dispatch-auto))
    (define auto-step 3)
    (define (update! teken-adt)
      (cond ((< (auto-pos 'x) 0) ((auto-pos 'x!) x-pos)) ((auto-pos 'x!) y-pos))
      
          ((auto-pos 'x!) (- (auto-pos 'x) auto-step))
            
      
           (teken! teken-adt))

          (define (dispatch-auto msg)
            (cond ((eq? msg 'x) (auto-pos 'x))
                  ((eq? msg 'y) (auto-pos 'y))
                  ((eq? msg 'update!) update!)
                  ((eq? msg 'teken!) teken!)
                  ((eq? msg 'volgende) volgende-positie)))

          dispatch-auto))


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;; Spel-adt ;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define (maak-adt-spel)


      ;objectenaanmaak
      (define kikker-adt (maak-adt-kikker))
      (define munt-adt (maak-adt-munt))
      (define teken-adt (maak-adt-teken "Frogger" px-venster-hoogte px-venster-breedte))
      (define auto-adt (maak-adt-auto 390 (* 5 px-element-hoogte) ))
      (define auto-adt2 (maak-adt-auto 390 (* 6 px-element-hoogte) ))
  

      ;keyboard-inputprocedure
      (define (toets-functie-tijdens-spel type toets)
        (if (eq? type 'down)
            (cond
              ((eq? toets 'right)
               ((kikker-adt 'beweging!) 'rechts))
              ((eq? toets 'left)
               ((kikker-adt 'beweging!) 'links))
              ((eq? toets 'up)
               ((kikker-adt 'beweging!) 'omhoog))
              ((eq? toets 'down)
               ((kikker-adt 'beweging!) 'omlaag)))))

      ;Abstractie die overlap met munt en kikker berekent
      (define (controleer-x-volgende asset-coordinaat volgende)
        (define (rechts-pixel coordinaat)
          (+ 1 coordinaat))
        (define (links-pixel coordinaat)
          (- coordinaat 1))
        (or (and (<= asset-coordinaat volgende)
                 (< volgende (rechts-pixel asset-coordinaat)))
            (< (links-pixel asset-coordinaat) volgende asset-coordinaat)))


      ;overloopt coordinaten van struik en checkt overlap
      (define (controleer-struiken kikker-volgende)
        (define bool #f)
        (for-each (lambda (struik-x)
                    (if (eq? bool #f)
                        (set! bool (controleer-x-volgende struik-x kikker-volgende))))
                  (struik-pos level))
        bool)
  
      (define (start)
        (define kikker-tijd  0)
        ;; Deze functie voert men elke `tick` uit.
        (define (spel-lus-functie delta-tijd)
          (set! kikker-tijd  (+ kikker-tijd delta-tijd))
          ((auto-adt 'update!) teken-adt)
          ((auto-adt2 'update!) teken-adt)

          ;Refresh kikker
          (if (> kikker-tijd kikker-refresh-rate)
              (begin
                ;; Verander positie van de kikker (Collision detection)
                (let* ((kikker-y (kikker-adt 'y))
                       (kikker-x (kikker-adt 'x))
                       (munt-x (munt-adt 'x))
                       (munt-y (munt-adt 'y))
                       (kikker-volgende-y (cdr ((kikker-adt 'volgende) kikker-x kikker-y)))
                       (kikker-volgende-x (car ((kikker-adt 'volgende) kikker-x kikker-y))))

                  (cond ((or (not (<= links-scherm kikker-volgende-x rechts-scherm))
                             (not (<= bovenaan-scherm kikker-volgende-y onderaan-scherm)) ;Niet uit het scherm 
                             (and (controleer-struiken kikker-volgende-x)
                                  (= kikker-volgende-y x-pos-berm-met-struik))) ;botsing met struik
                         ((kikker-adt 'beweging!) 'doe-niets))
                        ((and (= munt-y kikker-volgende-y)
                              (controleer-x-volgende munt-x kikker-volgende-x))
                         ((munt-adt 'verwijder!) teken-adt)
                         ;((munt-adt 'verander-plaats!) teken-adt) ;uncomment deze lijn voor randomgeneratie munt
                         ((kikker-adt 'beweeg!))); botsing met munt
                        (else
                         ((kikker-adt 'beweeg!)))))
                (set! kikker-tijd 0)))

          ;;Herteken kikker
          ((kikker-adt 'teken!) teken-adt))
      
      
        ;; Zet de callbacks via de library.
        ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
        ((teken-adt 'set-toets-functie!) toets-functie-tijdens-spel))
  
      ;Teken achtergrond en munt
      ((teken-adt 'teken-scherm!))
      ((munt-adt 'teken!) teken-adt)

      ((auto-adt 'teken!) teken-adt)


      (define (dispatch-frogger msg)
        (cond ((eq? msg 'start) start)))
  
      dispatch-frogger)


    ;;;;;;;;;;;;;;;;;
    ;; Entry Point ;;
    ;;;;;;;;;;;;;;;;;

    (define spel (maak-adt-spel))

    ((spel 'start))
    