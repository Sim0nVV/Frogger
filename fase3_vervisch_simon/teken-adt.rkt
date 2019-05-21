#lang racket 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Teken ADT ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "graphics.rkt")
(require "abstracties.rkt")
(provide maak-adt-teken)



(define (maak-adt-teken titel pixels-verticaal pixels-horizontaal)

  ;Maak venster
  (define venster (make-window pixels-horizontaal pixels-verticaal titel))
  ;; Teken zwarte achtergrond
  ((venster 'set-background!) "black")

  ;; Maak Lagen aan
  (define achtergrond-laag (venster 'make-layer))
  (define struik-laag (venster 'make-layer))
  (define eetbare-objecten-laag (venster 'make-layer))
  (define kikker-laag (venster 'make-layer))
  (define auto-laag (venster 'make-layer))
  (define score-laag (venster 'make-layer))

  ; Tekent achtergrond en struiken
  (define (teken-scherm! struik-x-pos-lijst)
    (let ((y y-pos-berm-met-struik))
      (define (teken-struiken)
        (struik-laag 'clear)
        (for-each (lambda (x-pos)
                    (define struik-tile (make-bitmap-tile "sprites/Bosje.png" "sprites/Bosje_mask.png"))
                    (voeg-tile-toe-hogere-orde! x-pos y struik-laag struik-tile))
                  struik-x-pos-lijst))
      (define (teken-achtergrond)
        (achtergrond-laag 'clear)
        ((achtergrond-laag 'add-drawable) (make-bitmap-tile "sprites/Achtergrond.png" "sprites/Achtergrond_mask.png"))
        (teken-struiken))
      (teken-achtergrond)))

  ;; dit is de fontgrootte van de score
  (define size-ft 15)

  ;; Teken tekst op scherm (levens, score en level, seconden pil)
  (define (teken-score! score-adt)
    (let ((score-string (string-append "Score: " (number->string (score-adt 'score))))
          (highscore-string (string-append "HS: " (number->string (score-adt 'highscore)))))
      (score-tile 'clear)
      ((score-tile 'draw-text) score-string size-ft 0 0 "Red")
      ((score-tile 'draw-text) highscore-string size-ft 330 0 "Blue")))

  (define (teken-levens! kikker-adt)
    (let ((levens-string (string-append "Levens: " (number->string (kikker-adt 'levens)))))
      (leven-tile 'clear)
      ((leven-tile 'draw-text) levens-string size-ft 0 0 "Red")))

  (define (teken-level! level-adt)
    (let ((level-string (string-append "Lvl:  " (number->string (level-adt 'level)))))
      (level-tile 'clear)
      ((level-tile 'draw-text) level-string size-ft 0 0 "Red")))

  (define (update-seconden! kikker-adt)
    (let ((seconden-kikker (quotient (kikker-adt 'seconden) 1000)))
      (unless (= seconden-kikker scherm-seconden)
        (seconds-tile 'clear)
        ((seconds-tile 'draw-text) (string-append (number->string (+ seconden-kikker 1)) " s")
                                   size-ft 0 0 "Yellow"))))
  
  
  ;maak tekst-tiles aan en teken ze op het scherm
  (define (voeg-tile-toe-hogere-orde! x y laag tile)
    ((tile 'set-x!) x)
    ((tile 'set-y!) y)
    ((laag 'add-drawable) tile))
    
  (define (voeg-tile-toe-stats! tile  x)
    (voeg-tile-toe-hogere-orde! x 6.5 score-laag tile))

  (define level-tile (make-tile 100 30))
  (voeg-tile-toe-stats! level-tile 240)
  
  (define score-tile (make-tile 420 30))
  (voeg-tile-toe-stats! score-tile 0)

  (define seconds-tile (make-tile 420 30))
  (voeg-tile-toe-stats! seconds-tile 100)

  (define leven-tile (make-tile 100 30))
  (voeg-tile-toe-stats! leven-tile 150)

  (define welkom/game-over-tile (make-tile 420 150))
  (voeg-tile-toe-hogere-orde! 0 180 score-laag welkom/game-over-tile)

  

  ; teken welkomst-tekst en game-over/finish
  (define (teken-welkom! msg . score-adt)
    (welkom/game-over-tile 'clear)
    (case msg
      ('teken!
       ((welkom/game-over-tile 'draw-rectangle) 0 0 420 150 "black")
       ((welkom/game-over-tile 'draw-text) "Welkom in Frogger! " 30 80 0 "Green")
       ((welkom/game-over-tile 'draw-text) "Gemaakt door Simon Vervisch 2018-2019 " 8 140 40 "Green")
       ((welkom/game-over-tile 'draw-text) (string-append "Uw laatste highscore was: " (number->string ((car score-adt) 'highscore))) 20 70 50 "Yellow")
       ((welkom/game-over-tile 'draw-text) "h: highscore reset, p = pauze/play"  10 130 85 "Green")
       ((welkom/game-over-tile 'draw-text) "Druk s om te starten " 30 70 100 "Yellow"))
      ('clear! (welkom/game-over-tile 'clear))))

  (define (game-over msg)
    (case msg
      ('dood 
       ((welkom/game-over-tile 'draw-text) "Game Over" 40 110 0 "Red"))
      ('finished
       ((welkom/game-over-tile 'draw-text) "Je hebt het gehaald!" 40 20 0 "Red"))))
    
  (define scherm-seconden -1)
  
  (define (verwijder-seconden!)
    (set! scherm-seconden -1)
    (seconds-tile 'clear))

  ;; Teken Kikker
  (define (teken-kikker! kikker-adt)
    (hogere-orde-teken! kikker-adt kikker-tile))

  ;; Configuratie Kikker laag
  (define kikker-normaal-tile (make-bitmap-tile "sprites/Frogger.png" "sprites/Frogger_mask.png"))
  (define kikker-onschendbaar-tile (make-bitmap-tile "sprites/Frogger-Onschendbaar.png" "sprites/Frogger-Onschendbaar_mask.png"))
  (define kikker-tile '())
  (set! kikker-tile kikker-normaal-tile)
  ((kikker-laag 'add-drawable) kikker-tile)

  (define (verander-kikker-tile! kikker-adt) 
    (if (kikker-adt 'onschendbaar?)
        (set! kikker-tile kikker-onschendbaar-tile)
        (set! kikker-tile kikker-normaal-tile))
    (kikker-laag 'clear)
    ((kikker-laag 'add-drawable) kikker-tile))


  (define pil-lijst '())
  (define munt-lijst '())
  (define insect-lijst '())
  (define auto-lijst '())

  (define (update-auto! auto-adt)
    (hogere-orde-teken! auto-adt (cdr (assq auto-adt auto-lijst)))) 


  
  ;; Teken en verwijder functioneel passen assoc-lijst aan (adt . tile) en geven lijst terug
  (define (teken-functioneel adt lijst laag tile)
    (if (assq adt lijst)
        lijst
        (begin ((laag 'add-drawable) tile)
               (hogere-orde-teken! adt tile)
               (cons (cons adt tile) lijst))))


  (define (verwijder-functioneel adt lijst laag)
    (let ((object (assq adt lijst)))
      (if object
          (begin ((laag 'remove-drawable) (cdr object))
                 (remq object lijst))
          lijst)))

  (define (maak-bitmap-tile-auto adt)
    (define (path msg)
      (string-append "sprites/auto-" (symbol->string (adt 'richting)) (number->string (adt 'soort))
                        (case msg
                          ('mask "_mask.png")
                          ('png ".png"))))
    (make-bitmap-tile (path 'png) (path 'mask)))


  ;teken pil/insect/auto/munt
  (define (teken-adt! adt)
    (case (adt 'type)
      ('munt (set! munt-lijst (teken-functioneel adt munt-lijst eetbare-objecten-laag
                                                 (make-bitmap-tile "sprites/Bitcoin.png" "sprites/bitcoin_mask.png"))))
      ('pil (set! pil-lijst (teken-functioneel adt pil-lijst eetbare-objecten-laag
                                               (make-bitmap-tile "sprites/pil.png" "sprites/pil_mask.png"))))
      ('insect (case (adt 'soort)
                 ((0) (set! insect-lijst (teken-functioneel adt insect-lijst eetbare-objecten-laag
                                                            (make-bitmap-tile "sprites/insect0.png" "sprites/insect0_mask.png"))))
                 ((1) (set! insect-lijst (teken-functioneel adt insect-lijst eetbare-objecten-laag
                                                            (make-bitmap-tile "sprites/insect1.png" "sprites/insect1_mask.png"))))
                 ((2) (set! insect-lijst (teken-functioneel adt insect-lijst eetbare-objecten-laag
                                                            (make-bitmap-tile "sprites/insect2.png" "sprites/insect2_mask.png"))))
                 ((3) (set! insect-lijst (teken-functioneel adt insect-lijst eetbare-objecten-laag
                                                            (make-bitmap-tile "sprites/insect3.png" "sprites/insect3_mask.png"))))))
      ('auto (case (adt 'soort)
               ((0) (set! auto-lijst (teken-functioneel adt auto-lijst auto-laag
                                                        (maak-bitmap-tile-auto adt))))
               ((1) (set! auto-lijst (teken-functioneel adt auto-lijst auto-laag
                                                    (maak-bitmap-tile-auto adt))))
               ((2) (set! auto-lijst (teken-functioneel adt auto-lijst auto-laag
                                                    (maak-bitmap-tile-auto adt))))
               ((3) (set! auto-lijst (teken-functioneel adt auto-lijst auto-laag
                                                    (maak-bitmap-tile-auto adt))))))))

  ;verwijder pil/insect/auto/munt
  (define (verwijder-eetbaar-adt! adt)
    (case (adt 'type)
      ('munt (set! munt-lijst (verwijder-functioneel adt munt-lijst eetbare-objecten-laag)))
      ('pil (set! pil-lijst (verwijder-functioneel adt pil-lijst eetbare-objecten-laag)))
      ('insect (set! insect-lijst (verwijder-functioneel adt insect-lijst eetbare-objecten-laag)))))

  (define (initialiseer-level!)
    (auto-laag 'clear)
    (eetbare-objecten-laag 'clear)
    (set! auto-lijst '())
    (set! insect-lijst '()))


  
  ;; Spel lus functies
  (define (set-spel-lus-functie! fun)
    ((venster 'set-update-callback!) fun))
  
  (define (set-toets-functie! fun)
    ((venster 'set-key-callback!) fun))
  
  (define (dispatch-teken-adt msg)
    (case msg
      ('set-toets-functie! set-toets-functie!)
      ('set-spel-lus-functie! set-spel-lus-functie!)
      ;; Teken functies.
      ('teken-kikker! teken-kikker!)
      ('teken-welkom! teken-welkom!)
      ('teken-scherm! teken-scherm!)
      ('teken-eetbaar-adt! teken-adt!)
      ('teken-level! teken-level!)
      ('game-over game-over)
      ('initialiseer-level! (initialiseer-level!))
      ('update-auto! update-auto!)
      ('update-seconden! update-seconden!)
      ('verwijder-seconden! (verwijder-seconden!))
      ('teken-levens! teken-levens!)
      ('teken-score! teken-score!)
      ('verwijder-eetbaar-adt! verwijder-eetbaar-adt!)
      ('verander-kleur-kikker! verander-kikker-tile!)))
  dispatch-teken-adt)