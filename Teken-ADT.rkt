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

  ;; Teken Achtergrond en struiken
  (define achtergrond-laag (venster 'make-layer))
  (define struik-laag (venster 'make-layer))
  
  (define (teken-scherm!)
    (define (teken-struiken) 
      (define y y-pos-berm-met-struik)
      (for-each (lambda (x-pos)
                  (define struik-tile (make-bitmap-tile "Bosje.png" "Bosje_mask.png"))
                  ((struik-laag 'add-drawable) struik-tile)
                  ((struik-tile 'set-x!) x-pos)
                  ((struik-tile 'set-y!) y))
                (struik-pos level)))
    (define (teken-achtergrond)
      (define achtergrond-tile (make-bitmap-tile "Achtergrond.png" "Achtergrond_mask.png"))
      ((achtergrond-laag 'add-drawable) achtergrond-tile)
      (teken-struiken))
    (teken-achtergrond))


  ; Configuratie Eetbare-objecten-laag
  (define eetbare-objecten-laag (venster 'make-layer))
  (define munt-tile (make-bitmap-tile "Bitcoin.png" "Bitcoin_mask.png"))

  (define pil-tile (make-bitmap-tile "pil.png" "pil_mask.png"))
  
  ;Tekent en verwijder munt

  
  (define (teken-munt! munt-adt)
    ((eetbare-objecten-laag 'add-drawable) munt-tile)
    (hogere-orde-teken! munt-adt munt-tile))

  (define (verwijder-munt!)
    ((eetbare-objecten-laag 'remove-drawable) munt-tile))


  
  ;; Configuratie Kikker tile
  (define kikker-laag (venster 'make-layer))
  (define kikker-tile (make-bitmap-tile "Frogger.png" "Frogger_mask.png"))
  (define kikker-onschendbaar-tile (make-bitmap-tile "Frogger-Onschendbaar.png" "Frogger-Onschendbaar_mask.png"))
  ((kikker-laag 'add-drawable) kikker-tile)

  ;; Configuratie Auto tile
  (define auto-en-score-laag (venster 'make-layer))

  (define auto-tile (make-bitmap-tile "auto1.png" "Auto1_mask.png"))
  (define auto-tile2 (make-bitmap-tile "auto1.png" "Auto1_mask.png"))
  (define auto-tile3 (make-bitmap-tile "auto2.png" "Auto2_mask.png"))
  (define auto-tile4 (make-bitmap-tile "auto2.png" "Auto2_mask.png"))
  
  ((auto-en-score-laag 'add-drawable) auto-tile)
  ((auto-en-score-laag 'add-drawable) auto-tile2)
  ((auto-en-score-laag 'add-drawable) auto-tile3)
  ((auto-en-score-laag 'add-drawable) auto-tile4)

  (define auto-tile-vector (vector auto-tile auto-tile2 auto-tile3 auto-tile4))


  ;Score ADT getekend
  
  (define score-tile (make-tile 400 30)) 
  
  (define (teken-score! score-adt score)
    (let ((score-string (string-append "Score: " (number->string score))))
      (score-tile 'clear)
      ((score-tile 'draw-text) score-string 20 0 0 "Red")
      ((auto-en-score-laag 'add-drawable) score-tile)))

  
  ;; Teken Kikker
  (define (teken-kikker! kikker-adt)
    (if (kikker-adt 'onschendbaar?)
        (hogere-orde-teken! kikker-adt kikker-onschendbaar-tile)
        (hogere-orde-teken! kikker-adt kikker-tile)))

  (define (verander-kikker-tile! kikker-adt)
    (if (kikker-adt 'onschendbaar?)
        (begin
          ((kikker-laag 'remove-drawable) kikker-tile)
          ((kikker-laag 'add-drawable) kikker-onschendbaar-tile))
        (begin
          ((kikker-laag 'remove-drawable) kikker-onschendbaar-tile)
          ((kikker-laag 'add-drawable) kikker-tile))))

  (define (teken-auto! auto-adt nr)
    (hogere-orde-teken! auto-adt (vector-ref auto-tile-vector nr)))

  ;  Pil
  (define (teken-pil! pil-adt)
    ((eetbare-objecten-laag 'add-drawable) pil-tile)
    (hogere-orde-teken! pil-adt pil-tile))

  (define (verwijder-pil!)
    ((eetbare-objecten-laag 'remove-drawable) pil-tile))


  ;; Spel lus functies
  (define (set-spel-lus-functie! fun)
    ((venster 'set-update-callback!) fun))
  
  (define (set-toets-functie! fun)
    ((venster 'set-key-callback!) fun))

  
  
  (define (dispatch-teken-adt msg)
    (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
          ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
          ;; Teken functies.
          ((eq? msg 'teken-kikker!) teken-kikker!)
          ((eq? msg 'teken-scherm!) teken-scherm!)
          ((eq? msg 'teken-munt!) teken-munt!)
          ((eq? msg 'teken-auto!) teken-auto!)
          ((eq? msg 'teken-score!) teken-score!)
          ((eq? msg 'teken-pil!) teken-pil!)
          ;((eq? msg 'teken-pil!) teken-pil!)
          ((eq? msg 'verwijder-munt!) verwijder-munt!)
          ((eq? msg 'verwijder-pil!) verwijder-pil!)
          ((eq? msg 'canvas-h) pixels-verticaal)
          ((eq? msg 'canvas-w) pixels-horizontaal)
          ((eq? msg 'verander-kleur-kikker!) verander-kikker-tile!)))
  dispatch-teken-adt)