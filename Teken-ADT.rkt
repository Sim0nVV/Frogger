#lang racket 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Teken ADT ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "graphics.rkt")
(require "abstracties.rkt")
(provide maak-adt-teken)

;oplossing voor teken-adt: maak assoc met object en tile


(define (maak-adt-teken titel pixels-verticaal pixels-horizontaal)

  ;Maak venster
  (define venster (make-window pixels-horizontaal pixels-verticaal titel))
  ;; Teken zwarte achtergrond
  ((venster 'set-background!) "black")

  ;; Teken Achtergrond en struiken (lagen zijn imperformant -> dingen die nooit zullen overlappen nooit samen)
  (define achtergrond-laag (venster 'make-layer))
  (define struik-laag (venster 'make-layer))
  (define eetbare-objecten-laag (venster 'make-layer))
  (define kikker-laag (venster 'make-layer))
  (define auto-en-score-laag (venster 'make-layer))
  
  (define (teken-scherm!)
    (let ((y y-pos-berm-met-struik))
      (define (teken-struiken) 
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
      (teken-achtergrond)))


  (define (teken-score! score-adt)
    (let ((score-string (string-append "Score: " (number->string (score-adt 'score))))
          (highscore-string (string-append "HS: " (number->string (score-adt 'highscore)))))
      (score-tile 'clear)
      ((score-tile 'draw-text) score-string 20 0 0 "Red")
      ((score-tile 'draw-text) highscore-string 20 300 0 "Red")
      ((auto-en-score-laag 'add-drawable) score-tile)))

  ;; Teken Kikker
  (define (teken-kikker! kikker-adt)
    (if (kikker-adt 'onschendbaar?)
        (hogere-orde-teken! kikker-adt kikker-onschendbaar-tile)
        (hogere-orde-teken! kikker-adt kikker-tile)))

  (define (verander-kikker-tile! kikker-adt) ;BUG; Wanneer de kikker onschendbaar is, en opnieuw de pil neemt, blijft de tile hangen.
    (if (kikker-adt 'onschendbaar?)
        (begin
          ((kikker-laag 'remove-drawable) kikker-tile)
          ((kikker-laag 'add-drawable) kikker-onschendbaar-tile))
        (begin
          ((kikker-laag 'remove-drawable) kikker-onschendbaar-tile)
          ((kikker-laag 'add-drawable) kikker-tile))))

  (define (teken-auto! auto-adt nr)
    (hogere-orde-teken! auto-adt (list-ref auto-lijst nr)))


  (define pil-lijst '())
  (define munt-lijst '())
  (define insect-lijst '())

  
  ;; teken en verwijder functies (hogere-orde)

  (define (teken-functioneel adt lijst tile)
    (if (assq adt lijst)
        lijst
        (begin ((eetbare-objecten-laag 'add-drawable) tile)
               (hogere-orde-teken! adt tile)
               (cons (cons adt tile) lijst))))


  (define (verwijder-functioneel adt lijst)
    (let ((object (assq adt lijst)))
      (if object
          (begin ((eetbare-objecten-laag 'remove-drawable) (cdr object))
                 (remq object lijst))
          lijst)))

  

  (define (teken-eetbaar-adt! adt)
    (case (adt 'type)
      ('munt (set! munt-lijst (teken-functioneel adt munt-lijst
                                        (make-bitmap-tile "Bitcoin.png" "bitcoin_mask.png"))))
      ('pil (set! pil-lijst (teken-functioneel adt pil-lijst
                                       (make-bitmap-tile "pil.png" "pil_mask.png"))))
      ('insect (set! insect-lijst (teken-functioneel adt insect-lijst
                                          (make-bitmap-tile "insect.png" "insect_mask.png"))))))

  
  (define (verwijder-eetbaar-adt! adt)
    (case (adt 'type)
      ('munt (set! munt-lijst (verwijder-functioneel adt munt-lijst)))
      ('pil (set! pil-lijst (verwijder-functioneel adt pil-lijst)))
      ('insect (set! insect-lijst (verwijder-functioneel adt insect-lijst)))))
      

  ;; Configuratie Kikker tile
  (define kikker-tile (make-bitmap-tile "Frogger.png" "Frogger_mask.png"))
  (define kikker-onschendbaar-tile (make-bitmap-tile "Frogger-Onschendbaar.png" "Frogger-Onschendbaar_mask.png"))
  ((kikker-laag 'add-drawable) kikker-tile)

  ;; Configuratie Auto tile  

  (define auto-lijst
    (list 
     (make-bitmap-tile "auto1.png" "Auto1_mask.png")
     (make-bitmap-tile "auto2.png" "Auto2_mask.png")
     (make-bitmap-tile "auto3.png" "Auto3_mask.png")
     (make-bitmap-tile "auto4.png" "Auto4_mask.png")))

  (for-each (lambda (auto-tile) ((auto-en-score-laag 'add-drawable) auto-tile))
            auto-lijst)


  ;Score ADT getekend
  
  (define score-tile (make-tile 420 30))
  
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
      ('teken-scherm! teken-scherm!)
      ('teken-eetbaar-adt! teken-eetbaar-adt!)
      ('teken-auto! teken-auto!)
      ('teken-score! teken-score!)
      ('verwijder-eetbaar-adt! verwijder-eetbaar-adt!)
      ('verander-kleur-kikker! verander-kikker-tile!)))
      dispatch-teken-adt)