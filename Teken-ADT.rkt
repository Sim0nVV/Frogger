;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Teken ADT ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

    

  ; Configuratie Munt-layer
  (define munt-laag (venster 'make-layer))
  (define munt-tile (make-bitmap-tile "Bitcoin.png" "Bitcoin_mask.png"))
  ((munt-laag 'add-drawable) munt-tile)
  

  ;Tekent en verwijder munt

  (define (teken-munt! munt-adt)
    (let* ((munt-x (munt-adt 'x))
           (munt-y (munt-adt 'y)))
      ((munt-tile 'set-x!) munt-x)
      ((munt-tile 'set-y!) munt-y)))

  (define (verwijder-munt!)
    ((munt-laag 'remove-drawable) munt-tile))
  
  ;; Configuratie Kikker tile
  (define kikker-laag (venster 'make-layer))
  (define kikker-tile (make-bitmap-tile "Frogger.png" "Frogger_mask.png"))
  ((kikker-laag 'add-drawable) kikker-tile)

  ;; Configuratie Auto tile
  (define auto-laag (venster 'make-layer))

  (define auto-tile (make-bitmap-tile "auto1.png" "Auto1_mask.png"))
  (define auto-tile2 (make-bitmap-tile "auto1.png" "Auto1_mask.png"))
  (define auto-tile3 (make-bitmap-tile "auto2.png" "Auto2_mask.png"))
  (define auto-tile4 (make-bitmap-tile "auto2.png" "Auto2_mask.png"))
  
  ((auto-laag 'add-drawable) auto-tile)
  ((auto-laag 'add-drawable) auto-tile2)
  ((auto-laag 'add-drawable) auto-tile3)
  ((auto-laag 'add-drawable) auto-tile4)

  
  (define auto-tile-vector (vector auto-tile auto-tile2 auto-tile3 auto-tile4))


  ;Score ADT getekend
  (define score-tile (make-tile 300 100))
  
  ((score-tile 'draw-text) "dit is een zeer mooi tekstje" 15 0 0 "Olive")
  ((auto-laag 'add-drawable) score-tile)
   ;TODO Maak nog een extra laag aan
  
  


  ;; Teken Kikker
  (define (teken-kikker! kikker-adt) ;TODO, laat frogger draaien
    (let* ((kikker-x (kikker-adt 'x))
           (kikker-y (kikker-adt 'y)))
      ((kikker-tile 'set-x!) kikker-x)
      ((kikker-tile 'set-y!) kikker-y)))

  (define (teken-auto! auto-adt nr)
    (let* ((auto-x (auto-adt 'x))
           (auto-y (auto-adt 'y)))
      (((vector-ref auto-tile-vector nr) 'set-x!) auto-x)
      (((vector-ref auto-tile-vector nr) 'set-y!) auto-y)))

  
  #;(define (teken-functie! adt)
    (let ((x-pos (adt 'x))
          (y-pos (adt 'y))))) ;TODO: hoe vind je de tile bijhorend bij het adt??
      

  

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
          ((eq? msg 'verwijder-munt!) verwijder-munt!) 
          ((eq? msg 'canvas-h) pixels-verticaal)
          ((eq? msg 'teken-score!) teken-score!)
          ((eq? msg 'canvas-w) pixels-horizontaal)))
  dispatch-teken-adt)