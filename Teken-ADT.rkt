(define (maak-teken-adt titel pixels-verticaal pixels-horizontaal)

  
   (define venster (make-window pixels-horizontaal pixels-verticaal titel))
  
  ;; We willen een zwarte achtergrond.
  ((venster 'set-background!) "black")


  ;; Spel lus functies
  (define (set-spel-lus-functie! fun)
    ((venster 'set-update-callback!) fun))
  
  (define (set-toets-functie! fun)
    ((venster 'set-key-callback!) fun))

;  (define appel-laag (venster 'make-layer))
;  (define appel-tile (make-bitmap-tile "apple.jpg" "apple-mask.jpg"))
;  ((appel-laag 'add-drawable) appel-tile)
;
;
;  (define slang-laag (venster 'make-layer))
;  (define slang-tiles '())
  (define (dispatch-teken-adt msg)
    (cond ((eq? msg 'set-toets-functie!) set-toets-functie!)
          ((eq? msg 'set-spel-lus-functie!) set-spel-lus-functie!)
          ;; Teken functies.
          ((eq? msg 'canvas-h) pixels-verticaal)
          ((eq? msg 'canvas-w) pixels-horizontaal)))
  dispatch-teken-adt)