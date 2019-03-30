;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Kikker ADT ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maak-adt-kikker x-pos y-pos)
  (let ((kikker-positie (maak-adt-positie x-pos
                                          y-pos))
        (beweging 'doe-niets)) ;beweging is de richting waarin de kikker zal updaten na beweeg!

    (define px-afgelegde-afstand 10)

    ;berekent volgende positie aan de hand van de tag beweging
    (define (volgende-positie x y)
      (cond ((eq? 'omhoog beweging)
             (cons x (- y px-element-hoogte)))
            ((eq? 'omlaag beweging)
             (cons x (+ y px-element-hoogte)))
            ((eq? 'links beweging)
             (cons (- x px-afgelegde-afstand) y))
            ((eq? 'rechts beweging)
             (cons (+ x px-afgelegde-afstand) y))
            ((eq? 'doe-niets beweging)
             (cons x y))))

    ;tekent kikker op nieuwe positie
    (define (teken! teken-adt)
      ((teken-adt 'teken-kikker!) dispatch-kikker))

    ;verandert tag beweging
    (define (set-beweging! b)
      (set! beweging b))

    ;Verandert coordinaten kikker
    (define (beweeg!)
      (let* ((oude-x (kikker-positie 'x))
             (oude-y (kikker-positie 'y))
             (volgende (volgende-positie oude-x oude-y)))
        ((kikker-positie 'x!) (car volgende)) 
        ((kikker-positie 'y!) (cdr volgende))
        (set-beweging! 'doe-niets)))


    (define (dispatch-kikker msg)
      (cond ((eq? msg 'x) (kikker-positie 'x))
            ((eq? msg 'y) (kikker-positie 'y))
            ((eq? msg 'teken!) teken!)
            ((eq? msg 'beweging!) set-beweging!)
            ((eq? msg 'beweeg!) beweeg!)
            ((eq? msg 'beweging) beweging)
            ((eq? msg 'volgende) volgende-positie)))
        
    dispatch-kikker))