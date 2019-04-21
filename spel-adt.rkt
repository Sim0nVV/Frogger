#lang racket


(require "Teken-ADT.rkt")
(require "kikker-adt.rkt")
(require "positie-adt.rkt")
(require "munt-adt.rkt")
(require "abstracties.rkt")
(require "pil-adt.rkt")
(require "auto-adt.rkt")
(require "score-adt.rkt")
(require "insect-adt.rkt")
(require "helpers.rkt")

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Spel-adt ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (maak-adt-spel)

  ;aanmaak objecten

  (define kikker-adt (maak-adt-kikker kikker-x-startpos kikker-y-startpos))
  (define teken-adt (maak-adt-teken "Frogger" px-venster-hoogte px-venster-breedte))
  (define score-adt (maak-adt-score))  
  (define pil-adt (maak-adt-pil (random-x) (random-y)))

  (define coordinaat-lijst '(5 6 10 9))
  (define type-lijst '(0 1 2 3))
  (define auto-lijst (map (lambda (x y) (maak-adt-auto 390 (* x px-element-hoogte)
                                                       y))
                          coordinaat-lijst
                          type-lijst))

  

  (define insect-lijst
    (build-list 3 (lambda (x) (maak-adt-insect (random-x) (random-y)))))

  (define munt-lijst
    (build-list 3 (lambda (x) (maak-adt-munt (random-x) (random-y)))))


  (define (init-teken!)
    ((teken-adt 'teken-scherm!))
    ((score-adt 'teken!) teken-adt)
    ((pil-adt 'teken!) teken-adt)
    (for-each (lambda (insect-adt) ((insect-adt 'teken!) teken-adt)) insect-lijst)
    (for-each (lambda (munt-adt) ((munt-adt 'teken!) teken-adt)) munt-lijst))

  
  (define (reset!)

    ((kikker-adt 'reset!) teken-adt)
    (for-each (lambda (auto-adt) (auto-adt 'reset!)) auto-lijst)
    ((score-adt 'reset!) teken-adt)
    ((pil-adt 'reset!) teken-adt)
    (for-each (lambda (insect-adt) ((insect-adt 'reset!) teken-adt)) insect-lijst)
    (for-each (lambda (munt-adt) ((munt-adt 'reset!) teken-adt)) munt-lijst))

  ;keyboard-inputprocedure
  (define (toets-functie-tijdens-spel type toets)
    (when (eq? type 'down)
      (case toets
        ('right
         ((kikker-adt 'beweging!) 'rechts))
        ('left
         ((kikker-adt 'beweging!) 'links))
        ('up
         ((kikker-adt 'beweging!) 'omhoog))
        ('down
         ((kikker-adt 'beweging!) 'omlaag))
        ('escape
         (exit))
        ('#\r
         (reset!)))))

  (define (collision-auto? kikker-adt auto-lijst teken-adt) 
    (let ((kikker-volgende-y (cdr ((kikker-adt 'volgende))))
          (kikker-volgende-x (car ((kikker-adt 'volgende))))
          (kikker-x (kikker-adt 'x))
          (kikker-y (kikker-adt 'y)))
      
        
      (ormap (lambda (auto-x auto-y) (and (controleer-x-volgende auto-x kikker-x)
                                          (eq? kikker-y auto-y)))
             (map (lambda (auto-adt) (auto-adt 'x)) auto-lijst )
             (map (lambda (auto-adt) (auto-adt 'y)) auto-lijst ))))
        
  


  (define update-autos 0)
  
  (define (start)
    (define kikker-tijd  0)
    (define auto-tijd    0)
    ;; Deze functie voert men elke `tick` uit.
    (define (spel-lus-functie delta-tijd)
        
      (set! kikker-tijd  (+ kikker-tijd delta-tijd))
      (set! auto-tijd    (+ auto-tijd delta-tijd))
        

      ;; Refresh Auto ;;
      (when (> auto-tijd auto-refresh-rate) 
        (for-each (lambda (auto-adt) ((auto-adt 'update!) teken-adt kikker-adt)) auto-lijst)
          
        (let* ((auto-adt auto-lijst)
               (volgende-auto (list-ref auto-adt volgende-auto-type))
               (rare-auto (list-ref auto-adt rare-auto-type)))
        
          (when (and (controleer-x-volgende (rare-auto 'x) (volgende-auto 'x)) 
                     (= (volgende-auto 'y) (rare-auto 'y)))
            ((volgende-auto 'botsing) (rare-auto 'y) teken-adt)))

        (set! auto-tijd 0))

          


      (when (kikker-adt 'onschendbaar?) ; check voor kikker onschendbaarheid
        ((kikker-adt 'update-onschendbaarheid!) delta-tijd teken-adt))
      ;; Refresh kikker ;;
        
      (when (> kikker-tijd kikker-refresh-rate)

          
          
          
        ;; Verander positie van de kikker (Collision detection)
        (let* ((kikker-x (kikker-adt 'x)) 
               (kikker-y (kikker-adt 'y))
               (pil-x (pil-adt 'x))
               (pil-y (pil-adt 'y))
               (kikker-volgende-x (car ((kikker-adt 'volgende))))
               (kikker-volgende-y (cdr ((kikker-adt 'volgende))))
               (munt-pos-assoc-lijst (map (lambda (munt-adt) (cons (and (controleer-x-volgende (munt-adt 'x) kikker-volgende-x)
                                                                        (eq? kikker-volgende-y (munt-adt 'y))) munt-adt))
                                          munt-lijst))
               (insect-pos-assoc-lijst (map (lambda (insect-adt) (cons (and (controleer-x-volgende (insect-adt 'x) kikker-volgende-x)
                                                                            (eq? kikker-volgende-y (insect-adt 'y))) insect-adt))
                                            insect-lijst)))

          (cond
            ((and (collision-auto? kikker-adt auto-lijst teken-adt)
                  (not (kikker-adt 'onschendbaar?)))
             (not (kikker-adt 'onschendbaar?))
             (reset!))
             
            ((and (< kikker-volgende-y bovenaan-scherm)
                  (andmap (lambda (munt-adt) (munt-adt 'verzameld?)) munt-lijst)
                  (pil-adt 'verzameld?))
             (reset!))
             
            ((or (not (<= links-scherm kikker-volgende-x rechts-scherm))
                 (not (<= bovenaan-scherm kikker-volgende-y onderaan-scherm)) ;Niet uit het scherm
                 (and (controleer-lijst kikker-volgende-x (struik-pos level))
                      (= kikker-volgende-y y-pos-berm-met-struik)));botsing met struik
             ((kikker-adt 'beweging!) 'doe-niets))

             
            ((assq #t munt-pos-assoc-lijst)
             (let ((gegeten-munt-adt (cdr (assq #t munt-pos-assoc-lijst))))
               ((gegeten-munt-adt 'verwijder!) teken-adt score-adt)
               ((kikker-adt 'beweeg!)))); botsing met munt
            ((assq #t insect-pos-assoc-lijst)
             (let ((gegeten-insect-adt (cdr (assq #t insect-pos-assoc-lijst))))
               ((gegeten-insect-adt 'verwijder!) teken-adt score-adt)
               ((kikker-adt 'beweeg!))))
             
            ((and (= pil-y kikker-volgende-y) ; zet dat in een lijst 
                  (controleer-x-volgende pil-x kikker-volgende-x))

             ((kikker-adt 'onschendbaar!) teken-adt)
             ((pil-adt 'verwijder!) teken-adt score-adt)
             ((kikker-adt 'beweeg!)))
            
            (else
             ((kikker-adt 'beweeg!))))
            
          (set! kikker-tijd 0)))

      ;;Herteken kikker
        
      ((kikker-adt 'teken!) teken-adt))

    ;;; INIT ;;;

    (define (init)
      (init-teken!))
    (init)


    ;; Zet de callbacks via de library.
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
    ((teken-adt 'set-toets-functie!) toets-functie-tijdens-spel))

  (define (dispatch-spel msg)
    (cond ((eq? msg 'start) start)))

  dispatch-spel)


;;;;;;;;;;;;;;;;;
;; Entry Point ;;
;;;;;;;;;;;;;;;;;

(define spel (maak-adt-spel))

((spel 'start))
