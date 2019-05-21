#lang racket

(require "teken-adt.rkt")
(require "kikker-adt.rkt")
(require "positie-adt.rkt")
(require "munt-adt.rkt")
(require "abstracties.rkt")
(require "pil-adt.rkt")
(require "auto-adt.rkt")
(require "score-adt.rkt")
(require "insect-adt.rkt")
(require "level-adt.rkt")


(provide maak-adt-spel)

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Spel-adt ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (maak-adt-spel)

  ;aanmaak objecten

  (define kikker-adt (maak-adt-kikker kikker-x-startpos kikker-y-startpos))
  (define teken-adt (maak-adt-teken "Frogger" px-venster-hoogte px-venster-breedte))
  (define score-adt (maak-adt-score))  
  (define pil-adt (maak-adt-pil (maak-adt-positie (random-x) (random-y))))
  (define level-adt (maak-adt-level))


  ;aanmaak lijsten die auto's zullen genereren
  (define y-coordinaat-boven-lijst '())
  (define x-pixel-boven-lijst '())
  (define type-boven-lijst '())
  (define y-coordinaat-onder-lijst '())
  (define x-pixel-onder-lijst '())
  (define type-onder-lijst '())
  (define auto-boven-lijst '())
  (define auto-onder-lijst '())
  (define volgende-auto '())
  (define auto-lijst '())

  (define insect-lijst '())
  (define munt-lijst '())

  ; spelen? kan spel pauzeren, game-over-tijd houdt bij hoelang game-over getekend wordt
  (define spelen? #f)
  (define game-over-tijd 0)

  (define (maak-auto-lijst x y type)
    (map (lambda (x-pixel y-coord type) (maak-adt-auto x-pixel
                                                       (convert-naar-pixels y-coord)
                                                       type))
         x y type))

  (define (set-autos!)

    (case (level-adt 'level)
      ((4 5 6)
       (set! y-coordinaat-boven-lijst '(5 5 6 6 7))
       (set! x-pixel-boven-lijst '(0 90 0 90 0))
       (set! type-boven-lijst '(0 0 1 1 0))
       (set! y-coordinaat-onder-lijst '(10 9 11 11))
       (set! x-pixel-onder-lijst '(390 390 390 300))
       (set! type-onder-lijst '(2 3 1 1)))
      ((1 2 3)
       (set! y-coordinaat-boven-lijst '(5 6 7))
       (set! x-pixel-boven-lijst '(0 0 0))
       (set! type-boven-lijst '(0 1 3))
       (set! y-coordinaat-onder-lijst '(10))
       (set! x-pixel-onder-lijst '(390))
       (set! type-onder-lijst '(2))))            
          

    (set! auto-boven-lijst (maak-auto-lijst
                            x-pixel-boven-lijst
                            y-coordinaat-boven-lijst
                            type-boven-lijst))

    (set! auto-onder-lijst (maak-auto-lijst
                            x-pixel-onder-lijst
                            y-coordinaat-onder-lijst
                            type-onder-lijst))

    (set! volgende-auto (car auto-onder-lijst))

    (set! auto-lijst (append auto-onder-lijst auto-boven-lijst)))

  ;past pil/munt/lijst aan
  (define (set-pil/munt/insect!)
    (case (level-adt 'level)
      ((4 5 6)
       (set! insect-lijst (build-list 3 (lambda (x) (maak-adt-insect (maak-adt-positie (random-x) (random-y))))))
       (set! munt-lijst (build-list 3 (lambda (x) (maak-adt-munt (maak-adt-positie (random-x) (random-y)))))))
      ((1 2 3)
       (set! insect-lijst (build-list 1 (lambda (x) (maak-adt-insect (maak-adt-positie (random-x) (random-y))))))
       (set! munt-lijst (build-list 1 (lambda (x) (maak-adt-munt (maak-adt-positie (random-x) (random-y)))))))))


  ;begin objecten die getekend moeten worden
  (define (init-teken!)
    ((kikker-adt 'init) teken-adt)
    ((teken-adt 'teken-scherm!) (level-adt 'struik-x-pos))
    (for-each (lambda (auto-adt) ((auto-adt 'teken!) teken-adt)) auto-lijst)
    ((score-adt 'teken!) teken-adt)
    ((pil-adt 'teken!) teken-adt)
    (for-each (lambda (insect-adt) ((insect-adt 'teken!) teken-adt)) insect-lijst)
    (for-each (lambda (munt-adt) ((munt-adt 'teken!) teken-adt)) munt-lijst)
    ((level-adt 'teken-level!) teken-adt))

  (define (initialiseer-level!)
    (teken-adt 'initialiseer-level!)
    (set-autos!)
    (set-pil/munt/insect!)
    (init-teken!))


  ;spellogica be-eindigen spel
  (define (eind-spel msg)
    ((score-adt 'einde-spel!) teken-adt)
    ((teken-adt 'game-over) msg)
    (set! spelen? #f)
    (set! game-over-tijd 1))

  (define (volgend-level!)
    (when (= (level-adt 'level) 6)
      (eind-spel 'finished))
    ((score-adt 'volgend-level!) teken-adt)
    ((level-adt 'volgend-level!) teken-adt)
    (initialiseer-level!)
    (reset!))

  ;spellogica dood kikker
  (define (dood!)
    (cond ((= (kikker-adt 'levens) 0)
           (eind-spel 'dood)
           ((score-adt 'reset-level!) teken-adt)
           ((level-adt 'reset!) teken-adt)
           (initialiseer-level!)
           (reset!))
          (else ((kikker-adt 'dood!) teken-adt)
                ((score-adt 'reset-level!) teken-adt)
                (reset!))))

  ;tekenen welkomstscherm
  (define (welkomst)
    (set! spelen? #f)
    ((kikker-adt 'levens-reset!) teken-adt)
    ((teken-adt 'teken-welkom!) 'teken! score-adt))    

  
  (define (reset!)
    ((teken-adt 'teken-scherm!) (level-adt 'struik-x-pos))
    ((kikker-adt 'reset!) teken-adt)
    (for-each (lambda (auto-adt) (auto-adt 'reset!)) auto-lijst)
    ((pil-adt 'reset!) teken-adt)
    (for-each (lambda (insect-adt) ((insect-adt 'reset!) teken-adt)) insect-lijst)
    (for-each (lambda (munt-adt) ((munt-adt 'reset!) teken-adt)) munt-lijst))

  ;keyboard-inputprocedure
  (define (toets-functie-tijdens-spel type toets)
    (when (eq? type 'down)
      (case toets
        ('right
         (if (= (level-adt 'level) 6)
           ((kikker-adt 'beweging!) 'links)
           ((kikker-adt 'beweging!) 'rechts)))
        ('left
         (if (= (level-adt 'level) 6)
           ((kikker-adt 'beweging!) 'rechts)
           ((kikker-adt 'beweging!) 'links)))
        ('up
         (if (= (level-adt 'level) 6)
             ((kikker-adt 'beweging!) 'omlaag)
             ((kikker-adt 'beweging!) 'omhoog)))
        ('down
         (if (= (level-adt 'level) 6)
             ((kikker-adt 'beweging!) 'omhoog)
             ((kikker-adt 'beweging!) 'omlaag)))
        ('escape
         (exit))
        ('#\r
         (reset!))
        ('#\h
         ((score-adt 'reset-highscore!) teken-adt))
        ('#\p
         (set! spelen? (not spelen?)))
        ('#\s
         ((teken-adt 'teken-welkom!) 'clear!)
         (set! spelen? #t)))))

  ;collision check auto met kikker
  (define (collision-auto? kikker-adt auto-lijst teken-adt) 
    (let ((kikker-volgende-y (cdr ((kikker-adt 'volgende))))
          (kikker-volgende-x (car ((kikker-adt 'volgende))))
          (kikker-x (kikker-adt 'x))
          (kikker-y (kikker-adt 'y)))
        
      (ormap (lambda (auto-x auto-y) (and (controleer-x-volgende auto-x kikker-x)
                                          (eq? kikker-y auto-y)))
             (map (lambda (auto-adt) (auto-adt 'x)) auto-lijst)
             (map (lambda (auto-adt) (auto-adt 'y)) auto-lijst))))        

  
  (define (start)
    (define kikker-tijd  0)
    (define auto-tijd    0)
    ;; Deze functie voert men elke `tick` uit.
    (define (spel-lus-functie delta-tijd)

      (set! kikker-tijd  (+ kikker-tijd delta-tijd))
      (set! auto-tijd    (+ auto-tijd delta-tijd))      

      ;; Refresh Auto ;;
      (when (and (> auto-tijd (level-adt 'auto-refresh-rate))
                 spelen?)
        (for-each (lambda (auto-adt) ((auto-adt 'update!) teken-adt kikker-adt)) auto-lijst)
       

        ;collision detection volgende auto met andere auto's
        (let ((auto-lijst-x (map (lambda (auto) (auto 'x)) (cdr auto-onder-lijst)))
              (auto-lijst-y (map (lambda (auto) (auto 'y)) (cdr auto-onder-lijst))))
          (when (ormap (lambda (auto-x auto-y) (and (controleer-x-volgende (volgende-auto 'x) auto-x)
                                                    (= (volgende-auto 'y) auto-y)))
                       auto-lijst-x
                       auto-lijst-y)
              
            ((volgende-auto 'botsing) teken-adt)))

        (set! auto-tijd 0))


      (when (and (kikker-adt 'onschendbaar?)
                 spelen?); check voor kikker-onschendbaarheid
        ((kikker-adt 'check-onschendbaarheid!) delta-tijd teken-adt))

      ; update game-over-tijd (voor game-over scherm)
      (unless (= game-over-tijd 0)
        (set! game-over-tijd (+ delta-tijd game-over-tijd))
        (when (> game-over-tijd 5000)
          (set! game-over-tijd 0)
          (welkomst)))
      
      ;; Refresh kikker ;;
        
      (when (and (> kikker-tijd kikker-refresh-rate)
                 spelen?)
          
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
             (dood!)) ;botsing met auto
             
            ((and (= kikker-volgende-y bovenaan-scherm)
                  (andmap (lambda (munt-adt) (munt-adt 'verzameld?)) munt-lijst)
                  (pil-adt 'verzameld?)) ;volgend-leve
             
             (volgend-level!))
            ((or (not (<= links-scherm kikker-volgende-x rechts-scherm))
                 (not (<= bovenaan-scherm kikker-volgende-y onderaan-scherm)) ;uit scherm
                 (and (= kikker-volgende-y y-pos-berm-met-struik)
                      (controleer-lijst kikker-volgende-x (level-adt 'struik-x-pos))));botsing met struik
             ((kikker-adt 'beweging!) 'doe-niets))
             
            ((assq #t munt-pos-assoc-lijst)
             (let ((gegeten-munt-adt (cdr (assq #t munt-pos-assoc-lijst))))
               ((gegeten-munt-adt 'verwijder!) teken-adt score-adt)
               (kikker-adt 'beweeg!))); botsing met munt
            
            ((assq #t insect-pos-assoc-lijst) ;botsing met insect
             (let ((gegeten-insect-adt (cdr (assq #t insect-pos-assoc-lijst))))
               ((gegeten-insect-adt 'verwijder!) teken-adt score-adt)
               (kikker-adt 'beweeg!)))
             
            ((and (= pil-y kikker-volgende-y) ;botsing met pil
                  (controleer-x-volgende pil-x kikker-volgende-x))
             ((kikker-adt 'onschendbaar!) teken-adt)
             ((pil-adt 'verwijder!) teken-adt score-adt)
             (kikker-adt 'beweeg!))

            ((and (member kikker-volgende-y (convert-naar-pixels pos-rivier)) ;in het water
                  (not (collision-detection-lijst-boom kikker-volgende-x
                                                       (vector-ref boomstronk-vector
                                                                   (convert-naar-coord
                                                                    kikker-volgende-y)))))
             (dood!))
            
            (else
             (kikker-adt 'beweeg!)))
            
          (set! kikker-tijd 0)))

      ;;Herteken kikker
        
      ((kikker-adt 'teken!) teken-adt))

    ;;; INIT ;;;

    (let init ()
      (initialiseer-level!)
      (welkomst)
      (init-teken!))


    ;; Zet de callbacks via de library.
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie)
    ((teken-adt 'set-toets-functie!) toets-functie-tijdens-spel))

  (define (dispatch-spel msg)
    (case msg
      ('start (start))))

  dispatch-spel)


;;;;;;;;;;;;;;;;;
;; Entry Point ;;
;;;;;;;;;;;;;;;;;

(define spel (maak-adt-spel))

(spel 'start)
