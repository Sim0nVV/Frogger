#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Level ADT  ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require "abstracties.rkt")

(provide (all-defined-out))

(define (maak-adt-level)
  (let ((level 1)
        (auto-refresh-rate-vector (vector 40 35 20 40 30 25)))

    (define (teken-level! teken-adt)
      ((teken-adt 'teken-level!) dispatch-level))

    ;verhoogt het level telkens met 1 (tot aan 6)
    (define (volgend-level! teken-adt)
      (if (= level 6)
          (set! level 1)
          (set! level (+ level 1)))
      (teken-level! teken-adt))

    (define (reset! teken-adt)
      (set! level 1)
      (teken-level! teken-adt))

    ;geeft struik-x-posities weer
    (define (struik-x-pos)
      (convert-naar-pixels
       (case level
        ((1 2 3)
         '(1 12))
        ((4 5 6)
         '(1 4 9 13)))))        


    (define (dispatch-level msg)
      (case msg
        ('auto-refresh-rate (vector-ref auto-refresh-rate-vector (- level 1)))
        ('volgend-level! volgend-level!)
        ('reset! reset!)
        ('level level)
        ('teken-level! teken-level!)
        ('struik-x-pos (struik-x-pos))))

    dispatch-level))