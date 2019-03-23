(define debug? #t)

(define (debug x . xs)
  (if debug?
      (begin
        (display x)
        (for-each display xs)
        (newline))))

(define (any-true? lst)
  (cond ((null? lst) #f)
        ((car lst) (car lst))
        (else (any-true? (cdr lst)))))
