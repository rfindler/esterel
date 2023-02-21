#lang racket
(require esterel/full)

#|

This is a translation of the code in 1.6 of
_Compiling Esterel_; the test cases are as
given in figure 1.2

|#

(define A (signal))
(define B (signal))
(define R (signal))
(define O (signal))

(define abro
  (reaction
   (par (loop-each
         (begin (par (await (present? A))
                     (await (present? B)))
                (emit O))
         (present? R))
        (begin
          #;0 (pause)
          #;1 (emit A) (emit B) (pause)
          #;2 (emit A) (pause)
          #;3 (emit R) (pause)
          #;4 (emit A) (pause)
          #;5 (emit B) (pause)
          #;6 (emit A) (emit B) (emit R) (pause)
          ))))

(define (react/o?)
  (hash-ref (react! abro) O #f))
(require rackunit)

(check-equal? (react/o?) #f) ;; 0
(check-equal? (react/o?) #t) ;; 1
(check-equal? (react/o?) #f) ;; 2
(check-equal? (react/o?) #f) ;; 3
(check-equal? (react/o?) #f) ;; 4
(check-equal? (react/o?) #t) ;; 5
(check-equal? (react/o?) #f) ;; 6
