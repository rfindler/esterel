#lang racket
(require "esterel.rkt")


(define A (signal))
(define B (signal))
(define R (signal))
(define O (signal))

(define (halt)
  (let loop ()
    (pause)
    (loop)))

(define-syntax-rule
  (loop-each p t)
  (let loop ()
    (abort-when (begin p (halt)) t)
    (loop)))

(define-syntax-rule
  (abort-when p d)
  (with-trap T
    (par (begin (suspend p d) (exit-trap T))
         (begin (await d) (exit-trap T)))))

(define (await s)
  (with-trap T
    (let loop ()
      (pause)
      (when (signal-value s)
        (exit-trap T))
      (loop))))

(define abro
  (reaction
   (par (loop-each
         (begin (par (await A)
                     (await B))
                (emit O))
         R)
        (begin
          (pause)
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
