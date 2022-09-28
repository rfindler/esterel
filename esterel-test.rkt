#lang racket
(require "esterel.rkt" rackunit)

(check-equal?
 (react!
  (reaction
   (void)))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (signal-value s)))
   (hash s #f)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (emit s)
     (signal-value s)))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par (begin (emit s) #f)
          (signal-value s))))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par (signal-value s)
          (signal-value s))))
   (hash s #f)))
