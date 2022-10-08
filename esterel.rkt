#lang racket
(require "kernel-esterel.rkt")
(provide halt loop-each abort-when await
         (all-from-out "kernel-esterel.rkt"))

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