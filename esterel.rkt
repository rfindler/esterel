#lang racket
(require "kernel-esterel.rkt")
(provide halt loop-each abort-when sustain
         await await-immediate
         every every-immediate
         (all-from-out "kernel-esterel.rkt"))

(define (halt)
  (let loop ()
    (pause)
    (loop)))

(define-syntax-rule
  (loop-each p t)
  (loop-each/proc (λ () p) t))
(define (loop-each/proc thunk t)
  (let loop ()
    (abort-when (begin (thunk) (halt)) t)
    (loop)))

(define-syntax-rule
  (abort-when p d)
  (abort-when/proc (λ () p) d))
(define (abort-when/proc thunk d)
  (with-trap T
    (par (begin (suspend (thunk) d) (exit-trap T))
         (begin (await d) (exit-trap T)))))

(define (await s)
  (with-trap T
    (let loop ()
      (pause)
      (when (signal-value s)
        (exit-trap T))
      (loop))))

(define (await-n s n)
  (error 'await-n
         "need to think harder about this https://github.com/florence/esterel-calculus/blob/master/front-end.rkt#L797"))

(define (await-immediate s)
  (with-trap T
    (let loop ()
      (when (signal-value s)
        (exit-trap T))
      (pause)
      (loop))))

(define-syntax (every stx)
  (syntax-case stx ()
    [(_ s p) #'(every/proc s (λ () p))]
    [(_ s n p) #'(every-n/proc s n (λ () p))]))

(define (every/proc s thunk)
  (await s)
  (loop-each
   (thunk)
   s))

(define (every-n/proc s n thunk)
  (let ([every-n (signal)])
    (par (let loop ()
           (await n every-n)
           (emit every-n)
           (loop))
         (every s (thunk)))))

(define-syntax-rule
  (every-immediate s p)
  (every-immediate/proc s (λ () p)))
(define (every-immediate/proc s thunk)
  (await-immediate s)
  (loop-each
   (thunk)
   s))

(define (sustain s)
  (let loop ()
    (emit s)
    (pause)
    (loop)))
