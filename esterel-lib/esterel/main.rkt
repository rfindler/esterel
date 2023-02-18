#lang racket/base
(require "kernel.rkt"
         (for-syntax racket/base))
(provide halt loop-each abort-when sustain
         await await-immediate await-n
         every every-immediate
         weak-abort weak-abort-immediate
         (all-from-out "kernel.rkt"))

(define (halt)
  (let loop ()
    (pause)
    (loop)))

(define-syntax-rule
  (loop-each p s)
  (loop-each/proc (λ () p) (λ () s)))
(define (loop-each/proc thunk abort-thunk)
  (let loop ()
    (abort-when (begin (thunk) (halt)) (abort-thunk))
    (loop)))

(define-syntax-rule
  (abort-when p s)
  (abort-when/proc (λ () p) (λ () s)))
(define (abort-when/proc body-thunk when-thunk)
  (with-trap T-abort-when.1
    (with-trap T-abort-when.2
      (par (begin (suspend (body-thunk) (when-thunk)) (exit-trap T-abort-when.1))
           (begin (await (when-thunk)) (exit-trap T-abort-when.2))))))

(define-syntax-rule
  (await e)
  (await/proc (λ () e)))
(define (await/proc thunk)
  (with-trap T-await
    (let loop ()
      (pause)
      (when (thunk)
        (exit-trap T-await))
      (loop))))

(define-syntax-rule
  (await-n when-expr n-expr)
  (await-n/proc (λ () when-expr) n-expr))
(define (await-n/proc thunk n)
  (suspend
   (repeat n (λ () (pause)))
   (not (thunk))))

(define (repeat n thunk)
  (with-trap T
    (thunk)
    (let loop ([n (- n 1)])
      (if (> n 0)
          (begin (thunk) (loop (- n 1)))
          (exit-trap T)))))

(define (await-immediate s)
  (with-trap T-await-immediate
    (let loop ()
      (when (present? s)
        (exit-trap T-await-immediate))
      (pause)
      (loop))))

(define-syntax (every stx)
  (syntax-case stx ()
    [(_ s p) #'(every/proc (λ () s) (λ () p))]
    [(_ s n p) #'(every-n/proc (λ () s) n (λ () p))]))

(define (every/proc test-thunk body-thunk)
  (await (test-thunk))
  (loop-each
   (body-thunk)
   (test-thunk)))

(define (every-n/proc test-thunk n body-thunk)
  (define every-n (signal))
  (par (let loop ()
         (await-n (test-thunk) n)
         (emit every-n)
         (loop))
       (every (present? every-n) (body-thunk))))

(define-syntax-rule
  (every-immediate s p)
  (every-immediate/proc s (λ () p)))
(define (every-immediate/proc s thunk)
  (await-immediate s)
  (loop-each
   (thunk)
   (present? s)))

(define (sustain s)
  (let loop ()
    (emit s)
    (pause)
    (loop)))

(define-syntax-rule
  (weak-abort signal expr1 expr2 ...)
  (weak-abort/proc signal (λ () expr1 expr2 ...)))

(define (weak-abort/proc signal body)
  (with-trap T-weak-abort
    (par (begin (body) (exit-trap T-weak-abort))
         (begin (await-immediate signal) (exit-trap T-weak-abort)))))

(define-syntax-rule
  (weak-abort-immediate signal expr1 expr2 ...)
  (weak-abort-immediate/proc signal (λ () expr1 expr2 ...)))

(define (weak-abort-immediate/proc signal body)
  (with-trap T-weak-abort-immediate
    (par (begin (body) (exit-trap T-weak-abort-immediate))
         (begin (await-immediate signal) (exit-trap T-weak-abort-immediate)))))
