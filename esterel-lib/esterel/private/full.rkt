#lang racket/base
(require "kernel.rkt"
         (for-syntax racket/base syntax/parse))
(provide halt loop abort sustain await every
         loop-each/proc loop/proc
         await/proc await-n/proc await-immediate/proc
         every-n/proc every/proc every-immediate/proc
         abort-when/proc weak-abort/proc weak-abort-immediate/proc)

(define (halt)
  (let loop ()
    (pause)
    (loop)))

(define-syntax (loop stx)
  (syntax-parse stx
    [(_ p:expr ...+ #:each s:expr)
     #'(loop-each/proc (λ () p ...) (λ () s))]
    [(_ p:expr ...+)
     #'(loop/proc (λ () p ...))]))
(define (loop-each/proc thunk abort-thunk)
  (let loop ()
    (abort (thunk) (halt) #:when (abort-thunk))
    (loop)))
(define (loop/proc thunk)
  (let loop ()
    (thunk)
    (loop)))

(define-syntax (await stx)
  (syntax-parse stx
    [(_ e:expr (~optional (~seq #:n n:expr)))
     (if (attribute n)
         #'(await-n/proc (λ () e) n)
         #'(await/proc (λ () e)))]
    [(_ #:immediate e:expr)
     #'(await-immediate/proc (λ () e))]))

(define (await/proc thunk)
  (with-trap T-await
    (let loop ()
      (pause)
      (when (thunk)
        (exit-trap T-await))
      (loop))))

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

(define (await-immediate/proc test-thunk)
  (with-trap T-await-immediate
    (let loop ()
      (when (test-thunk)
        (exit-trap T-await-immediate))
      (pause)
      (loop))))

(define-syntax (every stx)
  (syntax-parse stx
    [(_ s #:do p ...+)
     #'(every/proc (λ () s) (λ () p ...))]
    [(_ s #:n n #:do p ...+)
     #'(every-n/proc (λ () s) n (λ () p ...))]
    [(_ #:immediate s #:do p ...+)
     #'(every-immediate/proc (λ () s) (λ () p ...))]))

(define (every/proc test-thunk body-thunk)
  (await (test-thunk))
  (loop
   (body-thunk)
   #:each (test-thunk)))

(define (every-n/proc test-thunk n body-thunk)
  (with-signal (every-n)
    (par (let loop ()
           (await (test-thunk) #:n n)
           (emit every-n)
           (loop))
         (every (present? every-n) #:do (body-thunk)))))

(define (every-immediate/proc test-thunk body-thunk)
  (await #:immediate (test-thunk))
  (loop
   (body-thunk)
   #:each (test-thunk)))

(define (sustain a-signal [value no-value-provided])
  (emit-check-and-error 'sustain a-signal value)
  (loop
   (emit a-signal value)
   (pause)))

(define-syntax (abort stx)
  (syntax-parse stx
    [(_ body:expr ...+ #:when test:expr)
     #'(abort-when/proc (λ () body ...) (λ () test))]
    [(_ #:weak body:expr ...+ #:when test:expr)
     #'(weak-abort/proc (λ () test) (λ () body ...))]
    [(_ #:weak body:expr ...+ #:when-immediate test:expr)
     #'(weak-abort-immediate/proc (λ () test) (λ () body ...))]))

(define (abort-when/proc body-thunk when-thunk)
  (with-trap T-abort-when.1
    (with-trap T-abort-when.2
      (par (begin (suspend (body-thunk) (when-thunk)) (exit-trap T-abort-when.1))
           (begin (await (when-thunk))
                  (exit-trap T-abort-when.2))))))

(define (weak-abort/proc test body)
  (with-trap T-weak-abort
    (par (begin (body) (exit-trap T-weak-abort))
         (begin (await #:immediate (test)) (exit-trap T-weak-abort)))))

(define (weak-abort-immediate/proc test body)
  (with-trap T-weak-abort-immediate
    (par (begin (body) (exit-trap T-weak-abort-immediate))
         (begin (await #:immediate (test)) (exit-trap T-weak-abort-immediate)))))
