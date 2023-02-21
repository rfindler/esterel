#lang racket/base
(require "kernel.rkt"
         (for-syntax racket/base syntax/parse))
(provide halt loop-each sustain await
         every every-immediate abort
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
         (await (test-thunk) #:n n)
         (emit every-n)
         (loop))
       (every (present? every-n) (body-thunk))))

(define-syntax-rule
  (every-immediate t p)
  (every-immediate/proc (λ () t) (λ () p)))
(define (every-immediate/proc test-thunk body-thunk)
  (await #:immediate (test-thunk))
  (loop-each
   (body-thunk)
   (test-thunk)))

(define (sustain s)
  (let loop ()
    (emit s)
    (pause)
    (loop)))

(define-syntax (abort stx)
  (syntax-parse stx
    [(_ body:expr ...+ #:when test:expr)
     #'(abort-when/proc (λ () body ...) (λ () test))]
    [(_ #:weak body:expr ...+ #:when test:expr)
     #'(weak-abort/proc (λ () test) (λ () body ...))]
    [(_ #:weak body:expr ...+ #:when-immediate test:expr)
     #'(weak-abort-immediate/proc (λ () test) (λ () body ...))]))


(define-syntax-rule
  (abort-when p s)
  (abort-when/proc (λ () p) (λ () s)))
(define (abort-when/proc body-thunk when-thunk)
  (with-trap T-abort-when.1
    (with-trap T-abort-when.2
      (par (begin (suspend (body-thunk) (when-thunk)) (exit-trap T-abort-when.1))
           (begin (await (when-thunk)) (exit-trap T-abort-when.2))))))

(define-syntax-rule
  (weak-abort test-expr expr1 expr2 ...)
  (weak-abort/proc (λ () test-expr) (λ () expr1 expr2 ...)))

(define (weak-abort/proc test body)
  (with-trap T-weak-abort
    (par (begin (body) (exit-trap T-weak-abort))
         (begin (await #:immediate (test)) (exit-trap T-weak-abort)))))

(define-syntax-rule
  (weak-abort-immediate test-expr expr1 expr2 ...)
  (weak-abort-immediate/proc (λ () test-expr) (λ () expr1 expr2 ...)))

(define (weak-abort-immediate/proc test body)
  (with-trap T-weak-abort-immediate
    (par (begin (body) (exit-trap T-weak-abort-immediate))
         (begin (await #:immediate (test)) (exit-trap T-weak-abort-immediate)))))
