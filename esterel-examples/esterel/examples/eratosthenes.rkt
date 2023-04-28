#lang racket
(require esterel/full)

(define-signal result #:combine +)
(define (print-int i) (emit result i))

(define (integers n s-out)
  (pause)
  (emit s-out n)
  (integers (+ n 1) s-out))

(define (not-multiple? n p) (not (= (modulo n p) 0)))

(define (filter prime s-in s-out)
  (loop
   (await (present? s-in))
   (define n (signal-value s-in))
   (when (not-multiple? n prime)
     (emit s-out n))))

(define (shift s-in s-out)
  (await (present? s-in))
  (define prime (signal-value s-in))
  (emit s-out prime)
  (with-signal (s #:init 0 #:combine (λ (x y) x))
    (par (filter prime s-in s)
         (shift s s-out))))

(define (output s-in)
  (loop
   (await (present? s-in))
   (define prime (signal-value s-in))
   (print-int prime)))

(define (sieve)
  (with-signal (nat
                #:init 0 #:combine (λ (x y) x)
                prime
                #:init 0 #:combine (λ (x y) x))
    (par
     (integers 2 nat)
     (shift nat prime)
     (output prime))))

(define r
  (esterel
   (sieve)))

(void (react! r))
(for/list ([i (in-range 2 20)]
           #:when (hash-ref (react! r) result #f))
  i)
