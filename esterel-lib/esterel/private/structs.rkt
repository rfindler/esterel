#lang racket/base
(require racket/fixnum)

;; these two functions (->fx and fxmix-hash-code) should
;; be replaced with a dependency on racket/hash-code and
;; a call to hash-code-combine once the next release is out.
(define (->fx v [who '->fx])
  (cond
    [(fixnum? v) v]
    [(exact-integer? v) (bitwise-and v (most-positive-fixnum))]
    [else (raise-argument-error who "exact-integer?" v)]))
(define (fxmix-hash-code hc)
  (let ([hc2 (fx+/wraparound hc (fxlshift/wraparound (fx+/wraparound hc 1) 10))])
    (fxxor hc2 (fxrshift/logical hc2 6))))

(define (mk-write-proc get-name what)
  (define (write-proc a-signal port mode)
    (define name (get-name a-signal))
    (cond
      [name
       (display "#<" port)
       (display what port)
       (display ": " port)
       (display name port)
       (display ">" port)]
      [else
       (display "#<" port)
       (display what port)
       (display ">" port)]))
  write-proc)

;; identity controls the equivalence relation on signals
;; if it is #f, then the comparison is just like a regular
;; opaque struct, namely based on each allocation being different
;; from all the others. This works fine when we're not in a
;; reaction, but when we are, we might create a signal during
;; a can exploration. In that case, we should create the same
;; signal during a later must run. See `mk-signal.args` for
;; how that is done
(struct signal (name identity combine)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (signal-name x)) "signal"))]
  #:methods gen:equal-mode+hash
  [(define (equal-mode-proc self other rec mode)
     (cond
       [(signal? other)
        (define self-id (signal-identity self))
        (define other-id (signal-identity other))
          (cond
            [(and self-id other-id)
             ;; the identity of the signal is not a generic thing
             ;; inside a signal but instead something where the
             ;; predefined equal? is the correct comparison
             (equal? self-id other-id)]
            [else
             (eq? self other)])]
       [else #f]))
   (define (hash-mode-proc self rec mode)
     (define self-id (signal-identity self))
     (cond
       [self-id (fxmix-hash-code (->fx (equal-hash-code self-id)))]
       [else (eq-hash-code self)]))])

(struct trap (name counter escape)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (trap-name x)) "trap"))])

(provide (struct-out signal)
         (struct-out trap))
