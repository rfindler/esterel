#lang racket/base

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
       ;; bitwise-xor should be replaced by a call to hash-code-mix after 8.9 is out
       [self-id (bitwise-xor (equal-hash-code self-id))]
       [else (eq-hash-code self)]))])

(struct trap (name counter escape)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (trap-name x)) "trap"))])

(provide (struct-out signal)
         (struct-out trap))
