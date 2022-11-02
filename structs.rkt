#lang racket

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

(struct signal (name combine)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (signal-name x)) "signal"))])

(struct trap (name counter escape)
  #:methods gen:custom-write
  [(define write-proc (mk-write-proc (λ (x) (trap-name x)) "trap"))])

(provide (struct-out signal)
         (struct-out trap))
