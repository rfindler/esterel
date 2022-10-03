#lang racket
(struct signal (name)
  #:methods gen:custom-write
  [(define (write-proc a-signal port mode)
     (define name (signal-name a-signal))
     (cond
       [name
        (display "#<signal: " port)
        (display name port)
        (display ">" port)]
       [else
        (display "#<signal>" port)]))])
(provide (struct-out signal))