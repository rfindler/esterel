#lang racket

(require esterel/full)

; IMPORTS & EXPORTS
(require "./watch-signals.rkt")
(provide button)

; BRANCHES
(define view-watch::LR_received
  (λ () (let loop ()
          (if (present? LR)
              (emit TOGGLE_24H_MODE_COMMAND)
              (void))
          (pause)
          (loop))))
(define set-watch::LL_received
  (λ () (let loop ()
          (if (present? LL)
              (emit NEXT_WATCH_TIME_POSITION_COMMAND)
              (void))
          (pause)
          (loop))))
(define set-watch::LR_received
  (λ () (let loop ()
          (if (present? LR)
              (emit SET_WATCH_COMMAND)
              (void))
          (pause)
          (loop))))

; MAIN
(define view-watch
  (λ ()
    (abort
     (par
      (view-watch::LR_received))
     #:when (present? UL))
    (emit ENTER_SET_WATCH_MODE_COMMAND)))

(define set-watch
  (λ ()
    (abort
     (par
      (set-watch::LL_received)
      (set-watch::LR_received))
     #:when (present? UL))
    (emit EXIT_SET_WATCH_MODE_COMMAND)))

(define button
  (λ () (let loop ()
          (view-watch)
          (set-watch)
          (loop))))