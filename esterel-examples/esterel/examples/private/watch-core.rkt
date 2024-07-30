#lang racket

(require esterel/full)

; IMPORTS & EXPORTS
(require "./watch-signals.rkt")
(provide watch)

(define get-next-watch-time-position
  (λ (current-position)
    (cond
      [(equal? current-position 'hours) 'minutes]
      [(equal? current-position 'minutes) 'seconds]
      [(equal? current-position 'seconds) 'hours])))

(define (increment-time cur-time mode)
  (match mode
    ('set-hours (set-mode-increment-time cur-time 'hours))
    ('set-minutes (set-mode-increment-time cur-time 'minutes))
    ('set-seconds (set-mode-increment-time cur-time 'seconds))
    ('view (view-mode-increment-time cur-time))
    (_ (raise-argument-error 'increment-time "'set-hour, 'set-minute, 'set-second, or 'view" mode))))

(define (view-mode-increment-time cur-time)
  (if (= 59 (time-seconds cur-time))
      (if (= 59 (time-minutes cur-time))
          (if (= 23 (time-hours cur-time))
              (time 0 0 0)
              (time (+ 1 (time-hours cur-time)) 0 0))
          (time (time-hours cur-time) (+ 1 (time-minutes cur-time)) 0))
      (time (time-hours cur-time) (time-minutes cur-time) (+ 1 (time-seconds cur-time)))))

(define (set-mode-increment-time cur-time position)
  (match position
    ('seconds cur-time)
    ('minutes (time (time-hours cur-time) (time-minutes cur-time) (modulo (+ 1 (time-seconds cur-time)) 60)))
    ('hours (if (= 59 (time-seconds cur-time))
                (time (time-hours cur-time) (modulo (+ 1 (time-minutes cur-time)) 60) 0)
                (time (time-hours cur-time) (time-minutes cur-time) (+ 1 (time-seconds cur-time)))))))

(define (get-mode-from-watch-position position)
  (match position
    ('hours 'set-hours)
    ('minutes 'set-minutes)
    ('seconds 'set-seconds)
    (_ (raise-argument-error 'get-mode-from-watch-position "'hours, 'minutes, or 'seconds" position))))

(define (increment-position cur-time position)
  (match position
    ('hours (time (modulo (+ 1 (time-hours cur-time)) 24) (time-minutes cur-time) (time-seconds cur-time)))
    ('minutes (time (time-hours cur-time) (modulo (+ 1 (time-minutes cur-time)) 60) (time-seconds cur-time)))
    ('seconds (time (time-hours cur-time) (time-minutes cur-time) (modulo (+ 1 (time-seconds cur-time)) 60)))))

(define (repeat signal can)
  (emit signal (signal-value signal #:pre 1 #:can can)))

; LISTENERS
(define view-watch
  (λ ()
    (with-trap EXIT_VIEW_MODE
      (let loop ()
        (await #:cases
               [#:immediate (and (present? ENTER_SET_WATCH_MODE_COMMAND) (not (present? EXIT_SET_WATCH_MODE_COMMAND)))
                (exit-trap EXIT_VIEW_MODE)]
               [#:immediate (present? S)
                (begin (repeat 24H_MODE (set))
                       (emit WATCH_TIME (increment-time (signal-value WATCH_TIME #:pre 1 #:can (set)) 'view)))]
               [#:immediate (present? TOGGLE_24H_MODE_COMMAND)
                (begin (emit 24H_MODE (not (signal-value 24H_MODE #:pre 1 #:can (set))))
                       (repeat WATCH_TIME (set)))]
               [#:immediate #t
                (begin (repeat WATCH_TIME (set))
                       (repeat 24H_MODE (set)))])
        (pause)
        (loop)))))

(define set-watch
  (λ ()
    (with-trap EXIT_SET_MODE
      (emit WATCH_TIME_POSITION 'hours)
      (let loop ()
        (await #:cases
               [#:immediate (and (present? EXIT_SET_WATCH_MODE_COMMAND) (not (present? ENTER_SET_WATCH_MODE_COMMAND)))
                (exit-trap EXIT_SET_MODE)]
               [#:immediate (present? S)
                (begin (emit WATCH_TIME (increment-time (signal-value WATCH_TIME #:pre 1 #:can (set))
                                                        (get-mode-from-watch-position (signal-value WATCH_TIME_POSITION #:pre 1 #:can (set)))))
                       (repeat WATCH_TIME_POSITION (set))
                       (repeat 24H_MODE (set)))]
               [#:immediate (present? SET_WATCH_COMMAND)
                (begin (emit WATCH_TIME (increment-position (signal-value WATCH_TIME #:pre 1 #:can (set))
                                                            (signal-value WATCH_TIME_POSITION #:pre 1 #:can (set))))
                       (repeat WATCH_TIME_POSITION (set))
                       (repeat 24H_MODE (set)))]
               [#:immediate (present? NEXT_WATCH_TIME_POSITION_COMMAND)
                (begin (emit WATCH_TIME_POSITION (get-next-watch-time-position (signal-value WATCH_TIME_POSITION #:pre 1 #:can (set))))
                       (repeat WATCH_TIME (set))
                       (repeat 24H_MODE (set)))]
               [#:immediate #t
                (begin (repeat WATCH_TIME_POSITION (set))
                       (repeat WATCH_TIME (set))
                       (repeat 24H_MODE (set)))])
        (pause)
        (loop)))))

; MAIN
(define watch
  (λ () (let loop ()
          (view-watch)
          (set-watch)
          (loop))))