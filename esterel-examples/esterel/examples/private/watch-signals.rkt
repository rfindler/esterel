#lang racket

(require esterel/full)

; EXPORTS
(provide (all-defined-out))

; INPUTS
(define-signal S)
(define-signal LL LR UL UR)

; VIEW WATCH
(struct time (hours minutes seconds) #:transparent)
(define-signal WATCH_TIME #:init (time 0 0 0) #:combine (λ (a b) a))
;(define-signal INCREMENT_WATCH_TIME)
(define-signal 24H_MODE #:init #f #:combine (λ (a b) (and a b)))
(define-signal TOGGLE_24H_MODE_COMMAND)

; SET WATCH
(define-signal WATCH_TIME_POSITION #:init 'hours #:combine (λ (a b) a))
;(define-signal INCREMENT_WATCH_POSITION)
(define-signal ENTER_SET_WATCH_MODE_COMMAND)
(define-signal EXIT_SET_WATCH_MODE_COMMAND)
(define-signal SET_WATCH_COMMAND)
(define-signal NEXT_WATCH_TIME_POSITION_COMMAND)

; STOPWATCH

; VIEW ALARM

; SET ALARM