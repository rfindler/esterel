#lang racket

(require esterel/full)

; IMPORTS
(require "./watch-signals.rkt")
(require "./watch-button.rkt")
(require "./watch-core.rkt")
(provide create-watch)
   
; MAIN
(define (create-watch)
  (esterel #:pre 1
           (par
            (button)
            (watch))))