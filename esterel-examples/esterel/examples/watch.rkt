#lang racket/base

; IMPORTS & EXPORTS
(require racket/gui/base
         racket/class
         racket/pretty)
(require esterel/full)
(require "./private/watch-signals.rkt")
(require "./private/create-watch.rkt")

(define watch (create-watch))

(define (to-2digit-string num)
  (define as-string (format "~s" num))
  (if (< (string-length as-string) 2)
      (string-append "0" as-string)
      as-string))

(define (update-gui hash)

  (define time (hash-ref hash WATCH_TIME))

  (send hh set-label
        (to-2digit-string
         (if (hash-ref hash 24H_MODE)
             (time-hours time)
             (let ([modded (modulo (time-hours time) 12)])
               (if (= 0 modded)
                   12
                   modded)))))

  (send mm set-label
        (to-2digit-string (time-minutes time)))
  
  (send seconds-display set-label
        (to-2digit-string (time-seconds time)))

  (if (hash-ref hash 24H_MODE)
            (begin (send twenty4h-display set-label "24h")
                   (send ampm-display set-label ""))
            (begin (send twenty4h-display set-label "")
                   (send ampm-display set-label
                         (if (>= (time-hours time) 12)
                "PM"
                "AM"))))

  (define emphasis-color
    (make-object color% 255 0 0))

  (define normal-color
    (make-object color% 0 0 0))

  (case (hash-ref hash WATCH_TIME_POSITION #false)
    [(hours) (send hh set-color emphasis-color)
             (send mm set-color normal-color)
             (send seconds-display set-color normal-color)]
    [(minutes) (send mm set-color emphasis-color)
               (send hh set-color normal-color)
               (send seconds-display set-color normal-color)]
    [(seconds) (send seconds-display set-color emphasis-color)
               (send hh set-color normal-color)
               (send mm set-color normal-color)]
    [else
     (send hh set-color normal-color)
     (send mm set-color normal-color)
     (send seconds-display set-color normal-color)])
  )


;; Font configurations

(define hhmm-font
  (make-object font% 70 'modern 'normal 'bold))

(define seconds-font
  (make-object font% 50 'modern 'normal 'bold))

(define twenty4h-font
  (make-object font% 25 'modern 'normal 'bold))

(define ampm-font
  (make-object font% 25 'modern 'normal 'bold))

;; GUI Elements

(define frame (new frame% [label "Watch"]))

(define entire-space
  (new horizontal-panel%
       [parent frame]))

#;(define test-message
  (new message%
       [parent entire-space]
       [label "hello wordl"]))

;; entire-space children

(define left-buttons
  (new vertical-panel%
       [parent entire-space]))

(define display-with-padding
  (new panel%
       [parent entire-space]
       [style '(border)]))

(define right-buttons
  (new vertical-panel%
       [parent entire-space]))

;; left-buttons children

(define UL-button
  (new button%
     [parent left-buttons]
     [label "UL"]
     [callback (λ (button event)
                 (update-gui (react! watch #:emit (list UL))))]))

(define left-buttons-spacer
  (new vertical-panel%
       [parent left-buttons]
       [min-height 10]))

(define LL-button
  (new button%
     [parent left-buttons]
     [label "LL"]
     [callback (λ (button event)
                 (update-gui (react! watch #:emit (list LL))))]))

;; display-with-padding children

(define rectangular-display
  (new vertical-panel%
       [parent display-with-padding]
       [min-height 100]
       [min-width  500]
       [vert-margin  10]
       [horiz-margin 10]
       [style '(border)]))

;; right-buttons children

(define UR-button
  (new button%
     [parent right-buttons]
     [label "UR"]
     [callback (λ (button event)
                 (update-gui (react! watch #:emit (list UR))))]))

(define right-buttons-spacer
  (new vertical-panel%
       [parent right-buttons]
       [min-height 10]))

(define LR-button
  (new button%
     [parent right-buttons]
     [label "LR"]
     [callback (λ (button event)
                 (update-gui (react! watch #:emit (list LR))))]))

;; rectangular-display children

(define top-third
  (new horizontal-panel%
       [parent rectangular-display]))

(define middle-third
  (new horizontal-panel%
       [parent rectangular-display]))

(define bottom-third
  (new horizontal-panel%
       [parent rectangular-display]))

;; middle-third children

(define ampm24h
  (new vertical-panel%
       [parent middle-third]
       [style '(border)]))

(define m3spacer
  (new panel%
       [parent middle-third]
       [min-width 35]
       [style '(border)]))

;; bottom-third children


(define hhmm-container
  (new horizontal-panel%
       [parent bottom-third]
       [min-width 300]
       [alignment '(center center)]
       [style '(border)]))

(define ss-container
  (new horizontal-panel%
       [parent bottom-third]
       [min-width 200]
       [alignment '(center center)]
       [style '(border)]))

;; hhmm-container children

(define hh
  (new message%
       [parent hhmm-container]
       [auto-resize #t]
       [font hhmm-font]
       [label "12"]))

(define colon
  (new message%
       [parent hhmm-container]
       [auto-resize #t]
       [font hhmm-font]
       [label ":"]))

(define mm
  (new message%
       [parent hhmm-container]
       [auto-resize #t]
       [font hhmm-font]
       [label "00"]))

;; ss-container children

(define seconds-display
  (new message%
       [parent ss-container]
       [font seconds-font]
       [auto-resize #t]
       [label "00"]))

;; ampm24h children

(define ampm-display
  (new message%
       [parent ampm24h]
       [min-height 10]
       [min-width 30]
       [vert-margin 10]
       [auto-resize #t]
       [font ampm-font]
       [label "AM"]))

(define twenty4h-display
  (new message%
       [parent ampm24h]
       [min-height 10]
       [min-width 30]
       [vert-margin 10]
       [auto-resize #t]
       [font twenty4h-font]
       [label ""]))

(define manual-quartz
  (new button%
     [parent frame]
     [label "S"]
     [callback (λ (button event)
                 (update-gui (react! watch #:emit (list S))))]))

(define quartz-timer
  (new timer%
     [notify-callback (λ ()
                        (update-gui
                         (react! watch #:emit (list S))))]))

(define toggle-autoquartz
  (new button%
     [parent frame]
     [label "Enter real(ish)time mode"]
     [callback (λ (button event)
                 (send quartz-timer start 883))]))

(send frame show #t)