#lang racket/gui
(require esterel/full)

#|

This is a conversion of the example from section 4.1 in
HipHop.js: (A)Synchronous Reactive Web Programming
by Berry and Serrano, PLDI 2020

|#

(define smart-button%
  (class canvas%
    (init-field label callback)
    (inherit get-client-size get-dc refresh)
    (define in-color "gray")
    (define out-color "dark gray")
    (define blink-on? #f)
    (define/public (set-color _in-color _out-color)
      (unless (send the-color-database find-color _in-color)
        (error 'set-color "unknown color ~s" _in-color))
      (unless (send the-color-database find-color _out-color)
        (error 'set-color "unknown color ~s" _out-color))
      (set! in-color _in-color)
      (set! out-color _out-color)
      (refresh))
    (define/override (on-paint)
      (define dc (get-dc))
      (define-values (tw th _1 _2) (send dc get-text-extent label))
      (define-values (cw ch) (get-client-size))
      (send dc set-brush (if evt-in? in-color out-color) 'solid)
      (send dc set-pen "solid" 1 'transparent)
      (send dc draw-rectangle 0 0 cw ch)
      (when blink-on?
        (send dc set-brush "pink" 'solid)
        (send dc draw-ellipse (* cw .1) (* ch .1) (* cw .8) (* ch .8)))
      (send dc draw-text
            label
            (- (/ cw 2) (/ tw 2))
            (- (/ ch 2) (/ th 2))))

    (define timer (new timer%
                       [notify-callback
                        (λ ()
                          (set! blink-on? (not blink-on?))
                          (refresh))]))
    (define/public (set-blinking blink?)
      (cond
        [blink?
         (set! blink-on? #f)
         (send timer start 400)]
        [else
         (send timer stop)
         (set! blink-on? #f)
         (refresh)]))
    
    (define xy-in? #f)
    (define/private (set-xy-in n) (unless (equal? xy-in? n) (set! xy-in? n) (refresh)))
    (define evt-in? #f)
    (define/private (set-evt-in n) (unless (equal? evt-in? n) (set! evt-in? n) (refresh)))

    (define/override (on-event evt)
      (define-values (cw ch) (get-client-size))
      (set-xy-in
       (and (<= 0 (send evt get-x) cw)
            (<= 0 (send evt get-y) ch)))
      (when (send evt entering?) (set-evt-in #t))
      (when (send evt leaving?) (set-evt-in #f))
      (when (and xy-in? (send evt button-up?))
        (callback)))
    (super-new)))


(define f (new frame% [label "Lisinopril"] [width 300] [height 600]))
(define (try-callback) (time-has-passed (list try)))
(define try-button (new smart-button% [label "try"] [callback try-callback] [parent f]))
(define (confirm-callback) (time-has-passed (list confirm)))
(define confirm-button (new smart-button% [label "confirm"] [callback confirm-callback] [parent f]))
(define tick-button (new button% [label "tick"] [callback (λ _ (tick-hours))] [parent f]))
(define time (new message% [parent f] [label ""] [stretchable-width #t]))
(define messages (new message% [parent f] [label ""] [stretchable-width #t]))
(define timer
  (new timer%
       [notify-callback
        (λ ()
          (tick-hours))]))

(define the-time 0)
(define (tick-hours)
  (set! the-time (modulo (+ the-time 1) 24))
  (send time set-label (~a the-time ":00"))
  (time-has-passed (list tick)))

(define (button d tick B Active-true Active-false Alert-true Alert-false)
  (emit Active-true)
  (emit Alert-false)
  (abort (await (present? tick) #:n d)
         (let loop ()
           (when (present? tick)
             (emit Alert-true))
           (pause)
           (loop))
         #:when (present? B))
  (emit Alert-false)
  (emit Active-false))

(define try-delay 8)
(define confirm-delay 3)

(define max-dose-interval 34)
(define min-dose-interval 8)

(define-signal tick)

(define-signal
  try
  try-active-true
  try-active-false
  try-alert-true
  try-alert-false)

(define-signal
  confirm
  confirm-active-true
  confirm-active-false
  confirm-alert-true
  confirm-alert-false)

(define-signal
  deliver-dose
  in-dose-window
  try-not-in-window-warning
  no-dose-since-too-long-error
  try-too-close-error)

(define-signal record-dose)

(define r
  (esterel
   (let loop ()
     (with-trap dose-ok
       (par (begin
              (button try-delay tick try
                      try-active-true try-active-false
                      try-alert-true try-alert-false)
              (emit deliver-dose)
              (unless (present? in-dose-window)
                (emit try-not-in-window-warning))
              (button confirm-delay tick confirm
                      confirm-active-true confirm-active-false
                      confirm-alert-true confirm-alert-false)
              (emit record-dose)
              (exit-trap dose-ok))
            (begin
              (await tick #:n (- max-dose-interval min-dose-interval))
              (sustain no-dose-since-too-long-error))))
     (with-trap delay-passed
       (par (every try #:do (emit try-too-close-error))
            (begin (await tick #:n min-dose-interval)
                   (exit-trap delay-passed))))
     (loop))))

(define (time-has-passed input-signals)
  (define output-signals (react! r #:emit input-signals))
  (cond
    [(hash-ref output-signals try-alert-true #f)
     (send try-button set-blinking #t)]
    [(hash-ref output-signals try-alert-false #f)
     (send try-button set-blinking #f)])
  (cond
    [(hash-ref output-signals try-active-true #f)
     (send try-button set-color "forestgreen" "green")]
    [(hash-ref output-signals try-active-false #f)
     (send try-button set-color "gray" "lightgray")])
  (cond
    [(hash-ref output-signals confirm-alert-true #f)
     (send confirm-button set-blinking #t)]
    [(hash-ref output-signals confirm-alert-false #f)
     (send confirm-button set-blinking #f)])
  (cond
    [(hash-ref output-signals confirm-active-true #f)
     (send confirm-button set-color "forestgreen" "green")]
    [(hash-ref output-signals confirm-active-false #f)
     (send confirm-button set-color "gray" "lightgray")])
  (define msg "")
  (when (hash-ref output-signals record-dose #f)
    (printf "dose confirmed at ~a\n" the-time))
  (for ([s (in-list (list try-not-in-window-warning
                          no-dose-since-too-long-error
                          try-too-close-error
                          record-dose))])
    (when (hash-ref output-signals s #f)
      (unless (equal? msg "") (set! msg (string-append msg " ")))
      (set! msg (string-append msg (signal-name s)))))
  (send messages set-label msg))


(module+ main
  (send f show #t)
  ;(send timer start 333)
  )
