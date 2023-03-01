#lang racket
(require esterel/full pict racket/gui/base)

(define-syntax-rule (loop e ...) (let loop () e ... (loop)))

(define-signal second)

(define (light this-ts that-ts)
  (match-define (ts this-green this-orange this-red) this-ts)
  (match-define (ts that-green that-orange that-red) that-ts)
  (loop
   (unless (or (present? that-green)
               (present? that-orange))
     (traffic-light-stage this-green (* 4 60))
     (traffic-light-stage this-orange 4)
     (traffic-light-stage this-red 2))
   (with-trap T
     (par (sustain this-red)
          (begin (await (present? that-red))
                 (exit-trap T))))
   (pause)))

(define (traffic-light-stage color seconds)
  (with-signal (next-stage)
    (par (abort (sustain color)
                #:when (present? next-stage))
         (begin (await (present? second) #:n seconds)
                (emit next-stage)))))

(define-signal ns-green ns-orange ns-red)
(define-signal ew-green ew-orange ew-red)
(struct ts (green orange red))
(define ns (ts ns-green ns-orange ns-red))
(define ew (ts ew-green ew-orange ew-red))

(define r
  (esterel
   (emit (ts-green ns))
   (par (light ns ew)
        (light ew ns))))

(define (traffic-signal-pict a-ts bindings)
  (match-define (ts green orange red) a-ts)
  (define circle-size 40)
  (define off-color "gray")
  (define (circ signal color)
    (colorize (filled-ellipse circle-size circle-size)
              (if (hash-ref bindings signal #f)
                  color
                  off-color)))
  (vc-append (* circle-size 1/10)
             (circ red "firebrick")
             (circ orange "orange")
             (circ green "forestgreen")))

(define (traffic-signals-pict bindings)
  (inset 
   (hc-append 10
              (traffic-signal-pict ns bindings)
              (traffic-signal-pict ew bindings))
   20))


(define total-seconds-elapsed 0)
(define (n-instants n)
  (set! total-seconds-elapsed (+ total-seconds-elapsed n))
  (for/last ([i (in-range n)])
    (react! r #:emit (list second))))

(define (the-gui)
  (define f (new frame% [label ""]))
  (define p (traffic-signals-pict (react! r #:emit (list second))))
  (define (draw c dc)
    (send dc set-smoothing 'aligned)
    (define-values (cw ch) (send c get-client-size))
    (draw-pict p dc
               (- (/ cw 2) (/ (pict-width p) 2))
               (- (/ ch 2) (/ (pict-height p) 2))))
  (define c (new canvas% [parent f] [paint-callback draw]))
  (send c min-client-width (pict-width p))
  (send c min-client-height (pict-height p))
  (define time-msg (new message% [label ""] [parent f] [stretchable-width #t]))
  (define bp (new horizontal-panel% [parent f] [stretchable-height #f]))
  (new button%
       [label "1 second"]
       [parent bp]
       [callback
        (λ _
          (set! p (traffic-signals-pict (n-instants 1)))
          (refresh-gui))])
  (new button%
       [label "10 seconds"]
       [parent bp]
       [callback
        (λ _
          (set! p (traffic-signals-pict (n-instants 10)))
          (refresh-gui))])
  (new button%
       [label "1 minute"]
       [parent bp]
       [callback
        (λ _
          (set! p (traffic-signals-pict (n-instants 60)))
          (refresh-gui))])
  (define (refresh-gui)
    (send c refresh)
    (send time-msg set-label (~a (quotient total-seconds-elapsed 60)
                                 ":"
                                 (~r (modulo total-seconds-elapsed 60) #:min-width 2 #:pad-string "0")
                                 " elapsed")))
  (refresh-gui)
  (send f show #t))

(module+ main
  (the-gui))
