#lang racket/gui

(require esterel/full)

(define tab-bar%
  (class canvas%
    (inherit get-client-size)
    (define/override (on-event evt)
      (define-values (cw ch) (get-client-size))
      (react! r #:emit (event->signals cw ch evt))
      (send this refresh))
    (define/override (on-paint)
      (define signals (react! r))
      (define dc (send this get-dc))
      (send dc clear)
      (define-values (cw ch) (get-client-size))
      (send dc set-brush (current-ul-draw-color signals) 'solid)
      (send dc draw-rectangle 0 0 (/ cw 2) (/ ch 2))
      (send dc set-brush (current-ur-draw-color signals) 'solid)
      (send dc draw-rectangle (/ cw 2) 0 (/ cw 2) (/ ch 2))
      (send dc set-brush (current-ll-draw-color signals) 'solid)
      (send dc draw-rectangle 0 (/ ch 2) (/ cw 2) (/ ch 2))
      (send dc set-brush (current-lr-draw-color signals) 'solid)
      (send dc draw-rectangle (/ cw 2) (/ ch 2) (/ cw 2) (/ ch 2)))
    (super-new)))

(define (event->signals cw ch evt)
  (filter
   values
   (list
    (cond
      [(send evt moving?) move-event]
      [(send evt leaving?) leave-event]
      [(send evt entering?) enter-event]
      [else #f])
    (cons client-w cw)
    (cons client-h ch)
    (cons event-x (send evt get-x))
    (cons event-y (send evt get-y)))))

(define-signal
  move-event
  enter-event
  leave-event
  event-x #:init 1 #:combine max
  event-y #:init 1 #:combine max
  client-w #:init 1 #:combine max
  client-h #:init 1 #:combine max)

(define (region-loop l r t b s)
  (let loop ()
    (when (and (<= l (/ (signal-value event-x #:can (set)) (signal-value client-w #:can (set))) r)
               (<= t (/ (signal-value event-y #:can (set)) (signal-value client-h #:can (set))) b))
      (emit s))
    (pause)
    (loop)))

(define-signal
  ul-draw
  lr-draw
  ur-draw
  ll-draw)

(define ((green-draw-color s) emitted)
  (if (hash-ref emitted s #f) "forestgreen" "palegreen"))
(define ((blue-draw-color s) emitted)
  (if (hash-ref emitted s #f) "navy" "lightblue"))
(define current-ul-draw-color (green-draw-color ul-draw))
(define current-ur-draw-color (blue-draw-color ur-draw))
(define current-ll-draw-color (blue-draw-color ll-draw))
(define current-lr-draw-color (green-draw-color lr-draw))

(define r
  (esterel
   (par
    (region-loop 0 1/2 0 1/2 ul-draw)
    (region-loop 0 1/2 1/2 1 ll-draw)
    (region-loop 1/2 1 1/2 1 lr-draw)
    (region-loop 1/2 1 0 1/2 ur-draw))))

(define win (new frame% [label "test"]))
(define canvas (new tab-bar% [parent win]))
(send win show #t)

