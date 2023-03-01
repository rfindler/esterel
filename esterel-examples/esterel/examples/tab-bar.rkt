#lang racket/gui
(require esterel/full)

(define tab-bar%
  (class canvas%
    (inherit get-client-size)
    (define tabs '())
    (define/override (on-event evt)
      (define-values (cw ch) (get-client-size))
      (react! r #:emit (event->signals cw ch evt)))
    (define/override (on-paint)
      '(for ([(signal val) (in-hash (react! r #:emit (list S-paint)))])
        (interpret-drawing-commands....?)))
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
  event-x
  event-y
  client-w
  client-h)

(define-signal draw-rectangles #:combine set-union)

(define (region-loop in-color out-color l t r b s)
  (let loop ()
    (if (and (<= l (/ (signal-value event-x) (signal-value client-w)) r)
             (<= t (/ (signal-value event-y) (signal-value client-h)) b))
        (emit s in-color)
        (emit s out-color))
    (pause)
    (loop)))

(define-signal
  ul-draw
  lr-draw
  ur-draw
  ll-draw)

(define r
  (esterel
   (par
    (region-loop "forestgreen" "palegreen" 0 0 1/2 1/2 ul-draw)
    (region-loop "forestgreen" "palegreen" 1/2 1/2 1 1 lr-draw)
    (region-loop "lightblue" "navy" 0 1/2 1/2 1 ur-draw)
    (region-loop "lightblue" "navy" 1/2 0 1 1/2 ll-draw))))
