#lang racket
(module+ test (require rackunit))
(require esterel/kernel pict racket/gui/base)

(provide compute-cols/rows/boxes
         compute-houses
         ij->square
         sudoku-gui)

(define (compute-houses cells size)
  (define-values (cols rows boxes)
    (compute-cols/rows/boxes cells size))
  (vector-append cols rows boxes))

(define (compute-cols/rows/boxes cells size)
  (define cols
    (for/vector ([i (in-range size)])
      (for/vector ([j (in-range size)])
        (hash-ref cells (cons i j)))))
  (define rows
    (for/vector ([j (in-range size)])
      (for/vector ([i (in-range size)])
        (hash-ref cells (cons i j)))))

  (define boxes
    (for/vector ([corner (in-list (get-square-corners size))])
      (for/vector ([offset (in-list (get-square-index-offsets size))])
        (hash-ref cells
                  (cons (+ (car corner) (car offset))
                        (+ (cdr corner) (cdr offset)))))))
  (values cols rows boxes))

(define (get-square-index-offsets size)
  (for*/list ([i (in-range (sqrt size))]
              [j (in-range (sqrt size))])
    (cons i j)))
(module+ test
  (check-equal? (get-square-index-offsets 4)
                (list (cons 0 0) (cons 0 1)
                      (cons 1 0) (cons 1 1))))

(define (get-square-corners size)
  (for/list ([i (in-range size)])
    (define sx (* (sqrt size) (modulo i (sqrt size))))
    (define sy (* (sqrt size) (quotient i (sqrt size))))
    (cons sx sy)))
(module+ test
  (check-equal?
   (get-square-corners 4)
   (list (cons 0 0) (cons 2 0) (cons 0 2) (cons 2 2))))

(define (ij->square size i j)
  (+ (* (quotient j (sqrt size)) (sqrt size))
     (quotient i (sqrt size))))
(module+ test
  (check-equal? (ij->square 4 0 0) 0)
  (check-equal? (ij->square 4 1 1) 0)
  (check-equal? (ij->square 4 0 1) 0)
  (check-equal? (ij->square 4 1 0) 0)
  (check-equal? (ij->square 4 2 0) 1)
  (check-equal? (ij->square 4 3 0) 1)
  (check-equal? (ij->square 4 2 1) 1)
  (check-equal? (ij->square 4 3 1) 1)
  (check-equal? (ij->square 4 0 2) 2)
  (check-equal? (ij->square 4 0 3) 2)
  (check-equal? (ij->square 4 1 2) 2)
  (check-equal? (ij->square 4 1 3) 2)
  (check-equal? (ij->square 4 3 3) 3))

(define (transpose v)
  (define w (vector-length (vector-ref v 0)))
  (for/vector ([i (in-range (vector-length v))])
    (for/vector ([j (in-range w)])
      (vector-ref (vector-ref v j) i))))
(module+ test
  (check-equal? (transpose #(#(1 2 3 4)
                             #(a b c d)
                             #(9 8 7 6)
                             #(x y z w)))
                #(#(1 a 9 x)
                  #(2 b 8 y)
                  #(3 c 7 z)
                  #(4 d 6 w))))

(define (add-lines size p)
  (define w (pict-width p))
  (define h (pict-width p))
  (cc-superimpose
   (dc
    (λ (dc dx dy)
      (define (draw-lines heavy?)
        (for ([n (in-range 1 size)])
          (when (equal? heavy? (zero? (modulo n (sqrt size))))
            (send dc set-pen
                  (if heavy? "black" "gray")
                  (if heavy? 3 1)
                  'solid)
            (send dc draw-line
                  (+ dx (* n (/ w size)))
                  dy
                  (+ dx (* n (/ w size)))
                  (+ dy h))
            (send dc draw-line
                  dx
                  (+ dy (* n (/ h size)))
                  (+ dx w)
                  (+ dy (* n (/ h size)))))))
      (define pen (send dc get-pen))
      (draw-lines #f)
      (draw-lines #t)
      (send dc set-pen pen))
    w h)
   p))

(define (ht->pict size ht)
  (for/fold ([p (add-lines size (blank 400 400))])
            ([i (in-range size)])
    (for/fold ([p p])
              ([j (in-range size)])
      (define ht-info (hash-ref ht (cons i j)))
      (define w (/ (pict-width p) size))
      (define h (/ (pict-height p) size))
      (define x (* i w))
      (define y (* j h))
      (define s (cdr ht-info))
      (define cannot-be-p
        (cannot-picts->pict
         (for/list ([i (in-inclusive-range 1 size)])
           (cellophane
            (colorize
             (text (~a i))
             (if (set-member? s i)
                 "red"
                 "forestgreen"))
            .4))))
      (define cell-p
        (vc-append
         (cond
           [(car ht-info)
            =>
            (λ (n) (text (~a n)))]
           [else (ghost (text "0"))])
         (scale cannot-be-p 1/2)))
      (define margin .05)
      (define fit-to-cell
        (scale-to-fit cell-p
                      (* (- 1 margin margin) w)
                      (* (- 1 margin margin) h)))
      (pin-over p
                (+ x (/ w 2) (- (/ (pict-width fit-to-cell) 2)))
                (+ y (/ h 2) (- (/ (pict-height fit-to-cell) 2)))
                fit-to-cell))))

(define (cannot-picts->pict l)
  (let loop ([l l])
    (cond
      [(< (length l) 5) (apply hbl-append l)]
      [else (vc-append (apply hbl-append (take l 5))
                       (loop (drop l 5)))])))

(define (sudoku-gui size step)
  (define ht (step))
  (define the-pict (ht->pict size ht))
  (define steps 0)

  (define (draw c dc)
    (define-values (cw ch) (send c get-client-size))
    (define sp (scale-to-fit the-pict cw ch))
    (draw-pict sp
               dc
               (- (/ cw 2) (/ (pict-width sp) 2))
               (- (/ ch 2) (/ (pict-height sp) 2))))

  (define (solved)
    (for/sum ([(k v) (in-hash ht)])
      (if (car v) 1 0)))
  (define (calc-msg)
    (~a
     (solved)
     " cell"
     (if (= (solved) 1) "" "s")
     " solved in "
     steps
     " step"
     (if (= steps 1) "" "s")))

  (define (button-callback _1 _2)
    (set! steps (+ steps 1))
    (define next-ht (step))
    (define nothing-changed? (equal? next-ht ht))
    (set! ht next-ht)
    (define msg (calc-msg))
    (when nothing-changed?
      (set! msg (string-append msg " (but nothing changed)")))
    (send m set-label msg)
    (set! ht next-ht)
    (set! the-pict (ht->pict size ht))
    (when (= (solved) (* size size))
      (send b enable #f))
    (send c refresh))

  (define f (new frame% [label ""] [width 400] [height 400]))
  (define c (new canvas% [parent f] [paint-callback draw]))
  (define hp (new horizontal-panel% [parent f] [stretchable-height #f]))
  (define b (new button% [label "step"] [callback button-callback] [parent hp]))
  (define m (new message% [parent hp] [label (calc-msg)] [stretchable-width #t]))
  (send f show #t))

(module+ main
  (define size 9)
  (sudoku-gui size
              (λ ()
                (for*/hash ([i (in-range size)]
                            [j (in-range size)])
                  (values (cons i j)
                          (cons (and (zero? (random 10))
                                     (random size))
                                (set 1 2 3)))))))
