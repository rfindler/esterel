#lang racket
(require esterel/full
         pict
         racket/gui/base
         (only-in slideshow para current-line-sep)
         (for-syntax racket/format
                     syntax/parse))
(module+ test (require rackunit))

(struct cell (x y must-be cannot-be) #:transparent)

(define size 9)
(define-signals cells mk-signal
  (for/vector ([x (in-range size)])
    (for/vector ([y (in-range size)])
      (cell x y
            (mk-signal (~a "(" x "," y ") must")
                       #:init #f #:combine (λ (x y) y))
            (mk-signal (~a "(" x "," y ") cannot")
                       #:init (set) #:combine set-union)))))

(define sudoku-board #<<--
.4.....8.
..7....6.
....1....
41....2..
.....5...
.3.......
..6..7..3
..58.6...
........1
--
  )
#;
(define sudoku-board #<<--
..17..5.9
573.241.6
8..5.1..2
7..295.18
..94..3.5
6528....7
465.8..71
...159..4
9.8..7.53
--
  )
#;
(define sudoku-board #<<--
...3
.4..
..32
....
--
  )

(define (get-cell-xy x y) (vector-ref (vector-ref cells x) y))

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
(define cols
  (for/vector ([i (in-range size)])
    (vector-ref cells i)))
(define rows
  (let ([t (transpose cells)])
    (for/vector ([i (in-range size)])
      (vector-ref t i))))
(define (get-square-corners size)
  (for/list ([i (in-range size)])
    (define sx (* (sqrt size) (modulo i (sqrt size))))
    (define sy (* (sqrt size) (quotient i (sqrt size))))
    (cons sx sy)))
(module+ test
  (check-equal?
   (get-square-corners 4)
   (list (cons 0 0) (cons 2 0) (cons 0 2) (cons 2 2))))
(define (get-square-index-offsets size)
  (for*/list ([i (in-range (sqrt size))]
              [j (in-range (sqrt size))])
    (cons i j)))
(module+ test
  (check-equal? (get-square-index-offsets 4)
                (list (cons 0 0) (cons 0 1)
                      (cons 1 0) (cons 1 1))))
(define squares
  (for/vector ([corner (in-list (get-square-corners size))])
    (for/vector ([offset (in-list (get-square-index-offsets size))])
      (vector-ref (vector-ref cells (+ (car corner) (car offset)))
                  (+ (cdr corner) (cdr offset))))))

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

(define (ht->pict ht)
  (for/fold ([p (blank 400 400)])
            ([i (in-range size)])
    (for/fold ([p p])
              ([j (in-range size)])
      (match-define (cell _1 _2 must-be cannot-be)
        (get-cell-xy i j))
      (define w (/ (pict-width p) size))
      (define h (/ (pict-height p) size))
      (define x (* i w))
      (define y (* j h))
      (define s (hash-ref ht cannot-be set))
      (define cannot-be-p
        (parameterize ([current-line-sep -20])
          (apply
           para
           #:width 80
           #:fill? #f
           ;2
           (for/list ([i (in-range 1 (+ size 1))])
             (cellophane
              (colorize
               (text (~a i))
               (if (set-member? s i)
                   "red"
                   "forestgreen"))
              .4)))))
      (define cell-p
        (vc-append
         -4
         (cond
           [(hash-ref ht must-be #f)
            =>
            (λ (n) (text (~a n)))]
           [else (ghost (text "0"))])
         (scale cannot-be-p 1/2)))
      (pin-over p x y
                (cc-superimpose
                 (rectangle w h)
                 (scale-to-fit cell-p
                               (* .9 w)
                               (* .9 h)))))))

(define (bits->number ht bits)
  (for/sum ([bit (in-list bits)]
            [i (in-naturals)])
    (if (hash-ref ht bit #f)
        (expt 2 i)
        0)))

(define (for/par N f)
  (let loop ([i N])
    (cond
      [(zero? i) (void)]
      [else (par (f (- i 1))
                 (loop (- i 1)))])))

(define (sustain-initial-cells initial-cells)
  (loop
   (for ([initial-cell (in-list initial-cells)])
     (emit
      (cell-must-be
       (get-cell-xy (list-ref initial-cell 0)
                    (list-ref initial-cell 1)))
      (list-ref initial-cell 2)))
   (pause)))

(define initial-cells
  (for/list ([l (in-lines (open-input-string sudoku-board))]
             [y (in-naturals)]
             #:when #t
             [c (in-string l)]
             [x (in-naturals)]
             #:unless (equal? c #\.))
    (list x y (- (char->integer c) (char->integer #\0)))))

(define all-possible-values
  (for/set ([i (in-range size)]) (+ i 1)))

(define r
  (esterel
   #:pre 1
   (par
    (sustain-initial-cells initial-cells)
    (loop
     (for/par size
       (λ (x)
         (for/par size
           (λ (y)
             (loop
              (define a-cell (get-cell-xy x y))
              (match-define (cell _1 _2 must-be cannot-be) a-cell)
              (define sv (signal-value cannot-be #:pre 1))
              (cond
                [(= (- size 1) (set-count sv))
                 (define ans (set-first
                              (set-subtract
                               all-possible-values
                               sv)))
                 (loop
                  (emit must-be ans)
                  (pause))]
                [else
                 (pause)])))))))

    (loop
     (for ([x (in-range size)])
       (for ([y (in-range size)])
         (define my-col (vector-ref cols x))
         (define my-row (vector-ref rows y))
         (define my-square (vector-ref squares (ij->square size x y)))
         (match-define (cell _1 _2 must-be cannot-be) (get-cell-xy x y))
         (define (cross-out row/col/square)
           (for ([a-cell (in-vector row/col/square)])
             (define N (signal-value (cell-must-be a-cell) #:can (set)))
             (when (and N
                        (not (and (= (cell-x a-cell) x)
                                  (= (cell-y a-cell) y))))
               (emit cannot-be (set N)))))
         (cross-out my-row)
         (cross-out my-col)
         (cross-out my-square)))
     (pause)))))

(define the-ht (react! r))
(define the-pict (ht->pict the-ht))

(define (draw c dc)
  (define-values (cw ch) (send c get-client-size))
  (define sp (scale-to-fit the-pict cw ch))
  (draw-pict sp
             dc
             (- (/ cw 2) (/ (pict-width sp) 2))
             (- (/ ch 2) (/ (pict-height sp) 2))))

(define (next _1 _2)
  (set! the-ht (react! r))
  (set! the-pict (ht->pict the-ht))
  (send c refresh))

(define f (new frame% [label ""] [width 400] [height 400]))
(define c (new canvas% [parent f] [paint-callback draw]))
(define b (new button% [label "next"] [callback next] [parent f]))
(send f show #t)
