#lang racket
(require esterel/full
         "private/sudoku-helpers.rkt")

(struct cell (x y must-be cannot-be) #:transparent)

(define size 4)

(define-signals cells mk-signal
  (for/vector ([x (in-range size)])
    (for/vector ([y (in-range size)])
      (cell x y
            (mk-signal (~a "(" x "," y ") must")
                       #:init #f #:combine (λ (x y) y))
            (mk-signal (~a "(" x "," y ") cannot")
                       #:init (set) #:combine set-union)))))

(define (get-cell-xy x y)
  (vector-ref (vector-ref cells x) y))

(define-values (cols rows squares)
  (compute-blocks cells size))

(define sudoku-board #<<--
...3
.4..
..32
....
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

(define (sustain-initial-cells)
  (loop
   (for ([l (in-lines (open-input-string sudoku-board))]
         [y (in-naturals)])
     (for ([c (in-string l)]
           [x (in-naturals)])
       (unless (equal? c #\.)
         (match-define (cell _ _ must-be cannot-be) (get-cell-xy x y))
         (emit must-be (- (char->integer c) (char->integer #\0))))))
   (pause)))

(define (cannot-be)
  (for* ([my-x (in-range size)]
         [my-y (in-range size)])
    (define my-col (vector-ref cols my-x))
    (define my-row (vector-ref rows my-y))
    (define my-square (vector-ref squares (ij->square size my-x my-y)))
    (match-define (cell _ _ must-be cannot-be) (get-cell-xy my-x my-y))
    (define (cross-out row/col/square)
      (for ([sibling-cell (in-vector row/col/square)])
        (match-define (cell sibling-x sibling-y sibling-must-be _)
          sibling-cell)
        (define N (signal-value sibling-must-be #:can (set cannot-be)))
        (when N
          (unless (and (= sibling-x my-x)
                       (= sibling-y my-y))
            (emit cannot-be (set N))))))
    (cross-out my-row)
    (cross-out my-col)
    (cross-out my-square)))

(define r
  (esterel
   #:pre 1
   (par
    (sustain-initial-cells)

    (loop
     (cannot-be)
     (pause))

    (for*/par ([x (in-range size)]
               [y (in-range size)])
      (loop
       (define a-cell (get-cell-xy x y))
       (match-define (cell _1 _2 must-be cannot-be) a-cell)
       (define sv (signal-value cannot-be #:pre 1))
       (cond
         [(= (- size 1) (set-count sv))
          (define must-n
            (set-first
             (set-subtract
              (for/set ([i (in-range size)]) (+ i 1))
              sv)))
          (sustain must-be must-n)]
         [else
          (pause)]))))))

(define (cell-information ht)
  (for*/hash ([x (in-range size)]
              [y (in-range size)])
    (match-define (cell _ _ must-be cannot-be) (get-cell-xy x y))
    (values (cons x y)
            (cons (hash-ref ht must-be #f)
                  (hash-ref ht cannot-be (set))))))

(module+ main
  (define (step) (cell-information (react! r)))
  (sudoku-gui size step))
