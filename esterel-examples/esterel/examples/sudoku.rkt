#lang racket
(require esterel/full
         "private/sudoku-helpers.rkt")

#;
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
1...9...4
.8.37.1..
..9..73.5
.........
2.35..4..
9..7.....
..1.58.2.
6...1...7
--
  )

;; x : Natural
;; y : Natural
;; must-be : (Signal (or/c #f Natural))
;; cannot-be : (Signal (Set Natural))
(struct cell (x y must-be cannot-be) #:transparent)

;; size : Natural
;; reads the size from the string in `sudoku-board`
(define size (string-length (car (regexp-split #rx"\n" sudoku-board))))

(define cells
  (for*/hash ([x (in-range size)]
              [y (in-range size)])
    (values (cons x y)
            (cell x y
                  (make-global-signal (~a "(" x "," y ")-must")
                                      #:init #f #:combine (λ (x y) y))
                  (make-global-signal (~a "(" x "," y ")-cannot")
                                      #:init (set) #:combine set-union)))))

(define (get-cell-xy x y) (hash-ref cells (cons x y)))

(define-values (cols rows squares)
  (compute-blocks cells size))

(define (solve-sudoku)
  (par
   (sustain-initial-cells)
   (emit-cannot-be)
   (cell-with-only-one-option)
   (last-remaining)))

(define (sustain-initial-cells)
  (loop
   (parse-sudoku-board
    (λ (x y n)
      (match-define (cell _ _ must-be cannot-be) (hash-ref cells (cons x y)))
      (emit must-be n)))
   (pause)))

(define (parse-sudoku-board f)
  (for ([l (in-lines (open-input-string sudoku-board))]
        [y (in-naturals)])
    (for ([c (in-string l)]
          [x (in-naturals)])
      (unless (equal? c #\.)
        (f x y (- (char->integer c) (char->integer #\0)))))))

(define (emit-cannot-be)
  (loop
   (for ([(_ a-cell) (in-hash cells)])
     (match-define (cell my-x my-y must-be cannot-be) a-cell)
     (cross-out my-x my-y cannot-be (vector-ref cols my-x))
     (cross-out my-x my-y cannot-be (vector-ref rows my-y))
     (cross-out my-x my-y cannot-be (vector-ref squares (ij->square size my-x my-y))))
   (pause)))

(define (cross-out my-x my-y cannot-be block)
  (for ([sibling-cell (in-vector block)])
    (match-define (cell sibling-x sibling-y sibling-must-be _)
      sibling-cell)
    (define N (signal-value sibling-must-be #:can (set cannot-be)))
    (when N
      (unless (and (= sibling-x my-x)
                   (= sibling-y my-y))
        (emit cannot-be (set N))))))

(define (cell-with-only-one-option)
  (for/par ([x (in-range size)])
    (for/par ([y (in-range size)])
      (loop
       (define a-cell (hash-ref cells (cons x y)))
       (match-define (cell _1 _2 must-be cannot-be) a-cell)
       (define sv (signal-value cannot-be #:pre 1))
       (cond
         [(= (- size 1) (set-count sv))
          (define must-n (get-remaining-option sv))
          (sustain must-be must-n)]
         [else
          (pause)])))))

(define (get-remaining-option sv)
  (set-first
   (set-subtract
    all-possible-ns
    sv)))
(define all-possible-ns
  (for/set ([i (in-range 1 (+ size 1))])
    i))

(define (last-remaining)
  (for/par ([block (in-vector squares)])
    (last-remaining-in-block block))
  (for/par ([block (in-vector cols)])
    (last-remaining-in-block block))
  (for/par ([block (in-vector rows)])
    (last-remaining-in-block block)))

(define (last-remaining-in-block block)
  (for/par ([n (in-inclusive-range 1 size)])
    (loop
     (define the-remaining
       (filter
        values
        (for/list ([a-cell (in-vector block)])
          (match-define (cell _ _ must-be cannot-be) a-cell)
          (cond
            [(set-member? (signal-value cannot-be #:pre 1) n)
             #f]
            [(signal-value must-be #:pre 1)
             #f]
            [else a-cell]))))
     (cond
       [(= (length the-remaining) 1)
        (match-define (cell _ _ must-be cannot-be) (car the-remaining))
        (sustain must-be n)]
       [else
        (pause)]))))

(define r
  (esterel
   #:pre 1
   (solve-sudoku)))

(define (cell-information ht)
  (for*/hash ([x (in-range size)]
              [y (in-range size)])
    (match-define (cell _ _ must-be cannot-be) (hash-ref cells (cons x y)))
    (values (cons x y)
            (cons (hash-ref ht must-be #f)
                  (hash-ref ht cannot-be (set))))))

(module+ main
  (define (step) (cell-information (react! r)))
  (sudoku-gui size step))
