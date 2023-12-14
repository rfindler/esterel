#lang racket
(require esterel/full
         "private/sudoku-helpers.rkt")
(module+ test (require rackunit))

(define 4x4-sudoku-board #<<--
1..2
..3.
.1..
4..3
--
  )

;; these three 9x9 boards here are courtesy of the Sudoku Exchange
;; Puzzle Bank, https://github.com/grantm/sudoku-exchange-puzzle-bank

;; can be solved without `last-remaining`
(define simplest-sudoku-board #<<--
.5.7.3.6.
..7...8..
...816...
....3....
..5...1..
73..4..86
9.6...2.4
84.572.93
...4.9...
--
  )

;; cannot be solved without `last-remaining`
(define medium-sudoku-board #<<--
1......2.
2..73..4.
8..1..6..
..13.....
.........
.....58..
..6..4..5
.1..52..9
.9......7
--
  )

;; cannot be solved by code here
(define hard-sudoku-board #<<--
.........
7...6...3
..41.89..
...4.1...
.6..2..9.
.........
..15.48..
97.....26
.........
--
  )


;; x : Natural
;; y : Natural
;; must-be : (Signal (or/c #f Natural))
;; cannot-be : (Signal (Set Natural))
(struct cell (x y must-be cannot-be) #:transparent)

(define (build-cells-table size)
  (for*/hash ([x (in-range size)]
              [y (in-range size)])
    (values (cons x y)
            (cell x y
                  (make-global-signal (~a "(" x "," y ")-must")
                                      #:init #f #:combine (λ (x y) y))
                  (make-global-signal (~a "(" x "," y ")-cannot")
                                      #:init (set) #:combine set-union)))))

(define (solve-sudoku board size cells)
  (par
   (sustain-initial-cells board cells)
   (emit-cannot-be size cells)
   (cell-with-only-one-option size cells)
   (last-remaining size cells)))

(define (sustain-initial-cells board cells)
  (loop
   (parse-sudoku-board
    board
    (λ (x y n)
      (match-define (cell _ _ must-be cannot-be) (hash-ref cells (cons x y)))
      (emit must-be n)))
   (pause)))

(define (parse-sudoku-board sudoku-board f)
  (for ([l (in-lines (open-input-string sudoku-board))]
        [y (in-naturals)])
    (for ([c (in-string l)]
          [x (in-naturals)])
      (unless (equal? c #\.)
        (f x y (- (char->integer c) (char->integer #\0)))))))

(define (emit-cannot-be size cells)
  (define-values (cols rows squares)
    (compute-blocks cells size))
  (loop
   (for ([(_ a-cell) (in-hash cells)])
     (match-define (cell my-x my-y my-must-be my-cannot-be) a-cell)
     (cross-out my-x my-y my-cannot-be (vector-ref cols my-x))
     (cross-out my-x my-y my-cannot-be (vector-ref rows my-y))
     (cross-out my-x my-y my-cannot-be (vector-ref squares (ij->square size my-x my-y)))
     (define my-n (signal-value my-must-be #:can (set my-cannot-be)))
     (when my-n
       (emit my-cannot-be (set-remove (all-possible-ns size) my-n))))
   (pause)))

(define (cross-out my-x my-y my-cannot-be block)
  (for ([sibling-cell (in-vector block)])
    (match-define (cell sibling-x sibling-y sibling-must-be _)
      sibling-cell)
    (define sibling-n (signal-value sibling-must-be #:can (set my-cannot-be)))
    (when sibling-n
      (unless (and (= sibling-x my-x)
                   (= sibling-y my-y))
        (emit my-cannot-be (set sibling-n))))))

(define (cell-with-only-one-option size cells)
  (for/par ([x (in-range size)])
    (for/par ([y (in-range size)])
      (loop
       (define a-cell (hash-ref cells (cons x y)))
       (match-define (cell _1 _2 must-be cannot-be) a-cell)
       (define sv (signal-value cannot-be #:pre 1))
       (cond
         [(= (- size 1) (set-count sv))
          (define must-n (get-remaining-option size sv))
          (sustain must-be must-n)]
         [else
          (pause)])))))

(define (get-remaining-option size sv)
  (set-first
   (set-subtract
    (all-possible-ns size)
    sv)))
(define (all-possible-ns size)
  (for/set ([i (in-range 1 (+ size 1))])
    i))

(define (last-remaining size cells)
  (define-values (cols rows squares)
    (compute-blocks cells size))
  (for/par ([block (in-vector squares)])
    (last-remaining-in-block size block))
  (for/par ([block (in-vector cols)])
    (last-remaining-in-block size block))
  (for/par ([block (in-vector rows)])
    (last-remaining-in-block size block)))

(define (last-remaining-in-block size block)
  (for/par ([n (in-inclusive-range 1 size)])
    (loop
     (define the-remaining
       (for/list ([a-cell (in-vector block)]
                  #:unless (set-member? (signal-value (cell-cannot-be a-cell) #:pre 1)
                                        n))
         a-cell))
     (cond
       [(= (length the-remaining) 1)
        (match-define (cell _ _ must-be cannot-be) (car the-remaining))
        (sustain must-be n)]
       [else
        (pause)]))))

(define (cell-information cells size ht)
  (for*/hash ([x (in-range size)]
              [y (in-range size)])
    (match-define (cell _ _ must-be cannot-be) (hash-ref cells (cons x y)))
    (values (cons x y)
            (cons (hash-ref ht must-be #f)
                  (hash-ref ht cannot-be (set))))))

(define (show-puzzle sudoku-board)
  (define size (string-length (read-line (open-input-string sudoku-board))))
  (define cells (build-cells-table size))
  (define r
    (esterel
     #:pre 1
     (solve-sudoku sudoku-board size cells)))
  (define (step)
    (cell-information cells size (react! r)))
  (sudoku-gui size step))

(module+ main
  (show-puzzle medium-sudoku-board))

(module+ test

  (define (run-board sudoku-board)
    (define size (string-length (read-line (open-input-string sudoku-board))))
    (define cells (build-cells-table size))
    (define r
      (esterel
       #:pre 1
       (solve-sudoku sudoku-board size cells)))
    (define total-steps 100)
    (let loop ([i total-steps])
      (cond
        [(zero? i) (error 'run-board
                          "didn't solve board after ~a steps\n~a\n"
                          total-steps
                          sudoku-board)]
        [else
         (define ht (react! r))
         (define done?
           (for/and ([(k v) (in-hash (cell-information cells size ht))])
             (match-define (cons must-be cannot-be) v)
             must-be))
         (unless done?
           (loop (- i 1)))])))
  (run-board simplest-sudoku-board)

  (let ()
    (define size 4)
    (define cells (build-cells-table size))
    (define r
      (esterel
       #:pre 1
       (solve-sudoku 4x4-sudoku-board size cells)))
    (check-equal? (cell-information cells size (react! r))
                  (hash '(0 . 0) (cons 1 (set 2 3 4))
                        '(1 . 0) (cons #f (set 1 2))
                        '(0 . 1) (cons #f (set 1 3 4))
                        '(1 . 1) (cons #f (set 1 3))

                        '(2 . 0) (cons #f (set 1 2 3))
                        '(3 . 0) (cons 2 (set 1 3 4))
                        '(2 . 1) (cons 3 (set 1 2 4))
                        '(3 . 1) (cons #f (set 2 3))

                        '(0 . 2) (cons #f (set 1 4))
                        '(1 . 2) (cons 1 (set 2 3 4))
                        '(0 . 3) (cons 4 (set 1 2 3))
                        '(1 . 3) (cons #f (set 1 3 4))

                        '(2 . 2) (cons #f (set 1 3))
                        '(3 . 2) (cons #f (set 1 2 3))
                        '(2 . 3) (cons #f (set 3 4))
                        '(3 . 3) (cons 3 (set 1 2 4))))
    (check-equal? (cell-information cells size (react! r))
                  (hash '(0 . 0) (cons 1 (set 2 3 4))
                        '(1 . 0) (cons 3 (set 1 2 4))
                        '(0 . 1) (cons 2 (set 1 3 4))
                        '(1 . 1) (cons #f (set 1 2 3))

                        '(2 . 0) (cons 4 (set 1 2 3))
                        '(3 . 0) (cons 2 (set 1 3 4))
                        '(2 . 1) (cons 3 (set 1 2 4))
                        '(3 . 1) (cons 1 (set 2 3 4))

                        '(0 . 2) (cons 3 (set 1 2 4))
                        '(1 . 2) (cons 1 (set 2 3 4))
                        '(0 . 3) (cons 4 (set 1 2 3))
                        '(1 . 3) (cons 2 (set 1 3 4))

                        '(2 . 2) (cons #f (set 1 3 4))
                        '(3 . 2) (cons 4 (set 1 2 3))
                        '(2 . 3) (cons 1 (set 2 3 4))
                        '(3 . 3) (cons 3 (set 1 2 4))))
    (check-equal? (cell-information cells size (react! r))
                  (hash '(0 . 0) (cons 1 (set 2 3 4))
                        '(1 . 0) (cons 3 (set 1 2 4))
                        '(0 . 1) (cons 2 (set 1 3 4))
                        '(1 . 1) (cons 4 (set 1 2 3))

                        '(2 . 0) (cons 4 (set 1 2 3))
                        '(3 . 0) (cons 2 (set 1 3 4))
                        '(2 . 1) (cons 3 (set 1 2 4))
                        '(3 . 1) (cons 1 (set 2 3 4))

                        '(0 . 2) (cons 3 (set 1 2 4))
                        '(1 . 2) (cons 1 (set 2 3 4))
                        '(0 . 3) (cons 4 (set 1 2 3))
                        '(1 . 3) (cons 2 (set 1 3 4))

                        '(2 . 2) (cons 2 (set 1 3 4))
                        '(3 . 2) (cons 4 (set 1 2 3))
                        '(2 . 3) (cons 1 (set 2 3 4))
                        '(3 . 3) (cons 3 (set 1 2 4))))))
