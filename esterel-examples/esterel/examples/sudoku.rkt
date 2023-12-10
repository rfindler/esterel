#lang racket
(require esterel/full
         "private/sudoku-helpers.rkt")

#;
(define sudoku-board #<<--
1..2
..3.
.1..
4..3
--
  )

;; these three 9x9 boards here are courtesy of the Sudoku Exchange
;; Puzzle Bank, https://github.com/grantm/sudoku-exchange-puzzle-bank

#;
;; can be solved without `last-remaining`
(define sudoku-board #<<--
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
(define sudoku-board #<<--
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

#;
;; cannot be solved by code here
(define sudoku-board #<<--
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
     (match-define (cell my-x my-y my-must-be my-cannot-be) a-cell)
     (cross-out my-x my-y my-cannot-be (vector-ref cols my-x))
     (cross-out my-x my-y my-cannot-be (vector-ref rows my-y))
     (cross-out my-x my-y my-cannot-be (vector-ref squares (ij->square size my-x my-y)))
     (define my-n (signal-value my-must-be #:can (set my-cannot-be)))
     (when my-n
       (emit my-cannot-be (set-remove all-possible-ns my-n))))
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
