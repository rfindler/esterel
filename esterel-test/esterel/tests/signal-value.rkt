#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")

(with-signal (S #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S 1)))
   (hash S 1)))

(with-signal (S #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S 1)
     (emit S 2)))
   (hash S 3)))

(with-signal (S #:combine +)
  (check-exn
   #rx"signal has never been emitted"
   (λ ()
     (react!
      (esterel
       (signal-value S))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 11)
     (pause)
     (emit S2 (+ 1 (signal-value S1)))))
  (check-equal? (react! r) (hash S1 11))
  (check-equal? (react! r) (hash S2 12)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S1 1)
     (emit S1 2)
     (emit S2 (+ 2 (signal-value S1)))))
   (hash S1 3 S2 5)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (par (emit S1 1)
          (emit S2 (+ 1 (signal-value S1)))
          (emit S1 2))))
   (hash S1 3 S2 4)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (emit S1 1)
       (signal-value S1)
       (emit S1 2))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 44)
     (emit S2 55)
     (pause)
     (par (if (signal-value S1)
              (void)
              (emit S2 1))
          (if (signal-value S2)
              (void)
              (void)))))
  (react! r)
  (check-equal? (react! r) (hash)))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 #f)
     (emit S2 55)
     (pause)
     (par (if (signal-value S1)
              (void)
              (emit S2 1))
          (if (signal-value S2)
              (void)
              (void)))))
  (react! r)
  (check-equal? (react! r) (hash S2 1)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 1)
     (pause)
     (emit S 2)))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2)))

(with-signal (S #:combine + O)
  (define r
    (esterel
     #:pre 2
     (emit S 1)
     (pause)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O S #:combine +)
  (define r
    (esterel
     #:pre 2
     (emit S 1)
     (pause)
     (emit S 2)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))
     (pause)
     (when (= 1 (signal-value S #:pre 2))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2 O #t))
  (check-equal? (react! r) (hash O #t)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r) (hash)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 0)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (emit S 0)
     (present? S)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:combine +)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r #:emit (list (cons S 0)))
                (hash S 0)))

(with-signal (S #:combine + O1 O2)
  (define r
    (esterel
     (if (present? S)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r #:emit (list (cons S 2)))
                (hash S 2 O1 #t)))

(with-signal (S #:combine + O)
  (define r
    (esterel
     (let loop ()
       (when (and (present? S) (= (signal-value S) 2))
         (emit O))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r #:emit (list (cons S 1))) (hash S 1))
  (check-equal? (react! r #:emit (list (cons S 2))) (hash S 2 O #t)))

(with-signal (S #:init 0 #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (pause)
       (emit S (+ 1 (signal-value S #:pre 1)))
       (loop))))
  (react! r)
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2))
  (check-equal? (react! r) (hash S 3)))

(with-signal (S  #:init 0 #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (emit S (+ 1 (signal-value S #:pre 1)))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2))
  (check-equal? (react! r) (hash S 3)))

(with-signal (S #:init 0 #:combine + T #:combine +)
  (define r
    (esterel
     #:pre 1
     (let loop ()
       (emit T (signal-value S))
       (pause)
       (emit S (+ 1 (signal-value S #:pre 1)))
       (loop))))
  (check-equal? (react! r) (hash T 0))
  (check-equal? (react! r) (hash T 1 S 1))
  (check-equal? (react! r) (hash T 2 S 2)))

(with-signal (S #:init (set) #:combine set-union)
  (define r
    (esterel
     (let loop ([n 0])
       (for ([i (in-range n)]) (emit S (set i)))
       (pause)
       (loop (+ n 1)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash S (set 0)))
  (check-equal? (react! r) (hash S (set 0 1))))


(with-signal (S #:init 0 #:combine + T U)
  (check-equal?
   (react!
    (esterel
     (par (if (present? S)
              (emit U)
              (void))
          (if (present? T)
              (void)
              (emit S 1))
          (if (present? U)
              (void)
              (void)))))
   (hash T #f S 1 U #t)))

