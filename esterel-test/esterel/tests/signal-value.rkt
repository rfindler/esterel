#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")

(printf "1\n")
(with-signal (S #:value)
  (check-equal?
   (react!
    (esterel
     (emit S 1)))
   (hash S 1)))
(printf "2\n")
(with-signal (S #:value)
  (check-exn
   #rx"emit: different signal values"
   (λ ()
     (react!
      (esterel
       (emit S 1)
       (emit S 2))))))

(with-signal (S #:value)
  (check-exn
   #rx"signal-value: signal is not emitted in this instant"
   (λ ()
     (react!
      (esterel
       (signal-value S))))))

(with-signal (S1 #:value S2 #:value)
  (define r
    (esterel
     (emit S1 11)
     (emit S2 (+ 1 (signal-value S1)))))
  (check-equal? (react! r) (hash S1 11 S2 12)))

(with-signal (S1 #:value S2 #:value)
  (check-equal?
   (react!
    (esterel
     (par (emit S1 1)
          (emit S2 (+ 1 (signal-value S1)))
          (emit S1 1))))
   (hash S1 1 S2 2)))

(with-signal (S #:value)
  (define r
    (esterel
     (emit S 1)
     (pause)
     (emit S 2)))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2)))

(with-signal (S #:value O)
  (define r
    (esterel
     #:pre 2
     (emit S 1)
     (pause)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O S #:value)
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

(with-signal (S #:value)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r) (hash)))

(with-signal (S #:value)
  (define r
    (esterel
     (emit S 0)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:value)
  (define r
    (esterel
     (emit S 0)
     (present? S)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(with-signal (S #:value)
  (define r
    (esterel
     (present? S)))
  (check-equal? (react! r #:emit (list (cons S 0)))
                (hash S 0)))

(with-signal (S #:value O1 O2)
  (define r
    (esterel
     (if (present? S)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r #:emit (list (cons S 2)))
                (hash S 2 O1 #t)))

(with-signal (S #:value O)
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

(with-signal (S #:init 0 #:value)
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

(with-signal (S  #:init 0 #:value)
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

(with-signal (S #:init 0 #:value T U)
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

