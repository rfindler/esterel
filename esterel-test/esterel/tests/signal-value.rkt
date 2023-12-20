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

(with-signal (T #:combine (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y))))
              S #:combine (λ (x y) (string->symbol (string-append (symbol->string x) (symbol->string y)))))
  (check-equal?
   (react!
    (esterel
     (emit S 'suspended)
     (emit T (signal-value S #:can (set)))))
   (hash S 'suspended T 'suspended)))

(with-signal (S #:combine +)
  (check-exn
   #rx"signal has never been emitted"
   (λ ()
     (react!
      (esterel
       (signal-value S #:can (set)))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 11)
     (pause)
     (emit S2 (+ 1 (signal-value S1 #:can (set))))))
  (check-equal? (react! r) (hash S1 11))
  (check-equal? (react! r) (hash S2 12)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (emit S1 1)
     (emit S1 2)
     (emit S2 (+ 2 (signal-value S1 #:can (set))))))
   (hash S1 3 S2 5)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-equal?
   (react!
    (esterel
     (par (emit S1 1)
          (emit S2 (+ 1 (signal-value S1 #:can (set))))
          (emit S1 2))))
   (hash S1 3 S2 4)))

(with-signal (S1 #:combine + S2 #:combine +)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (emit S1 1)
       (signal-value S1 #:can (set S1))
       (emit S1 2))))))

(with-signal (S1 #:combine + S2 #:combine +)
  (define r
    (esterel
     (emit S1 44)
     (emit S2 55)
     (pause)
     (par (if (signal-value S1 #:can (set S2))
              (void)
              (emit S2 1))
          (if (signal-value S2 #:can (set))
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
     (par (if (signal-value S1 #:can (set S2))
              (void)
              (emit S2 1))
          (if (signal-value S2 #:can (set))
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
       (when (and (present? S) (= (signal-value S #:can (set O)) 2))
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
       (emit T (signal-value S #:can (set)))
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

(with-signal (S #:single)
  (define r
    (esterel
     (emit S 1)
     (pause)
     (emit S 2)))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2)))

(with-signal (S #:single)
  (define r
    (esterel
     (emit S 1)
     (emit S 2)))
  (check-exn
   #rx"signal-value: multiple emission of a single signal\n  signal: #<signal: S>\n  value: 1\n  value: 2"
   (λ ()
     (react! r))))

(with-signal (O1
              O2
              S #:memoryless #:init 0 #:combine +)
  (define r
    (esterel
     (emit S 1)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))
     (pause)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))
     (pause)
     (emit S 2)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))))
  (check-equal? (react! r) (hash S 1 O2 #t))
  (check-equal? (react! r) (hash O1 #t))
  (check-equal? (react! r) (hash S 2 O2 #t)))


(let ()
  (define O1 (make-global-signal "O1"))
  (define O2 (make-global-signal "O2"))
  (define S (make-global-signal "S" #:memoryless? #t #:init 0 #:combine +))
  (define r
    (esterel
     (emit S 1)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))
     (pause)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))
     (pause)
     (emit S 2)
     (if (= (signal-value S #:can (set O1 O2)) 0) (emit O1) (emit O2))))
  (check-equal? (react! r) (hash S 1 O2 #t))
  (check-equal? (react! r) (hash O1 #t))
  (check-equal? (react! r) (hash S 2 O2 #t)))
