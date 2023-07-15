#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")

(check-equal?
 (react! (esterel
          (par
           (pause)
           (void))))
 (hash))

(check-equal?
 (react! (esterel
          (par
           (pause)
           (pause))))
 (hash))

(with-signal (s)
  (check-equal?
   (react! (esterel
            (par
             (pause)
             (emit s))))
   (hash s #t)))

(check-equal?
 (react! (esterel
          (par
           (par
            (pause)
            (pause))
           (par
            (pause)
            (pause)))))
 (hash))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (par
       (present? s)
       (pause))
      (par
       (pause)
       (present? s)))))
   (hash s #f)))

(check-equal?
 (react! (esterel
          (par
           (par
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))
            (pause))
           (par
            (pause)
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))))))
 (hash))

(with-signal (O1 O2)
  (check-equal?
   (react! (esterel
            (if (equal? (set 1 2) (par 1 2))
                (emit O1)
                (emit O2))))
   (hash O1 #t)))

(with-signal (s1 s2)
  (define r
    (esterel
     (par
      (let loop ()
        (suspend (begin (pause) (emit s2))
                 (present? s1))
        (loop))
      (begin (emit s1) (pause) (emit s1)))))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t
       (emit s1)
       (exit-trap t)
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t
       (par
        (emit s1)
        (exit-trap t))
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t1
       (with-trap t2
         (par
          (exit-trap t1)
          (exit-trap t2))
         (emit s1))
       (emit s2))))
   (hash)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (with-trap t1
       (with-trap t2
         (with-trap t3
           (with-trap t4
             (par
              (par
               (exit-trap t1)
               (exit-trap t4))
              (par
               (exit-trap t2)
               (emit s1))))))
       (emit s2))))
   (hash s1 #t)))

(with-signal (s1 s2)
  (define r
    (esterel
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))

(with-signal (s1 s2 s3)
  (define r
    (esterel
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))
     (pause)
     (emit s3)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash s3 #t)))

(let ()
  (define r
    (esterel
     (with-trap T1
       (par
        (exit-trap T1)))
     (par
      (pause)
      (pause))))

  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))


(with-signal (i o)
  (define r
    (esterel
     (let loop ()
       (when (present? i)
         (emit o))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t))
  (check-equal? (react! r) (hash i #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (pause)
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause)
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define t
    (esterel
     (with-trap T
       (par
        (par
         (pause)
         (void))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (par
      (with-trap T
        (par
         (par
          (pause))
         (exit-trap T)))
      (pause))
     (emit O)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (par
      (with-trap T1
        (par
         (par
          (pause))
         (exit-trap T1)))
      (pause)
      (with-trap T2
        (par
         (par
          (pause))
         (exit-trap T2))))
     (emit O)))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (pause))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (pause))
           (exit-trap T2))))
       (emit O))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))

(with-signal (O)
  (define r
    (esterel
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (pause))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (pause))
           (exit-trap T2)))))
     (emit O)))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the following tests add a bunch of calls to `(present? can-explore)`
;; in order to test can mode in the same situations as above
(define-signal can-explore)

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (pause)
           (void))))
 (hash can-explore #f))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (pause)
           (pause))))
 (hash can-explore #f))

(with-signal (s)
  (check-equal?
   (react! (esterel
            (present? can-explore)
            (par
             (pause)
             (emit s))))
   (hash s #t can-explore #f)))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (par
            (pause)
            (pause))
           (par
            (pause)
            (pause)))))
 (hash can-explore #f))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (par
      (par
       (present? s)
       (pause))
      (par
       (pause)
       (present? s)))))
   (hash s #f can-explore #f)))

(check-equal?
 (react! (esterel
          (present? can-explore)
          (par
           (par
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))
            (pause))
           (par
            (pause)
            (par
             (par
              (pause)
              (pause))
             (par
              (pause)
              (pause)))))))
 (hash can-explore #f))

(with-signal (s1 s2)
  (define r
    (esterel
     (present? can-explore)
     (par
      (let loop ()
        (suspend (begin (pause) (present? can-explore) (emit s2))
                 (present? s1))
        (loop))
      (begin (emit s1) (pause) (emit s1)))))
  (check-equal? (react! r) (hash s1 #t can-explore #f))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t can-explore #f))
  (check-equal? (react! r) (hash s1 #f s2 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t
       (emit s1)
       (exit-trap t)
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t
       (par
        (emit s1)
        (exit-trap t))
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t1
       (with-trap t2
         (par
          (exit-trap t1)
          (exit-trap t2))
         (emit s1))
       (emit s2))))
   (hash can-explore #f)))

(with-signal (s1 s2)
  (check-equal?
   (react!
    (esterel
     (present? can-explore)
     (with-trap t1
       (with-trap t2
         (with-trap t3
           (with-trap t4
             (par
              (par
               (exit-trap t1)
               (exit-trap t4))
              (par
               (exit-trap t2)
               (emit s1))))))
       (emit s2))))
   (hash s1 #t can-explore #f)))

(with-signal (s1 s2)
  (define r
    (esterel
     (present? can-explore)
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))

(with-signal (s1 s2 s3)
  (define r
    (esterel
     (present? can-explore)
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))
     (pause)
     (emit s3)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash s3 #t)))

(let ()
  (define r
    (esterel
     (with-trap T1
       (par
        (exit-trap T1)
        (present? can-explore)))
     (par
      (pause)
      (pause))))

  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))


(with-signal (i o)
  (define r
    (esterel
     (let loop ()
       (present? can-explore)
       (when (present? i)
         (emit o))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f))
  (check-equal? (react! r) (hash i #f can-explore #f))
  (check-equal? (react! r #:emit (list i)) (hash i #t o #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (pause)
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause)
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define t
    (esterel
     (present? can-explore)
     (with-trap T
       (par
        (par
         (pause)
         (void))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t can-explore #f)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (par
      (with-trap T
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T)))
      (pause))
     (emit O)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (par
      (with-trap T1
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T1)))
      (pause)
      (with-trap T2
        (par
         (par
          (begin (pause) (present? can-explore)))
         (exit-trap T2))))
     (emit O)))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash O #t)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T2))))
       (emit O))))
  (check-equal? (react! r) (hash can-explore #f))
  (check-equal? (react! r) (hash)))

(with-signal (O)
  (define r
    (esterel
     (present? can-explore)
     (with-trap T0
       (par
        (with-trap T1
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T1)))
        (exit-trap T0)
        (with-trap T2
          (par
           (par
            (begin (pause) (present? can-explore)))
           (exit-trap T2)))))
     (emit O)))
  (check-equal? (react! r) (hash O #t can-explore #f))
  (check-equal? (react! r) (hash)))
