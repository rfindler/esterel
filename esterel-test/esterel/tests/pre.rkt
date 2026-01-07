#lang racket
(require esterel/kernel esterel/full rackunit)

(with-signal (S O)
  (define r
    (esterel
     #:pre 2
     (emit S)
     (pause)
     (when (present? S #:pre 1)
       (emit O))))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t)))

(with-signal (S O1 O2)
  (define r
    (esterel
     #:pre 2
     (if (present? S #:pre 1)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r) (hash O2 #t)))

(with-signal (O)
  (define r
    (esterel
     #:pre 1
     (pause)
     (pause)
     (present? O #:pre 2)))
  (react! r)
  (react! r)
  (check-exn
   #rx"present[?]: #:pre argument too large.*maximum: 1"
   (λ () (react! r))))

(with-signal (S O)
  (define r
    (esterel
     #:pre 1
     (pause)
     (par
      (let loop () (emit S) (pause) (pause) (loop))
      (let loop ()
        (when (present? S #:pre 1)
          (emit O))
        (pause)
        (loop)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t)))

(let ()
  (define r
    (esterel
     #:pre 1
     (loop
      (with-signal (S S-pre T)
        (pause)
        (if (present? S #:pre 1)
            (emit T)
            (emit S))))))
  (define (get-emitted-names ht)
    (for/set ([(k v) (in-hash ht)]
              #:when v)
      (signal-name k)))
  (check-equal? (get-emitted-names (react! r)) (set))
  (check-equal? (get-emitted-names (react! r)) (set "S"))
  (check-equal? (get-emitted-names (react! r)) (set "S"))
  (check-equal? (get-emitted-names (react! r)) (set "S"))
  )

;; from hiphop's test suite: reincar8
(with-signal (pre/0- no-pre/0- emit/1- pre/1-)
  (define r
    (esterel
     #:pre 1
     (loop
      (with-signal (L)
        (cond
          [(present? L #:pre 1)
           (emit pre/0-)
           (pause)]
          [else
           (emit no-pre/0-)
           (pause)
           (emit emit/1-)
           (emit L)
           (pause)
           (when (present? L #:pre 1)
             (emit pre/1-))])))))
  (define (get-emitted-names ht)
    (for/set ([(k v) (in-hash ht)]
              #:when v)
      (signal-name k)))
  (check-equal? (get-emitted-names (react! r)) (set "no-pre/0-"))
  (check-equal? (get-emitted-names (react! r)) (set "emit/1-" "L"))
  (check-equal? (get-emitted-names (react! r)) (set "pre/1-" "no-pre/0-")))

;; from hiphop's test suite: reincar10
(with-signal (S T)
  (define r
    (esterel
     #:pre 1
     (par
      (loop
       (pause)
       (when (present? S #:pre 1)
         (emit T))))))

  (check-equal? (react! r #:emit (list S)) (hash S #t))
  (check-equal? (react! r) (hash T #t))
  (check-equal? (react! r) (hash)))

;; from hiphop's test suite: reincar12
(with-signal (g1092 out)
  (define r
    (esterel
     #:pre 1
     (with-signal (g1095)
       (loop
        (when (present? g1092 #:pre 1)
          (emit out))
        (pause)))))

  (check-equal? (react! r #:emit (list g1092)) (hash g1092 #t))
  (check-equal? (react! r) (hash out #t)))

;; from hiphop's test suite: reincar13
(let ()
  (define r
    (esterel
     #:pre 1
     (loop
      (with-signal (S tt ff)
        (pause)
        (cond
          [(present? S #:pre 1)
           (emit tt)]
          [else
           (emit ff)
           (emit S)])))))
  (define (get-emitted-names ht)
    (for/set ([(k v) (in-hash ht)]
              #:when v)
      (signal-name k)))
  (check-equal? (get-emitted-names (react! r)) (set))
  (check-equal? (get-emitted-names (react! r)) (set "S" "ff"))
  (check-equal? (get-emitted-names (react! r)) (set "S" "ff")))

;; from hiphop's test suite: reincar15
(with-signal (tt ff)
  (define r
    (esterel
     #:pre 1
     (loop
      (with-signal (S)
        (pause)
        (cond
          [(present? S #:pre 1)
           (emit tt)]
          [else
           (emit ff)
           (emit S)])
        (pause)

        (cond
          [(present? S #:pre 1)
           (emit tt)]
          [else
           (emit ff)
           (emit S)])))))
  (define (get-emitted-names ht)
    (for/set ([(k v) (in-hash ht)]
              #:when v)
      (signal-name k)))
  (check-equal? (get-emitted-names (react! r)) (set))
  (check-equal? (get-emitted-names (react! r)) (set "ff" "S"))
  (check-equal? (get-emitted-names (react! r)) (set "tt"))
  (check-equal? (get-emitted-names (react! r)) (set "ff" "S"))
  (check-equal? (get-emitted-names (react! r)) (set "tt")))


;; test that with-signal doesn't break up
;; its but keeps everything in a single scope
(check-equal? (with-signal (S)
                (define (g) (f))
                (begin
                  (define (f) 1)
                  (g)))
              1)

