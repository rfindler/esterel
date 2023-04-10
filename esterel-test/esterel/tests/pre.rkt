#lang racket
(require esterel/kernel rackunit)

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


;; test that with-signal doesn't break up
;; its but keeps everything in a single scope
(check-equal? (with-signal (S)
                (define (g) (f))
                (begin
                  (define (f) 1)
                  (g)))
              1)

