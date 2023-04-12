#lang racket
(require esterel/kernel rackunit "private/util.rkt")

(check-equal? (with-signal (S) (signal-name S)) "S")
(check-equal? (with-signal (S) (signal-index S)) #f)
(check-equal?
 (signal-index (car (hash-keys (react! (esterel (with-signal (S) (emit S)))))))
 0)
(check-equal?
 (for/set ([(k v) (in-hash
                   (react! (esterel
                            (define (f)
                              (with-signal (S) (emit S)))
                            (f) (f) (f))))])
   (signal-index k))
 (set 0 1 2))

(check-equal?
 (react!
  (esterel
   (void)))
 (hash))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (present? s)))
   (hash s #f)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (emit s)
     (present? s)))
   (hash s #t)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (begin (emit s) #f)
      (present? s))))
   (hash s #t)))

(with-signal (s)
  (check-equal?
   (react!
    (esterel
     (par
      (present? s)
      (present? s))))
   (hash s #f)))

(check-equal?
 (react!
  (esterel
   (pause)))
 (hash))

(with-signal (s)
  (define r
    (esterel
     (pause)
     (emit s)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t)))

(with-signal (s)
  (define r
    (esterel
     (pause)
     (emit s)
     (pause)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t))
  (check-equal?
   (react! r)
   (hash)))

(with-signal (s)
  (define r
    (esterel
     (present? s)
     (pause)
     (emit s)
     (pause)
     (present? s)))
  (check-equal?
   (react! r)
   (hash s #f))
  (check-equal?
   (react! r)
   (hash s #t))
  (check-equal?
   (react! r)
   (hash s #f)))

;; test that suspension suspends signals
(with-signal (S2
              O #:init (set) #:combine set-union)
  (define r
    (esterel
     #:pre 1
     (suspend
      (with-signal (S1)
        (let loop ()
          (emit S1)
          (pause)
          (emit O (set (present? S1 #:pre 1)))
          (loop)))
      (present? S2))))
  (define (react!/names r #:emit [emitters '()])
    (for/hash ([(k v) (in-hash (react! r #:emit emitters))])
      (values (signal-name k) v)))
  (check-equal? (react!/names r) (hash "S1" #t))
  (check-equal? (react!/names r #:emit (list S2)) (hash "S2" #t))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r #:emit (list S2)) (hash "S2" #t))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f))
  (check-equal? (react!/names r) (hash "S1" #t "O" (set #t) "S2" #f)))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (emit S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(emit: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (present? S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(present[?]: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1 #:combine +)
             (emit C S1)
             (let loop ()
               (emit S1 0)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S2)
            (sleep .01)
            (signal-value S1)))))
  (react! r)
  ;; this test will usually be the emit error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(signal-value: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))

(with-signal (C #:combine (λ (x y) x) S2)
  (define r
    (esterel
     #:pre 1
     (par (suspend
           (with-signal (S1)
             (emit C S1)
             (let loop ()
               (emit S1)
               (pause)
               (loop)))
           (present? S2))
          (let ([S1 (signal-value C)])
            (pause)
            (emit S1)
            (sleep .01)
            (emit S2)))))
  (react! r)
  ;; this test will usually be the suspend error but it isn't guaranteed;
  ;; one of these errors should be guaranteed, however.
  (check-exn #rx"(emit: signal is suspended)|(suspend: suspended signal was used)"
             (λ () (react! r))))
