#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (SO1 SO2)
     (define r
       (esterel
        (par
         (par
          (par
           (begin
             (pause)
             (if (present? SO1)
                 (void)
                 (begin (emit SO1)
                        (emit SO2))))
           (begin
             (pause)
             (if (present? SO2)
                 (void)
                 (begin (emit SO1)
                        (emit SO2)))))))))
     (react! r)
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (emit S)
            (void))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (void)
            (emit S))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (if (present? S)
            (emit S)
            (emit S))))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (define r
       (esterel
        (present? S)
        (emit S)))
     (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (S)
     (react!
      (esterel
       (with-signal (S2)
         (cond
           [(present? S)
            (emit S2)]
           [else
            (if (present? S2)
                (emit S)
                (void))])))))))

(let ()
  (define r
    (esterel
     (with-signal (s)
       (suspend
        (begin (pause)
               (emit s))
        (present? s)))))

  (react! r)
  (check-exn
   non-constructive-exn?
   (λ () (react! r))))

(check-exn
 non-constructive-exn?
 (λ ()
   (react!
    (esterel
     (with-signal (s #:init 0 #:combine +)
       (emit s 1)
       (let ([x (signal-value s #:can (set s))])
         (emit s x)))))))

(check-exn
 non-constructive-exn?
 (λ ()
   (react!
    (esterel
     (with-signal (s1 s2)
       (par
        (if (present? s1)
            (emit s2)
            (void))
        (if (present? s2)
            (void)
            (emit s1))))))))

;; the test cases in this begin are all from Dumitru Potop-Butucaru
(begin
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (with-signal (A B C)
         (par (if (present? A)
                  (begin (emit C) (emit B))
                  (emit C))
              (if (present? B)
                  (begin (emit C) (emit A))
                  (emit C))
              (if (present? C)
                  (begin (emit A) (emit B))
                  (void))))))))

  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (with-signal (U V S)
         (if (present? S)
             (par (emit V)
                  (if (present? U)
                      (emit S)
                      (void)))
             (par (emit U)
                  (if (present? V)
                      (emit S)
                      (void)))))))))

  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (with-signal (S U)
        (esterel
         (begin (if (present? S)
                    (emit U)
                    (emit U))
                (if (present? U)
                    (emit S)
                    (void)))))))))

(with-signal (S1 S2 O)
  (define r
    (esterel
     (if (present? S1)
         (if (present? S2)
             (void)
             (void))
         (emit O))))
  (check-equal? (react! r)
                (hash S1 #f S2 #f O #t)))

(with-signal (S1 S2 O)
  (define r
    (esterel
     (if (present? S1)
         (void)
         (if (present? S2)
             (void)
             (emit O)))))
  (check-equal? (react! r)
                (hash S1 #f S2 #f O #t)))

(with-signal (S1 S2 S3 S4 S5)
  (define r
    (esterel
     (if (present? S1)
         (void)
         (if (present? S2)
             (if (present? S3)
                 (void)
                 (if (present? S4)
                     (if (present? S5)
                         (void)
                         (void))
                     (void)))
             (void)))))

  (check-equal? (react! r)
                (hash S1 #f S2 #f S3 #f S4 #f S4 #f S5 #f)))

(with-signal (S1 S2)
  (check-equal?
   (react!
    (esterel
     (begin
       (if (present? S1)
           (void)
           (void))
       (if (present? S2)
           (emit S1)
           (void)))))
   (hash S1 #f S2 #f)))

(with-signal (I O1 O2 tmp)
  (check-equal?
   (react!
    (esterel
     (if (present? tmp)
         (emit O1)
         (emit O2))
     (if (present? I)
         (emit tmp)
         (void))))
   (hash O2 #t tmp #f I #f)))

(with-signal (I O1 O2 tmp)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (par
        (emit I)
        (begin
          (if (present? tmp)
              (emit O1)
              (emit O2))
          (if (present? I)
              (emit tmp)
              (void)))))))))

(with-signal (I O1 O2 tmp)
  (check-equal?
   (react!
    (esterel
     (if (present? I)
         (emit tmp)
         (void))
     (if (present? tmp)
         (emit O1)
         (emit O2))))
   (hash O2 #t tmp #f I #f)))

(with-signal (V_S_C V_S_i)
  (check-equal?
   (react!
    (esterel
     (if (present? V_S_C)
         (void)
         (void))
     (when (present? V_S_i)
       (emit V_S_C)
       (pause))))
   (hash V_S_C #f V_S_i #f)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (emit S1 #f)
     (pause)
     (if (signal-value S1 #:can (set O1 O2))
         (emit O1)
         (emit O2))))

  (react! r)
  (check-equal? (react! r)
                (hash O2 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (emit S1 #t)
     (pause)
     (if (signal-value S1 #:can (set O1 O2))
         (emit O1)
         (emit O2))))

  (react! r)
  (check-equal? (react! r)
                (hash O1 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (par (emit S1 3)
          (if (signal-value S1 #:can (set O1 O2))
              (emit O1)
              (emit O2)))))

  (check-equal? (react! r)
                (hash S1 3 O1 #t)))

(with-signal (S1 #:combine + O1 O2)
  (define r
    (esterel
     (par (emit S1 3)
          (emit S1 5)
          (if (signal-value S1 #:can (set O1 O2))
              (emit O1)
              (emit O2)))))

  (check-equal? (react! r)
                (hash S1 8 O1 #t)))


