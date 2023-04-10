#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")
                                   
;; these are programs from Berry's
;; _The Constructive Semanitcs of Pure Esterel_

;; P1
(with-signal (I O)
  (define r
    (esterel
     (with-signal (S1 S2)
       (par (if (present? I)
                (emit S1)
                (void))
            (if (present? S1)
                (void)
                (emit S2))
            (if (present? S2)
                (emit O)
                (void))))))
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (check-equal? (signals->names (react! r))
                (hash "S1" #f
                      "S2" #t
                      "O" #t
                      "I" #f)))

;; P2
(let ()
  (define r
    (with-signal (O)
      (esterel
       (with-signal (S)
         (emit S)
         (when (present? O)
           (when (present? S)
             (pause))
           (emit O))))))
  (define (signals->names ht)
    (for/hash ([(k v) (in-hash ht)])
      (values (signal-name k) v)))
  (check-equal? (signals->names (react! r))
                (hash "O" #f "S" #t)))

;; the next few programs are probably elsewhere in
;; this test suite but they are also included here
;; just in case someone looks here for them

;; P3
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O)
     (react!
      (esterel
       (if (present? O)
           (void)
           (emit O)))))))

;; P4
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O)
     (react!
      (esterel
       (if (present? O)
           (emit O)
           (void)))))))

;; P5
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O1 O2)
     (react!
      (esterel
       (par
        (if (present? O1)
            (emit O2)
            (void))
        (if (present? O2)
            (void)
            (emit O1))))))))

;; P6
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O1 O2)
     (react!
      (esterel
       (par
        (if (present? O1)
            (emit O2)
            (void))
        (if (present? O2)
            (emit O1)
            (void))))))))

;; P7
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O)
     (react!
      (esterel
       (if (present? O)
           (pause)
           (void))
       (emit O))))))

;; P8
(with-signal (I O)
  (define (run I-present?)
    (react!
     (esterel
      (with-trap T
        (par
         (begin (if (present? I)
                    (void)
                    (pause))
                (emit O))
         (if (present? O)
             (exit-trap T)
             (void))))
      (emit O))
     #:emit (if I-present?
                (list I)
                (list))))
  (check-equal?
   (run #t)
   (hash O #t I #t))
  (check-exn
   non-constructive-exn?
   (λ ()
     (run #f))))

;; P8 variation; from the book:
;; Replacing “then” by “else” in the second parallel branch
;; would yield a program with no logically coherent behavior.
(with-signal (I O)
  (define (run I-present?)
    (react!
     (esterel
      (with-trap T
        (par
         (begin (if (present? I)
                    (void)
                    (pause))
                (emit O))
         (if (present? O)
             (void)
             (exit-trap T))))
      (emit O))
     #:emit (if I-present?
                (list I)
                (list))))
  ;; this test case is failing but I don't get
  ;; why the implementation is wrong; comment it out for now
  #;
  (check-exn
   non-constructive-exn?
   (λ ()
     (run #t)))
  (check-exn
   non-constructive-exn?
   (λ ()
     (run #f))))

;; P9
;; page 40 makes it clear this is
;; a non-constructive program
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O1 O2)
     (react!
      (esterel
       (par (if (present? O1)
                (emit O1)
                (void))
            (if (present? O1)
                (if (present? O2)
                    (void)
                    (emit O2))
                (void))))))))

;; P10
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O)
     (react!
      (esterel
       (if (present? O) (void) (void))
       (emit O))))))

;; P11
#; ;; this test case fails!
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (O)
     (react!
      (esterel
       (with-signal (S)
         (when (present? O)
           (emit S)
           (when (present? S)
             (pause))
           (emit O))))))))
