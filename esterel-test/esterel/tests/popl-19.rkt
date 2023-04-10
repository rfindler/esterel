#lang racket
(require esterel/kernel rackunit
         "private/util.rkt")

;; these are the programs from the figures in the POPL'19 paper:
;; A Calculus for Esterel: If can, can. If no can, no can.

;; figure 2
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (emit sl)
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #t so1 #t)))

;; figure 3
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (par
      (emit sl)
      (if (present? sl)
          (emit so1)
          (emit so2)))))
   (hash sl #t so1 #t)))


;; figure 4
(with-signal (sl so1 so2)
  (check-equal?
   (react!
    (esterel
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #f so2 #t)))

;; figure 5
(with-signal (sl1 sl2)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (par
        (when (present? sl1) (emit sl2))
        (when (present? sl2) (emit sl1))))))))


;; figure 6
(with-signal (sl so1 so2)
  (define r
    (esterel
     (par
      (begin (pause) (emit sl))
      (if (present? sl) (emit so1) (emit so2)))))
  (check-equal?
   (react! r)
   (hash sl #f so2 #t))
  (check-equal?
   (react! r)
   (hash sl #t)))

;; figure 7
(with-signal (sl1 sl2 sl3 so1 so2)
  (define r
    (esterel
     (par
      (if (present? sl1)
          (if (present? sl2)
              (emit so1)
              (emit sl3))
          (if (present? sl2)
              (emit so2)
              (emit sl3)))
      (begin
        (emit sl2)
        (when (present? sl3) (pause))
        (emit sl1)))))
  (check-equal?
   (react! r)
   (hash sl1 #t sl2 #t sl3 #f so1 #t)))

;; figure 8
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1)
           (void)
           (emit s1)))))))

;; figure 9
(with-signal (s1)
  (check-exn
   non-constructive-exn?
   (λ ()
     (react!
      (esterel
       (if (present? s1)
           (emit s1)
           (void)))))))


;; figure 10 example 1
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1)
           (emit s1)
           (emit s1)))))))

;; figure 10 example 2
(check-exn
 non-constructive-exn?
 (λ ()
   (with-signal (s1)
     (react!
      (esterel
       (if (present? s1) (void) (void))
       (emit s1))))))

;; figure 11
(check-exn
 exn:fail:not-constructive?
 (λ ()
   (with-signal (sl1 sl2)
     (react!
      (esterel
       (par
        (when (present? sl1) (emit sl2))
        (begin (when (present? sl2) pause)
               (emit sl1))))))))

;; figure 25
(with-signal (s-outer s-inner)
  (check-equal?
   (react!
    (esterel
     (begin
       (present? s-outer)
       (when (present? s-inner) (emit s-outer)))))
   (hash s-outer #f s-inner #f)))
