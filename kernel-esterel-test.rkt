#lang racket
(require "kernel-esterel.rkt" rackunit)

(check-equal?
 (react!
  (reaction
   (void)))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (present? s)))
   (hash s #f)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (emit s)
     (present? s)))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (begin (emit s) #f)
      (present? s))))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (present? s)
      (present? s))))
   (hash s #f)))

(check-equal?
 (react!
  (reaction
   (pause)))
 (hash))

(let ([s (signal)])
  (define r
    (reaction
     (pause)
     (emit s)))
  (check-equal?
   (react! r)
   (hash))
  (check-equal?
   (react! r)
   (hash s #t)))

(let ([s (signal)])
  (define r
    (reaction
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

(let ([s (signal)])
  (define r
    (reaction
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


(check-equal?
 (react! (reaction
          (par
           (pause)
           (void))))
 (hash))

(check-equal?
 (react! (reaction
          (par
           (pause)
           (pause))))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react! (reaction
            (par
             (pause)
             (emit s))))
   (hash s #t)))

(check-equal?
 (react! (reaction
          (par
           (par
            (pause)
            (pause))
           (par
            (pause)
            (pause)))))
 (hash))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (par
       (present? s)
       (pause))
      (par
       (pause)
       (present? s)))))
   (hash s #f)))

(check-equal?
 (react! (reaction
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

(let ()
  (define s1 (signal))
  (define s2 (signal))
  (define r
    (reaction
     (par
      (let loop () (suspend (begin (pause) (emit s2)) (present? s1)) (loop))
      (begin (emit s1) (pause) (emit s1)))))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t))
  (check-equal? (react! r) (hash s1 #f s2 #t)))

(let ([s1 (signal)]
      [s2 (signal)])
  (check-equal?
   (react!
    (reaction
     (with-trap t
       (emit s1)
       (exit-trap t)
       (emit s2))))
   (hash s1 #t)))

(let ([s1 (signal)]
      [s2 (signal)])
  (check-equal?
   (react!
    (reaction
     (with-trap t
       (par
        (emit s1)
        (exit-trap t))
       (emit s2))))
   (hash s1 #t)))

(let ([s1 (signal)]
      [s2 (signal)])
  (check-equal?
   (react!
    (reaction
     (with-trap t1
       (with-trap t2
         (par
          (exit-trap t1)
          (exit-trap t2))
         (emit s1))
       (emit s2))))
   (hash)))

(let ([s1 (signal)]
      [s2 (signal)])
  (check-equal?
   (react!
    (reaction
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

(let ([s1 (signal)]
      [s2 (signal)])
  (define r
    (reaction
     (with-trap t1
       (par
        (begin (pause) (emit s1))
        (exit-trap t1)))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))

(let ([s1 (signal)]
      [s2 (signal)]
      [s3 (signal)])
  (define r
    (reaction
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
    (reaction
     (with-trap T1
       (par
        (exit-trap T1)))
     (par
      (pause)
      (pause))))

  (check-equal? (react! r) (hash))
  (check-equal? (react! r) (hash)))


(let ()
  (define i (signal))
  (define o (signal))
  (define r
    (reaction
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

(let ()
  (define O (signal))
  (define t
    (reaction
     (with-trap T
       (par
        (pause)
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(let ()
  (define O (signal))
  (define t
    (reaction
     (with-trap T
       (par
        (par
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(let ()
  (define O (signal))
  (define t
    (reaction
     (with-trap T
       (par
        (par
         (pause)
         (pause))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(let ()
  (define O (signal))
  (define t
    (reaction
     (with-trap T
       (par
        (par
         (pause)
         (void))
        (exit-trap T)))
     (emit O)))

  (check-equal? (react! t) (hash O #t)))

(let ()
  (define O (signal))
  (define r
    (reaction
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

(let ()
  (define O (signal))
  (define r
    (reaction
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

(let ()
  (define O (signal))
  (define r
    (reaction
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

(let ()
  (define O (signal))
  (define r
    (reaction
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

;; exception raising
(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (reaction
     (car #f)))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (reaction
     (par
      (car #f))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (reaction
     (par
      (pause)
      (car #f))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (reaction
     (with-trap T
       (par
        (exit-trap T)
        (car #f)))))))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   (react!
    (reaction
     (with-trap T
       (par
        (par
         (exit-trap T))
        (par
         (car #f))))))))

(let ()
  (define S (signal))
  (define r
    (reaction
     (with-trap T
       (par
        (par
         (pause) (emit S))
        (par
         (car #f))))))
  (check-exn
   #rx"expected: pair[?].* given: #f"
   (λ () (react! r)))
  (check-exn
   #rx"expected: pair[?].* given: #f"
   (λ () (react! r))))

(let ()
  (define s1 (signal))
  (define s2 (signal))
  (check-equal?
   (react!
    (reaction
     (par
      (if (present? s1)
          (void)
          (car #f))
      (if (present? s2)
          (void)
          (emit s1)))))
   (hash s1 #t s2 #f)))

(let ()
  (define s1 (signal))
  (define s2 (signal))
  (check-equal?
   (react!
    (reaction
     (par
      (if (present? s2)
          (void)
          (emit s1))
      (if (present? s1)
          (void)
          (car #f)))))
   (hash s1 #t s2 #f)))

(let ()
  (define s1 (signal))
  (define s2 (signal))
  (check-equal?
   (react!
    (reaction
     (par
      (if (present? s1)
          (void)
          (emit s2))
      (if (present? s2)
          (void)
          (car #f)))))
   (hash s1 #f s2 #t)))

(check-exn
 #rx"expected: pair[?].* given: #f"
 (λ ()
   ;; raising an exception also counts as an incorrect
   ;; choice for the signal value so this program is
   ;; non constructive, by that logic but we raise one
   ;; of the exceptions as that seems more helpful
   (define s1 (signal))
   (define s2 (signal))
   (react!
    (reaction
     (par
      (if (present? s1)
          (void)
          (car #f))
      (if (present? s2)
          (void)
          (car #f)))))))

;; #:pre

(let ()
  (define S (signal))
  (define O (signal))
  (define r
    (reaction
     #:pre 2
     (emit S)
     (pause)
     (when (present? S #:pre 1)
       (emit O))))
  (check-equal? (react! r) (hash S #t))
  (check-equal? (react! r) (hash O #t)))

(let ()
  (define O1 (signal))
  (define O2 (signal))
  (define S (signal))
  (define r
    (reaction
     #:pre 2
     (if (present? S #:pre 1)
         (emit O1)
         (emit O2))))
  (check-equal? (react! r) (hash O2 #t)))

(let ()
  (define O (signal))
  (define r
    (reaction
     #:pre 1
     (pause)
     (pause)
     (present? O #:pre 2)))
  (react! r)
  (react! r)
  (check-exn
   #rx"present[?]: #:pre argument too large.*maximum: 1"
   (λ () (react! r))))

(let ()
  (define S (signal))
  (define O (signal))
  (define r
    (reaction
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

;; signals with values

(let ()
  (define S (signal #:combine +))
  (check-equal?
   (react!
    (reaction
     (emit S 1)))
   (hash S 1)))

(let ()
  (define S (signal #:combine +))
  (check-equal?
   (react!
    (reaction
     (emit S 1)
     (emit S 2)))
   (hash S 3)))

(let ()
  (define S1 (signal #:combine +))
  (define S2 (signal #:combine +))
  (check-equal?
   (react!
    (reaction
     (emit S1 1)
     (emit S1 2)
     (emit S2 (+ 2 (signal-value S1)))))
   (hash S1 3 S2 5)))

(let ()
  (define S1 (signal #:combine +))
  (define S2 (signal #:combine +))
  (check-equal?
   (react!
    (reaction
     (par (emit S1 1)
          (emit S2 (+ 1 (signal-value S1)))
          (emit S1 2))))
   (hash S1 3 S2 4)))

(let ()
  (define S1 (signal #:combine +))
  (check-exn
   #rx"not constructive"
   (λ ()
     (react!
      (reaction
       (emit S1 1)
       (signal-value S1)
       (emit S1 2))))))

(let ()
  (define S1 (signal #:combine +))
  (define S2 (signal #:combine +))
  (check-equal?
   (react!
    (reaction
     (par (if (signal-value S1)
              (void)
              (emit S2 1))
          (if (signal-value S2)
              (void)
              (void)))))
   (hash S2 1)))

(let ()
  (define S (signal #:combine +))
  (define r
    (reaction
     (emit S 1)
     (pause)
     (emit S 2)))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash S 2)))

(let ()
  (define S (signal #:combine +))
  (define O (signal))
  (define r
    (reaction
     #:pre 2
     (emit S 1)
     (pause)
     (when (= 1 (signal-value S #:pre 1))
       (emit O))))
  (check-equal? (react! r) (hash S 1))
  (check-equal? (react! r) (hash O #t)))

(let ()
  (define S (signal #:combine +))
  (define O (signal))
  (define r
    (reaction
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

(let ()
  (define S (signal #:combine +))
  (define r
    (reaction
     (present? S)))
  (check-equal? (react! r) (hash)))

(let ()
  (define S (signal #:combine +))
  (define r
    (reaction
     (emit S 0)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(let ()
  (define S (signal #:combine +))
  (define r
    (reaction
     (emit S 0)
     (present? S)
     (present? S)))
  (check-equal? (react! r)
                (hash S 0)))

(let ()
  (define S (signal #:combine +))
  (define r
    (reaction
     (present? S)))
  (check-equal? (react! r #:emit (list (cons S 0)))
                (hash S 0)))

(let ()
  (define S (signal #:combine +))
  (define O (signal))
  (define r
    (reaction
     (let loop ()
       (when (and (present? S) (= (signal-value S) 2))
         (emit O))
       (pause)
       (loop))))
  (check-equal? (react! r) (hash))
  (check-equal? (react! r #:emit (list (cons S 1))) (hash S 1))
  (check-equal? (react! r #:emit (list (cons S 2))) (hash S 2 O #t)))

(check-exn
 #rx"not constructive"
 (λ ()
   (let ([SO1 (signal)]
         [SO2 (signal)])
     (define r
       (reaction
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
 #rx"not constructive"
 (λ ()
   (let ([S (signal)])
     (define r
       (reaction
        (if (present? S)
            (emit S)
            (void))))
     (react! r))))

(check-exn
 #rx"not constructive"
 (λ ()
   (let ([S (signal)])
     (define r
       (reaction
        (if (present? S)
            (void)
            (emit S))))
     (react! r))))

(check-exn
 #rx"not constructive"
 (λ ()
   (let ([S (signal)])
     (define r
       (reaction
        (if (present? S)
            (emit S)
            (emit S))))
     (react! r))))

(check-exn
 #rx"not constructive"
 (λ ()
   (let ([S (signal)])
     (define r
       (reaction
        (present? S)
        (emit S)))
     (react! r))))

(let ([S1 (signal)]
      [S2 (signal)]
      [O (signal)])
  (define r
    (reaction
     (if (present? S1)
         (if (present? S2)
             (void)
             (void))
         (emit O))))
  (check-equal? (react! r)
                (hash S1 #f O #t)))

(let ([S1 (signal)]
      [S2 (signal)]
      [O (signal)])
  (define r
    (reaction
     (if (present? S1)
         (void)
         (if (present? S2)
             (void)
             (emit O)))))
  (check-equal? (react! r)
                (hash S1 #f S2 #f O #t)))

(let ([S1 (signal)]
      [S2 (signal)]
      [S3 (signal)]
      [S4 (signal)]
      [S5 (signal)])
  (define r
    (reaction
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
                (hash S1 #f S2 #f)))




;                                                                                                            
;                                                                                                            
;  ;;;;;;     ;;;;    ;;;;;;   ;;;          ;;;;;    ;;;      ;;    ;;;;  
;  ;;;;;;;   ;;;;;;   ;;;;;;;  ;;;         ;;;;;;;  ;;;;;    ;;;   ;;;;;; 
;  ;;; ;;;  ;;;  ;;;  ;;; ;;;  ;;;         ;;; ;;; ;;; ;;;  ;;;;   ;;; ;;;
;  ;;; ;;;  ;;;  ;;;  ;;; ;;;  ;;;             ;;; ;;; ;;;  ; ;;   ;;; ;;;
;  ;;;;;;;  ;;;  ;;;  ;;;;;;;  ;;;            ;;;  ;;; ;;;    ;;   ;;;;;;;
;  ;;;;;;   ;;;  ;;;  ;;;;;;   ;;;           ;;;   ;;; ;;;    ;;    ;; ;;;
;  ;;;      ;;;  ;;;  ;;;      ;;;          ;;;    ;;; ;;;    ;;       ;;;
;  ;;;       ;;;;;;   ;;;      ;;;;;;      ;;;;;;;  ;;;;;     ;;   ;;;;;; 
;  ;;;        ;;;;    ;;;      ;;;;;;      ;;;;;;;   ;;;      ;;    ;;;;  
;                                                                                                            
;                                                                                                            
;                                                                                                            


;; popl 2019 figure 2
(let ([sl (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (check-equal?
   (react!
    (reaction
     (emit sl)
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #t so1 #t)))

;; popl 2019 figure 3
(let ([sl (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (emit sl)
      (if (present? sl)
          (emit so1)
          (emit so2)))))
   (hash sl #t so1 #t)))


;; popl 2019 figure 4
(let ([sl (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (check-equal?
   (react!
    (reaction
     (if (present? sl)
         (emit so1)
         (emit so2))))
   (hash sl #f so2 #t)))

;; popl 2019 figure 5
;; this one is wrong -- this is a non-constructive program, but we don't detect that
;; and instead "live with" the assignment of both signals to absent
(let ([sl1 (signal)]
      [sl2 (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (when (present? sl1) (emit sl2))
      (when (present? sl2) (emit sl1)))))
   (hash sl1 #f sl2 #f)))


;; popl 2019 figure 6
(let ([sl (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (define r
    (reaction
     (par
      (begin (pause) (emit sl))
      (if (present? sl) (emit so1) (emit so2)))))
  (check-equal?
   (react! r)
   (hash sl #f so2 #t))
  (check-equal?
   (react! r)
   (hash sl #t)))

;; popl 2019 figure 7
(let ([sl1 (signal)]
      [sl2 (signal)]
      [sl3 (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (define r
    (reaction
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

;; popl 2019, figure 8
(check-exn
 #rx"not constructive"
 (λ ()
   (let ([s1 (signal)])
     (react!
      (reaction
       (if (present? s1)
           (void)
           (emit s1)))))))

;; popl 2019, figure 9
;; same deal as figure 5: this one is wrong -- this is a non-constructive program
;; but we don't detect it
(let ([s1 (signal)])
  (check-equal?
   (react!
    (reaction
     (if (present? s1)
         (emit s1)
         (void))))
   (hash s1 #f)))


;; popl 2019, figure 10 example 1
(check-exn
 #rx"not constructive"
 (λ ()
   (let ([s1 (signal)])
     (react!
      (reaction
       (if (present? s1)
           (emit s1)
           (emit s1)))))))

;; popl 2019, figure 10 example 1
(check-exn
 #rx"not constructive"
 (λ ()
   (let ([s1 (signal)])
     (react!
      (reaction
       (if (present? s1) (void) (void))
       (emit s1))))))

;; popl 2019, figure 11
(check-exn
 exn:fail:not-constructive?
 (λ ()
   (let ([sl1 (signal)]
         [sl2 (signal)])
     (react!
      (reaction
       (par
        (when (present? sl1) (emit sl2))
        (begin (when (present? sl2) pause)
               (emit sl1))))))))

;; popl 2019, figure 25
(let ([s-outer (signal)]
      [s-inner (signal)])
  (check-equal?
   (react!
    (reaction
     (begin
       (present? s-outer)
       (when (present? s-inner) (emit s-outer)))))
   (hash s-outer #f s-inner #f)))
