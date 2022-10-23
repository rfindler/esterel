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
     (signal-value s)))
   (hash s #f)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (emit s)
     (signal-value s)))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (begin (emit s) #f)
      (signal-value s))))
   (hash s #t)))

(let ([s (signal)])
  (check-equal?
   (react!
    (reaction
     (par
      (signal-value s)
      (signal-value s))))
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
     (signal-value s)
     (pause)
     (emit s)
     (pause)
     (signal-value s)))
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
       (signal-value s)
       (pause))
      (par
       (pause)
       (signal-value s)))))
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
      (let loop () (suspend (begin (pause) (emit s2)) (signal-value s1)) (loop))
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
       (when (signal-value i)
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
      (if (signal-value s1)
          (void)
          (car #f))
      (if (signal-value s2)
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
      (if (signal-value s2)
          (void)
          (emit s1))
      (if (signal-value s1)
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
      (if (signal-value s1)
          (void)
          (emit s2))
      (if (signal-value s2)
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
      (if (signal-value s1)
          (void)
          (car #f))
      (if (signal-value s2)
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
     (when (signal-value S #:pre 1)
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
     (if (signal-value S #:pre 1)
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
     (signal-value O #:pre 2)))
  (react! r)
  (react! r)
  (check-exn
   #rx"signal-value: #:pre argument too large.*maximum: 1"
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
        (when (signal-value S #:pre 1)
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


;                                                                                                            
;                                                                                                            
;                                                                                                            
;                                                                                                            
;   ;;;;                              ;;;;;;     ;;;;    ;;;;;;   ;;;          ;;;;;    ;;;      ;;    ;;;;  
;  ;;;                                ;;;;;;;   ;;;;;;   ;;;;;;;  ;;;         ;;;;;;;  ;;;;;    ;;;   ;;;;;; 
;  ;;;; ;;; ;; ;;;   ;;; ;; ;;;       ;;; ;;;  ;;;  ;;;  ;;; ;;;  ;;;         ;;; ;;; ;;; ;;;  ;;;;   ;;; ;;;
;  ;;;; ;;;;; ;;;;;  ;;;;;;;;;;;      ;;; ;;;  ;;;  ;;;  ;;; ;;;  ;;;             ;;; ;;; ;;;  ; ;;   ;;; ;;;
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;      ;;;;;;;  ;;;  ;;;  ;;;;;;;  ;;;            ;;;  ;;; ;;;    ;;   ;;;;;;;
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;      ;;;;;;   ;;;  ;;;  ;;;;;;   ;;;           ;;;   ;;; ;;;    ;;    ;; ;;;
;  ;;;  ;;;  ;;; ;;; ;;; ;;; ;;;      ;;;      ;;;  ;;;  ;;;      ;;;          ;;;    ;;; ;;;    ;;       ;;;
;  ;;;  ;;;   ;;;;;  ;;; ;;; ;;;      ;;;       ;;;;;;   ;;;      ;;;;;;      ;;;;;;;  ;;;;;     ;;   ;;;;;; 
;  ;;;  ;;;    ;;;   ;;; ;;; ;;;      ;;;        ;;;;    ;;;      ;;;;;;      ;;;;;;;   ;;;      ;;    ;;;;  
;                                                                                                            
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
     (if (signal-value sl)
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
      (if (signal-value sl)
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
     (if (signal-value sl)
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
      (when (signal-value sl1) (emit sl2))
      (when (signal-value sl2) (emit sl1)))))
   (hash sl1 #f sl2 #f)))


;; popl 2019 figure 6
(let ([sl (signal)]
      [so1 (signal)]
      [so2 (signal)])
  (define r
    (reaction
     (par
      (begin (pause) (emit sl))
      (if (signal-value sl) (emit so1) (emit so2)))))
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
      (if (signal-value sl1)
          (if (signal-value sl2)
              (emit so1)
              (emit sl3))
          (if (signal-value sl2)
              (emit so2)
              (emit sl3)))
      (begin
        (emit sl2)
        (when (signal-value sl3) (pause))
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
       (if (signal-value s1)
           (void)
           (emit s1)))))))

;; popl 2019, figure 9
;; same deal as figure 5: this one is wrong -- this is a non-constructive program
;; but we don't detect it
(let ([s1 (signal)])
  (check-equal?
   (react!
    (reaction
     (if (signal-value s1)
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
       (if (signal-value s1)
           (emit s1)
           (emit s1)))))))

;; popl 2019, figure 10 example 1
(check-exn
 #rx"not constructive"
 (λ ()
   (let ([s1 (signal)])
     (react!
      (reaction
       (if (signal-value s1) (void) (void))
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
        (when (signal-value sl1) (emit sl2))
        (begin (when (signal-value sl2) pause)
               (emit sl1))))))))

;; popl 2019, figure 25
(let ([s-outer (signal)]
      [s-inner (signal)])
  (check-equal?
   (react!
    (reaction
     (begin
       (signal-value s-outer)
       (when (signal-value s-inner) (emit s-outer)))))
   (hash s-outer #f s-inner #f)))
